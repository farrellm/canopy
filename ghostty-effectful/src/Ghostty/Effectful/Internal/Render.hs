{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Render states and frames: representations and calls (DESIGN.md §4.3).
-- The public API is "Ghostty.Effectful.Render".
--
-- A render state keeps one row iterator and one cell iterator, reused for
-- every row of every frame. Iterators only move forward, so the state
-- records how far its row iterator has moved since it was last filled, and
-- 'seek' fills it again only to go back. Everything read through them is
-- borrowed from the render state until its next update, which is why a
-- 'Frame' and its 'Row's carry the frame tag, and why a 'Cell' is a copy.
--
-- A render state is attached to its terminal's frame capture
-- ("Ghostty.Effectful.Internal.Callbacks"): when a program begins a
-- synchronized-output hold, the C callback updates it to the frame the
-- program wants left on screen, and until the hold ends a frame does not
-- update it. Turning the mode off ends the hold too
-- ("Ghostty.Effectful.Internal.Keys" drops the capture). While one of its
-- frames is open, the capture leaves it alone, since an update would
-- invalidate the rows being read.
module Ghostty.Effectful.Internal.Render
  ( -- * Representations
    RenderState (..),
    Frame (..),
    FrameInfo (..),
    Row (..),

    -- * Render states
    openRenderState,
    closeRenderState,

    -- * Frames
    beginFrame,
    endFrame,
    abandonFrame,
    dirtyRowsOf,
    allRowsOf,

    -- * Keys
    frameKey,
    rowKey,
    getFrameData,
    getRowData,

    -- * Cells
    copyCells,
    cellKeys,
  )
where

import Control.Exception (onException, throwIO)
import Data.Vector qualified as V
import Foreign.C.Types (CBool, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool, with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import Ghostty.Effectful.Cell (Cell (..), CellWidth (..), Hyperlink (..), SemanticContent (..))
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), ResultCode (..))
import Ghostty.Effectful.Internal.Callbacks (captureAttach, captureDetach, captureFraming, captureHeld)
import Ghostty.Effectful.Internal.Convert (emptyRef, enumSite, fromCursorStyle, fromPalette, fromRgb, fromStyle, readPalette, toPoint, unfoldWhileM, unknownEnum, withSize)
import Ghostty.Effectful.Internal.Error (bytesVia, expectSuccess, newHandle, optionalVia, outVia)
import Ghostty.Effectful.Internal.Terminal (Terminal (..), forgetRegion)
import Ghostty.Effectful.Point (Coord (..), Point (..))
import Ghostty.Effectful.Render.Keys
import Ghostty.Effectful.Style (defaultStyle)
import Ghostty.FFI.Color (GhosttyColorRgb (..))
import Ghostty.FFI.GridRef (ghosttyGridRefHyperlinkUri)
import Ghostty.FFI.Render
import Ghostty.FFI.Screen
import Ghostty.FFI.Style (GhosttyStyle, ghosttyStyleDefault)
import Ghostty.FFI.Terminal (ghosttyTerminalGridRef)
import Ghostty.FFI.Types (GhosttyBuffer (..), GhosttyResult, pattern GhosttyCursorStyleBar)

-- | A render state of one terminal, alive in the region @s@ that
-- 'Ghostty.Effectful.Render.withRenderState' opens.
data RenderState s = RenderState
  { -- | the library's handle
    handle :: GhosttyRenderState,
    -- | the terminal it is updated from, whose callbacks capture held
    -- frames into it and whose count of changes dates its frames. Its
    -- region is forgotten: the render state's own is inside it.
    terminal :: Terminal (),
    -- | the row iterator every frame reuses
    rowIterator :: GhosttyRenderStateRowIterator,
    -- | the cell iterator every row reuses
    rowCells :: GhosttyRenderStateRowCells,
    -- | how many rows the row iterator has moved past since it was filled;
    -- 'Nothing' if it has not been filled since the last update
    advanced :: IORef (Maybe Int),
    -- | the frame open now, if any
    open :: IORef (Maybe FrameInfo)
  }

-- | What a frame knows of the moment it describes.
data FrameInfo = FrameInfo
  { -- | whether it is a frame captured when a hold began
    held :: Bool,
    -- | the terminal's count of changes when the frame was updated
    writes :: Word64
  }

-- | A frame of a render state: its data as of one update, valid in the
-- region @f@ that 'Ghostty.Effectful.Render.withFrame' opens.
data Frame s f = Frame
  { -- | the render state
    state :: RenderState s,
    -- | whether it is a frame captured when a hold began, and the moment it
    -- describes
    info :: FrameInfo
  }

-- | A row of a frame.
data Row s f = Row
  { -- | the frame
    frame :: Frame s f,
    -- | its position among the frame's rows, from the top of the viewport
    y :: Word16
  }

-- * Render states

-- | A render state for the terminal, with its iterators, attached to the
-- terminal's frame capture. What was made is freed if a later step fails.
openRenderState :: Terminal t -> IO (RenderState s)
openRenderState t = do
  h <- newHandle (CallSite "ghostty_render_state_new") ghosttyRenderStateNew
  let undoState = ghosttyRenderStateFree h
  it <- newHandle (CallSite "ghostty_render_state_row_iterator_new") ghosttyRenderStateRowIteratorNew `onException` undoState
  let undoRows = ghosttyRenderStateRowIteratorFree it >> undoState
  cs <- newHandle (CallSite "ghostty_render_state_row_cells_new") ghosttyRenderStateRowCellsNew `onException` undoRows
  let undoCells = ghosttyRenderStateRowCellsFree cs >> undoRows
  captureAttach t.installed h `onException` undoCells
  advanced <- newIORef Nothing
  open <- newIORef Nothing
  pure
    RenderState
      { handle = h,
        terminal = forgetRegion t,
        rowIterator = it,
        rowCells = cs,
        advanced,
        open
      }

-- | Detach the render state from the capture, then free its iterators and
-- itself.
closeRenderState :: RenderState s -> IO ()
closeRenderState rs = do
  captureDetach rs.terminal.installed rs.handle
  ghosttyRenderStateRowCellsFree rs.rowCells
  ghosttyRenderStateRowIteratorFree rs.rowIterator
  ghosttyRenderStateFree rs.handle

-- * Frames

-- | Open a frame: update the render state from the terminal, unless it holds
-- a captured frame whose hold is still on. A frame opened inside another of
-- the same render state is that frame: it neither updates nor, at its end,
-- cleans. Returns the frame's moment, and whether it is the outermost.
beginFrame :: RenderState s -> IO (FrameInfo, Bool)
beginFrame rs =
  readIORef rs.open >>= \case
    Just info -> pure (info, False)
    Nothing -> do
      held <- captureHeld rs.terminal.installed rs.handle
      unless held $
        expectSuccess (CallSite "ghostty_render_state_update") =<< ghosttyRenderStateUpdate rs.handle rs.terminal.handle
      writeIORef rs.advanced Nothing
      w <- readIORef rs.terminal.writes
      let info = FrameInfo {held, writes = w}
      writeIORef rs.open (Just info)
      captureFraming rs.terminal.installed rs.handle True
      pure (info, True)

-- | Close the outermost frame, which ran to its end: its changes are
-- consumed (@ghostty_render_state_clean@), so the next frame reports only
-- what changed after it.
endFrame :: RenderState s -> IO ()
endFrame rs = do
  abandonFrame rs
  expectSuccess (CallSite "ghostty_render_state_clean") =<< ghosttyRenderStateClean rs.handle

-- | Close the outermost frame without consuming its changes, because it did
-- not run to its end: the next frame reports them again.
abandonFrame :: RenderState s -> IO ()
abandonFrame rs = do
  writeIORef rs.open Nothing
  captureFraming rs.terminal.installed rs.handle False

-- | Fill the row iterator from the render state.
fill :: RenderState s -> IO ()
fill rs = do
  with rs.rowIterator $ \p ->
    expectSuccess (CallSite "ghostty_render_state_get GHOSTTY_RENDER_STATE_DATA_ROW_ITERATOR")
      =<< ghosttyRenderStateGet rs.handle GhosttyRenderStateDataRowIterator (castPtr p)
  writeIORef rs.advanced (Just 0)

-- | The rows that need drawing (@ghostty_render_state_row_iterator_next_dirty@):
-- none if the frame is not dirty, every row if it is fully dirty, the dirty
-- rows otherwise.
dirtyRowsOf :: Frame s f -> IO [Row s f]
dirtyRowsOf f = do
  let rs = f.state
  fill rs
  -- The iterator is used up; the next seek fills it again.
  writeIORef rs.advanced Nothing
  alloca $ \yp ->
    unfoldWhileM
      (toBool <$> ghosttyRenderStateRowIteratorNextDirty rs.rowIterator yp)
      ((\y -> Row {frame = f, y}) <$> peek yp)

-- | Every row of the frame, top to bottom.
allRowsOf :: Frame s f -> IO [Row s f]
allRowsOf f = do
  n <- getFrameData f.state RenderRows
  -- Counted in Int: @n - 1@ in 'Word16' wraps when there are no rows.
  pure [Row {frame = f, y = fromIntegral y} | y <- [0 .. fromIntegral n - 1 :: Int]]

-- | Put the row iterator on the row, moving forward, or filling it again
-- first to go back.
seek :: Row s f -> IO ()
seek row = do
  let rs = row.frame.state
      target = fromIntegral row.y + 1
  from <-
    readIORef rs.advanced >>= \case
      Just n | n <= target -> pure n
      _ -> fill rs >> pure 0
  -- Unknown until the moves succeed: a failed one leaves the iterator
  -- somewhere else, and the next seek fills it again.
  writeIORef rs.advanced Nothing
  replicateM_ (target - from) $ do
    moved <- ghosttyRenderStateRowIteratorNext rs.rowIterator
    when (moved == 0) $
      throwIO (Invariant (CallSite "ghostty_render_state_row_iterator_next") ResultNoValue)
  writeIORef rs.advanced (Just target)

-- * Keys

-- | The header's key for a frame read.
frameKey :: FrameData a -> GhosttyRenderStateData
frameKey = \case
  RenderCols -> GhosttyRenderStateDataCols
  RenderRows -> GhosttyRenderStateDataRows
  RenderDirty -> GhosttyRenderStateDataDirty
  RenderColorBackground -> GhosttyRenderStateDataColorBackground
  RenderColorForeground -> GhosttyRenderStateDataColorForeground
  RenderColorCursor -> GhosttyRenderStateDataColorCursor
  RenderColorCursorHasValue -> GhosttyRenderStateDataColorCursorHasValue
  RenderColorPalette -> GhosttyRenderStateDataColorPalette
  RenderCursorVisualStyle -> GhosttyRenderStateDataCursorVisualStyle
  RenderCursorVisible -> GhosttyRenderStateDataCursorVisible
  RenderCursorBlinking -> GhosttyRenderStateDataCursorBlinking
  RenderCursorPasswordInput -> GhosttyRenderStateDataCursorPasswordInput
  RenderCursorViewportHasValue -> GhosttyRenderStateDataCursorViewportHasValue
  RenderCursorViewportX -> GhosttyRenderStateDataCursorViewportX
  RenderCursorViewportY -> GhosttyRenderStateDataCursorViewportY
  RenderCursorViewportWideTail -> GhosttyRenderStateDataCursorViewportWideTail
  RenderCursor -> GhosttyRenderStateDataCursor
  RenderColors -> GhosttyRenderStateDataColors

-- | The header's key for a row read.
rowKey :: RowData a -> GhosttyRenderStateRowData
rowKey = \case
  RowDirty -> GhosttyRenderStateRowDataDirty
  RowRaw -> GhosttyRenderStateRowDataRaw
  RowViewportY -> GhosttyRenderStateRowDataViewportY
  RowId -> GhosttyRenderStateRowDataId

-- | @ghostty_render_state_get@ at the key's output type, copied out. A
-- cursor value the header calls undefined while the cursor is outside the
-- viewport, and a cursor colour while none is set, are 'Nothing'; any
-- failure is 'Invariant', since a constructor is a key the header names.
getFrameData :: RenderState s -> FrameData a -> IO a
getFrameData rs key = case key of
  RenderCols -> fetch
  RenderRows -> fetch
  RenderDirty -> fetch >>= dirty
  RenderColorBackground -> fromRgb <$> fetch
  RenderColorForeground -> fromRgb <$> fetch
  RenderColorCursor -> whenHas RenderColorCursorHasValue (fromRgb <$> fetch)
  RenderColorCursorHasValue -> flag
  RenderColorPalette -> readPalette site (ghosttyRenderStateGet rs.handle k . castPtr)
  RenderCursorVisualStyle -> fetch >>= fromCursorStyle site
  RenderCursorVisible -> flag
  RenderCursorBlinking -> flag
  RenderCursorPasswordInput -> flag
  RenderCursorViewportHasValue -> flag
  RenderCursorViewportX -> whenHas RenderCursorViewportHasValue fetch
  RenderCursorViewportY -> whenHas RenderCursorViewportHasValue fetch
  RenderCursorViewportWideTail -> whenHas RenderCursorViewportHasValue flag
  RenderCursor -> do
    c <- fetchInto (withSize cursorTemplate)
    shape <- fromCursorStyle site c.visualStyle
    pure
      Cursor
        { position =
            CursorPosition
              { point = Point {col = c.viewportX, row = fromIntegral c.viewportY},
                wideTail = toBool c.wideTail
              }
              <$ guard (toBool c.viewportHasValue),
          visible = toBool c.visible,
          blinking = toBool c.blinking,
          passwordInput = toBool c.passwordInput,
          shape
        }
  RenderColors -> do
    c <- fetchInto (withSize colorsTemplate)
    p <- fromPalette site c.palette
    pure
      Colors
        { background = fromRgb c.background,
          foreground = fromRgb c.foreground,
          cursor = fromRgb c.cursor <$ guard (toBool c.cursorHasValue),
          palette = p
        }
  where
    k = frameKey key
    site = enumSite "ghostty_render_state_get" ghosttyRenderStateDataEnum (coerce k)

    get :: Ptr b -> IO GhosttyResult
    get = ghosttyRenderStateGet rs.handle k . castPtr

    -- A sized struct, its size written first.
    fetchInto :: (Storable b) => b -> IO b
    fetchInto v = with v $ \p -> do
      expectSuccess site =<< get p
      peek p

    fetch :: (Storable b) => IO b
    fetch = outVia site get

    flag :: IO Bool
    flag = toBool @CBool <$> fetch

    whenHas :: FrameData Bool -> IO b -> IO (Maybe b)
    whenHas has act = ifM (getFrameData rs has) (Just <$> act) (pure Nothing)

    dirty = \case
      GhosttyRenderStateDirtyFalse -> pure NotDirty
      GhosttyRenderStateDirtyPartial -> pure PartlyDirty
      GhosttyRenderStateDirtyFull -> pure FullyDirty
      GhosttyRenderStateDirtyUnknown n -> unknownEnum site ghosttyRenderStateDirtyEnum n

-- | An empty cursor of the given size.
cursorTemplate :: CSize -> GhosttyRenderStateCursor
cursorTemplate n =
  GhosttyRenderStateCursor
    { size = n,
      viewportHasValue = 0,
      viewportX = 0,
      viewportY = 0,
      wideTail = 0,
      visible = 0,
      blinking = 0,
      passwordInput = 0,
      visualStyle = GhosttyCursorStyleBar
    }

-- | Empty colours of the given size.
colorsTemplate :: CSize -> GhosttyRenderStateColors
colorsTemplate n =
  GhosttyRenderStateColors
    { size = n,
      background = black,
      foreground = black,
      cursor = black,
      cursorHasValue = 0,
      palette = []
    }
  where
    black = GhosttyColorRgb {r = 0, g = 0, b = 0}

-- | @ghostty_render_state_row_get@ at the key's output type, on the row.
-- Any failure is 'Invariant'.
getRowData :: Row s f -> RowData a -> IO a
getRowData row key = do
  seek row
  case key of
    RowDirty -> toBool @CBool <$> fetch
    RowViewportY -> fetch
    RowId -> do
      GhosttyRenderStateRowId bits <- fetch
      case bits of
        [a, b] -> pure (RowIdentity a b)
        _ -> throwIO (Invariant site ResultInvalidValue)
    RowRaw -> do
      raw :: GhosttyRow <- fetch
      let rowFlag d = toBool @CBool <$> rowValue raw d
      wrap <- rowFlag GhosttyRowDataWrap
      wrapContinuation <- rowFlag GhosttyRowDataWrapContinuation
      grapheme <- rowFlag GhosttyRowDataGrapheme
      styled <- rowFlag GhosttyRowDataStyled
      hasHyperlink <- rowFlag GhosttyRowDataHyperlink
      prompt <-
        rowValue raw GhosttyRowDataSemanticPrompt >>= \case
          GhosttyRowSemanticNone -> pure NoPrompt
          GhosttyRowSemanticPromptTag -> pure PromptLine
          GhosttyRowSemanticPromptContinuation -> pure PromptContinuation
          GhosttyRowSemanticPromptUnknown n -> unknownEnum rowSite ghosttyRowSemanticPromptEnum n
      kittyVirtualPlaceholder <- rowFlag GhosttyRowDataKittyVirtualPlaceholder
      dirty <- rowFlag GhosttyRowDataDirty
      pure RowFlags {wrap, wrapContinuation, grapheme, styled, hasHyperlink, prompt, kittyVirtualPlaceholder, dirty}
  where
    rs = row.frame.state
    k = rowKey key
    site = enumSite "ghostty_render_state_row_get" ghosttyRenderStateRowDataEnum (coerce k)
    rowSite = CallSite "ghostty_row_get"

    fetch :: (Storable b) => IO b
    fetch = outVia site (ghosttyRenderStateRowGet rs.rowIterator k . castPtr)

    rowValue :: (Storable b) => GhosttyRow -> GhosttyRowData -> IO b
    rowValue raw d = outVia rowSite (ghosttyRowGet raw d . castPtr)

-- * Cells

-- | The cell keys a copy reads. The rest of the header's: @GRAPHEMES_LEN@
-- and @GRAPHEMES_BUF@, the text as codepoints, which @GRAPHEMES_UTF8@ reads
-- in one call; and @SELECTED@, with the selection API (PLAN.md M6.5).
cellKeys :: [GhosttyRenderStateRowCellsData]
cellKeys =
  [ GhosttyRenderStateRowCellsDataRaw,
    GhosttyRenderStateRowCellsDataHasStyling,
    GhosttyRenderStateRowCellsDataStyle,
    GhosttyRenderStateRowCellsDataGraphemesUtf8,
    GhosttyRenderStateRowCellsDataFgColor,
    GhosttyRenderStateRowCellsDataBgColor
  ]

-- | Copy the row's cells out, left to right. A cell's hyperlink URI is read
-- from the terminal at the cell's viewport position, which is only right
-- while the terminal is the frame's moment: not in a held frame, and not
-- after the terminal changed.
copyCells :: Row s f -> IO (V.Vector Cell)
copyCells row = do
  seek row
  let rs = row.frame.state
  with rs.rowCells $ \p ->
    expectSuccess (CallSite "ghostty_render_state_row_get GHOSTTY_RENDER_STATE_ROW_DATA_CELLS")
      =<< ghosttyRenderStateRowGet rs.rowIterator GhosttyRenderStateRowDataCells (castPtr p)
  now <- readIORef rs.terminal.writes
  let live = not row.frame.info.held && now == row.frame.info.writes
  flip V.unfoldrM 0 $ \x ->
    ifM
      (toBool <$> ghosttyRenderStateRowCellsNext rs.rowCells)
      (Just . (,x + 1) <$> copyCell rs live row.y x)
      (pure Nothing)

-- | Copy the cell the cell iterator is on, at column @x@ of row @y@.
copyCell :: RenderState s -> Bool -> Word16 -> Word16 -> IO Cell
copyCell rs live y x = do
  raw :: GhosttyCell <- fetch GhosttyRenderStateRowCellsDataRaw
  width <-
    cellValue raw GhosttyCellDataWide >>= \case
      GhosttyCellWideNarrow -> pure Narrow
      GhosttyCellWideWide -> pure Wide
      GhosttyCellWideSpacerTail -> pure SpacerTail
      GhosttyCellWideSpacerHead -> pure SpacerHead
      GhosttyCellWideUnknown n -> unknownEnum cellSite ghosttyCellWideEnum n
  semantic <-
    cellValue raw GhosttyCellDataSemanticContent >>= \case
      GhosttyCellSemanticOutput -> pure ContentOutput
      GhosttyCellSemanticInput -> pure ContentInput
      GhosttyCellSemanticPrompt -> pure ContentPrompt
      GhosttyCellSemanticContentUnknown n -> unknownEnum cellSite ghosttyCellSemanticContentEnum n
  protected <- toBool @CBool <$> cellValue raw GhosttyCellDataProtected
  linked <- toBool @CBool <$> cellValue raw GhosttyCellDataHasHyperlink
  styled <- toBool @CBool <$> fetch GhosttyRenderStateRowCellsDataHasStyling
  style <-
    if styled
      then fromStyle (site GhosttyRenderStateRowCellsDataStyle) =<< styleOf
      else pure defaultStyle
  text <- decodeUtf8 <$> graphemes
  foreground <- color GhosttyRenderStateRowCellsDataFgColor
  background <- color GhosttyRenderStateRowCellsDataBgColor
  hyperlink <-
    if linked
      then Just . Hyperlink <$> if live then Just . decodeUtf8 <$> linkUri rs x y else pure Nothing
      else pure Nothing
  pure Cell {text, width, style, foreground, background, hyperlink, protected, semantic}
  where
    site = enumSite "ghostty_render_state_row_cells_get" ghosttyRenderStateRowCellsDataEnum . coerce
    cellSite = CallSite "ghostty_cell_get"

    get :: GhosttyRenderStateRowCellsData -> Ptr b -> IO GhosttyResult
    get d = ghosttyRenderStateRowCellsGet rs.rowCells d . castPtr

    fetch :: (Storable b) => GhosttyRenderStateRowCellsData -> IO b
    fetch d = outVia (site d) (get d)

    cellValue :: (Storable b) => GhosttyCell -> GhosttyCellData -> IO b
    cellValue raw d = outVia cellSite (ghosttyCellGet raw d . castPtr)

    -- A sized struct, defaulted first.
    styleOf :: IO GhosttyStyle
    styleOf = outVia (site d) (\p -> ghosttyStyleDefault p >> get d p)
      where
        d = GhosttyRenderStateRowCellsDataStyle

    -- The header's NO_VALUE for a colour is "none of its own: use the
    -- default" (INVALID_VALUE before libghostty-vt 1.0; regression 9).
    color d = fmap fromRgb <$> optionalVia ResultNoValue (site d) (get d)

    -- GRAPHEMES_UTF8, through a buffer struct that reports the length.
    graphemes :: IO ByteString
    graphemes = bytesVia (site d) $ \buf cap ->
      with GhosttyBuffer {ptr = buf, cap = fromIntegral cap, len = 0} $ \bp -> do
        r <- get d bp
        (r,) . fromIntegral . (.len) <$> peek bp
      where
        d = GhosttyRenderStateRowCellsDataGraphemesUtf8

-- | The URI of the link at a viewport cell of the terminal
-- (@ghostty_terminal_grid_ref@, then @ghostty_grid_ref_hyperlink_uri@).
linkUri :: RenderState s -> Word16 -> Word16 -> IO ByteString
linkUri rs x y = do
  let point = toPoint (Point {col = x, row = fromIntegral y} :: Point 'Viewport)
      refSite = CallSite "ghostty_terminal_grid_ref"
      uriSite = CallSite "ghostty_grid_ref_hyperlink_uri"
  with point $ \pp -> with emptyRef $ \rp -> do
    expectSuccess refSite =<< ghosttyTerminalGridRef rs.terminal.handle pp rp
    alloca $ \np -> bytesVia uriSite $ \buf cap -> do
      r <- ghosttyGridRefHyperlinkUri rp buf (fromIntegral cap) np
      (r,) . fromIntegral <$> peek np
