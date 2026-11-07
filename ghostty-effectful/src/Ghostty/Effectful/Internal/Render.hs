{-# LANGUAGE DuplicateRecordFields #-}

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
import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Foreign.C.Types (CBool, CSize)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (toBool, with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Ghostty.Effectful.Cell (Cell (..), CellWidth (..), Hyperlink (..), SemanticContent (..))
import Ghostty.Effectful.Color (Palette, paletteFromList)
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), ResultCode (..))
import Ghostty.Effectful.Internal.Callbacks (captureAttach, captureDetach, captureFraming, captureHeld)
import Ghostty.Effectful.Internal.Convert (enumName, fromRgb, fromStyle, toPoint, unknownEnum)
import Ghostty.Effectful.Internal.Error (expectSuccess, resultCode)
import Ghostty.Effectful.Internal.Terminal (Terminal (..), forgetRegion)
import Ghostty.Effectful.Point (Coord (..), Point (..))
import Ghostty.Effectful.Render.Keys
import Ghostty.Effectful.Style (defaultStyle)
import Ghostty.Effectful.Terminal.Keys (CursorShape (..))
import Ghostty.FFI.Color (GhosttyColorRgb (..))
import Ghostty.FFI.GridRef (GhosttyGridRef (..), ghosttyGridRefHyperlinkUri)
import Ghostty.FFI.Render
import Ghostty.FFI.Screen
import Ghostty.FFI.Style (GhosttyStyle, ghosttyStyleDefault)
import Ghostty.FFI.Terminal (ghosttyTerminalGridRef)
import Ghostty.FFI.Types (GhosttyBuffer (..), GhosttyResult)

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
  h <- newHandle "ghostty_render_state_new" ghosttyRenderStateNew
  let undoState = ghosttyRenderStateFree h
  it <- newHandle "ghostty_render_state_row_iterator_new" ghosttyRenderStateRowIteratorNew `onException` undoState
  let undoRows = ghosttyRenderStateRowIteratorFree it >> undoState
  cs <- newHandle "ghostty_render_state_row_cells_new" ghosttyRenderStateRowCellsNew `onException` undoRows
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
  where
    newHandle :: (Storable h) => Text -> (Ptr a -> Ptr h -> IO GhosttyResult) -> IO h
    newHandle site mk = alloca $ \out -> do
      expectSuccess (CallSite site) =<< mk nullPtr out
      peek out

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
    let go acc =
          ghosttyRenderStateRowIteratorNextDirty rs.rowIterator yp >>= \case
            0 -> pure (reverse acc)
            _ -> peek yp >>= \y -> go (Row {frame = f, y} : acc)
     in go []

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
  RenderColorPalette -> allocaArray 256 $ \p -> do
    expectSuccess site =<< ghosttyRenderStateGet rs.handle k (castPtr p)
    palette =<< peekArray 256 p
  RenderCursorVisualStyle -> fetch >>= cursorShape
  RenderCursorVisible -> flag
  RenderCursorBlinking -> flag
  RenderCursorPasswordInput -> flag
  RenderCursorViewportHasValue -> flag
  RenderCursorViewportX -> whenHas RenderCursorViewportHasValue fetch
  RenderCursorViewportY -> whenHas RenderCursorViewportHasValue fetch
  RenderCursorViewportWideTail -> whenHas RenderCursorViewportHasValue flag
  RenderCursor -> do
    c <- fetchInto (cursorTemplate (sized cursorTemplate))
    shape <- cursorShape c.visualStyle
    pure
      Cursor
        { position =
            if toBool c.viewportHasValue
              then
                Just
                  CursorPosition
                    { point = Point {col = c.viewportX, row = fromIntegral c.viewportY},
                      wideTail = toBool c.wideTail
                    }
              else Nothing,
          visible = toBool c.visible,
          blinking = toBool c.blinking,
          passwordInput = toBool c.passwordInput,
          shape
        }
  RenderColors -> do
    c <- fetchInto (colorsTemplate (sized colorsTemplate))
    p <- palette c.palette
    pure
      Colors
        { background = fromRgb c.background,
          foreground = fromRgb c.foreground,
          cursor = if toBool c.cursorHasValue then Just (fromRgb c.cursor) else Nothing,
          palette = p
        }
  where
    k = frameKey key
    site = CallSite ("ghostty_render_state_get " <> enumName ghosttyRenderStateDataEnum (let GhosttyRenderStateData n = k in n))

    fetchInto :: (Storable b) => b -> IO b
    fetchInto v = with v $ \p -> do
      expectSuccess site =<< ghosttyRenderStateGet rs.handle k (castPtr p)
      peek p

    fetch :: (Storable b) => IO b
    fetch = alloca $ \p -> do
      expectSuccess site =<< ghosttyRenderStateGet rs.handle k (castPtr p)
      peek p

    flag :: IO Bool
    flag = toBool <$> (fetch :: IO CBool)

    whenHas :: FrameData Bool -> IO b -> IO (Maybe b)
    whenHas has act = getFrameData rs has >>= bool (pure Nothing) (Just <$> act)

    palette :: [GhosttyColorRgb] -> IO Palette
    palette cs = maybe (throwIO (Invariant site ResultInvalidValue)) pure (paletteFromList (map fromRgb cs))

    dirty = \case
      GhosttyRenderStateDirtyFalse -> pure NotDirty
      GhosttyRenderStateDirtyPartial -> pure PartlyDirty
      GhosttyRenderStateDirtyFull -> pure FullyDirty
      GhosttyRenderStateDirtyUnknown n -> unknownEnum site ghosttyRenderStateDirtyEnum n

    cursorShape = \case
      GhosttyRenderStateCursorVisualStyleBar -> pure BarCursor
      GhosttyRenderStateCursorVisualStyleBlock -> pure BlockCursor
      GhosttyRenderStateCursorVisualStyleUnderline -> pure UnderlineCursor
      GhosttyRenderStateCursorVisualStyleBlockHollow -> pure HollowBlockCursor
      GhosttyRenderStateCursorVisualStyleUnknown n -> unknownEnum site ghosttyRenderStateCursorVisualStyleEnum n

-- | A sized struct's own size, for @GHOSTTY_INIT_SIZED@: the template at
-- the size of the struct it makes ('sizeOf' ignores its argument).
sized :: (Storable b) => (CSize -> b) -> CSize
sized mk = fromIntegral (sizeOf (mk 0))

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
      visualStyle = GhosttyRenderStateCursorVisualStyleBar
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

-- | An empty grid reference of the given size.
refTemplate :: CSize -> GhosttyGridRef
refTemplate n = GhosttyGridRef {size = n, node = nullPtr, x = 0, y = 0}

-- | @ghostty_render_state_row_get@ at the key's output type, on the row.
-- Any failure is 'Invariant'.
getRowData :: Row s f -> RowData a -> IO a
getRowData row key = do
  seek row
  case key of
    RowDirty -> toBool <$> (fetch :: IO CBool)
    RowViewportY -> fetch
    RowId -> do
      GhosttyRenderStateRowId bits <- fetch
      case bits of
        [a, b] -> pure (RowIdentity a b)
        _ -> throwIO (Invariant site ResultInvalidValue)
    RowRaw -> do
      raw :: GhosttyRow <- fetch
      let rowFlag d = toBool <$> (rowValue raw d :: IO CBool)
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
    site = CallSite ("ghostty_render_state_row_get " <> enumName ghosttyRenderStateRowDataEnum (let GhosttyRenderStateRowData n = k in n))
    rowSite = CallSite "ghostty_row_get"

    fetch :: (Storable b) => IO b
    fetch = alloca $ \p -> do
      expectSuccess site =<< ghosttyRenderStateRowGet rs.rowIterator k (castPtr p)
      peek p

    rowValue :: (Storable b) => GhosttyRow -> GhosttyRowData -> IO b
    rowValue raw d = alloca $ \p -> do
      expectSuccess rowSite =<< ghosttyRowGet raw d (castPtr p)
      peek p

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
      go x acc =
        ghosttyRenderStateRowCellsNext rs.rowCells >>= \case
          0 -> pure (V.fromList (reverse acc))
          _ -> copyCell rs live row.y x >>= \c -> go (x + 1) (c : acc)
  go 0 []

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
  protected <- toBool <$> (cellValue raw GhosttyCellDataProtected :: IO CBool)
  linked <- toBool <$> (cellValue raw GhosttyCellDataHasHyperlink :: IO CBool)
  styled <- toBool <$> (fetch GhosttyRenderStateRowCellsDataHasStyling :: IO CBool)
  style <-
    if not styled
      then pure defaultStyle
      else alloca $ \(p :: Ptr GhosttyStyle) -> do
        ghosttyStyleDefault p
        expectSuccess (site GhosttyRenderStateRowCellsDataStyle)
          =<< ghosttyRenderStateRowCellsGet rs.rowCells GhosttyRenderStateRowCellsDataStyle (castPtr p)
        fromStyle (site GhosttyRenderStateRowCellsDataStyle) =<< peek p
  text <- decodeUtf8 <$> graphemes
  foreground <- color GhosttyRenderStateRowCellsDataFgColor
  background <- color GhosttyRenderStateRowCellsDataBgColor
  hyperlink <-
    if not linked
      then pure Nothing
      else Just . Hyperlink <$> (if live then Just . decodeUtf8 <$> linkUri rs x y else pure Nothing)
  pure Cell {text, width, style, foreground, background, hyperlink, protected, semantic}
  where
    site d = CallSite ("ghostty_render_state_row_cells_get " <> enumName ghosttyRenderStateRowCellsDataEnum (let GhosttyRenderStateRowCellsData n = d in n))
    cellSite = CallSite "ghostty_cell_get"

    fetch :: (Storable b) => GhosttyRenderStateRowCellsData -> IO b
    fetch d = alloca $ \p -> do
      expectSuccess (site d) =<< ghosttyRenderStateRowCellsGet rs.rowCells d (castPtr p)
      peek p

    cellValue :: (Storable b) => GhosttyCell -> GhosttyCellData -> IO b
    cellValue raw d = alloca $ \p -> do
      expectSuccess cellSite =<< ghosttyCellGet raw d (castPtr p)
      peek p

    -- The header's INVALID_VALUE for a colour is "none: use the default".
    color d = alloca $ \p -> do
      r <- ghosttyRenderStateRowCellsGet rs.rowCells d (castPtr p)
      case resultCode r of
        ResultSuccess -> Just . fromRgb <$> peek p
        ResultInvalidValue -> pure Nothing
        other -> throwIO (Invariant (site d) other)

    -- GRAPHEMES_UTF8: into a small buffer, or one of the size the library
    -- asks for.
    graphemes :: IO ByteString
    graphemes = do
      let d = GhosttyRenderStateRowCellsDataGraphemesUtf8
          into cap buf = with GhosttyBuffer {ptr = buf, cap = fromIntegral cap, len = 0} $ \bp -> do
            r <- ghosttyRenderStateRowCellsGet rs.rowCells d (castPtr bp)
            b <- peek bp
            case resultCode r of
              ResultSuccess -> Right <$> bytesAt buf (fromIntegral b.len)
              ResultOutOfSpace -> pure (Left (fromIntegral b.len))
              other -> throwIO (Invariant (site d) other)
      retrySized (site d) into

-- | The URI of the link at a viewport cell of the terminal
-- (@ghostty_terminal_grid_ref@, then @ghostty_grid_ref_hyperlink_uri@).
linkUri :: RenderState s -> Word16 -> Word16 -> IO ByteString
linkUri rs x y = do
  let point = toPoint (Point {col = x, row = fromIntegral y} :: Point 'Viewport)
      refSite = CallSite "ghostty_terminal_grid_ref"
      uriSite = CallSite "ghostty_grid_ref_hyperlink_uri"
  with point $ \pp -> with (refTemplate (sized refTemplate)) $ \rp -> do
    expectSuccess refSite =<< ghosttyTerminalGridRef rs.terminal.handle pp rp
    alloca $ \np -> do
      let into cap buf = do
            r <- ghosttyGridRefHyperlinkUri rp buf (fromIntegral cap) np
            n <- fromIntegral <$> peek np
            case resultCode r of
              ResultSuccess -> Right <$> bytesAt buf n
              ResultOutOfSpace -> pure (Left n)
              other -> throwIO (Invariant uriSite other)
      retrySized uriSite into

-- | Read into a small buffer, or, if the read asks for more ('Left' with the
-- size it needs), once more into a buffer of that size.
retrySized :: CallSite -> (Int -> Ptr Word8 -> IO (Either Int ByteString)) -> IO ByteString
retrySized site into =
  allocaBytes smallBuffer (into smallBuffer) >>= \case
    Right bs -> pure bs
    Left need ->
      allocaBytes need (into need) >>= \case
        Right bs -> pure bs
        Left _ -> throwIO (Invariant site ResultOutOfSpace)

-- | The first try's buffer size, enough for nearly every grapheme.
smallBuffer :: Int
smallBuffer = 64

-- | Copy bytes out of a buffer.
bytesAt :: Ptr Word8 -> Int -> IO ByteString
bytesAt p n = BS.packCStringLen (castPtr p, n)
