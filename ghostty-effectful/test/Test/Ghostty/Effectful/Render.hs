{-# LANGUAGE PatternSynonyms #-}

-- | Render states and frames (PLAN.md M1.7, DESIGN.md §4.3): scoped render
-- states; frames that report only the rows that changed; cells copied out
-- that survive the next update; held frames under synchronized output; and
-- the frame and row keys, each read and held against the header.
module Test.Ghostty.Effectful.Render (tests) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.Char (isUpper, toUpper)
import Data.List (stripPrefix)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Effectful (Eff, IOE)
import Effectful.Exception qualified as E
import Foreign.Marshal.Utils (with)
import Ghostty.Effectful (Ghostty)
import Ghostty.Effectful.Cell
import Ghostty.Effectful.Color
import Ghostty.Effectful.Event (TerminalEvent (..))
import Ghostty.Effectful.Internal.Render (cellKeys, frameKey, rowKey)
import Ghostty.Effectful.Internal.Terminal (Terminal (..))
import Ghostty.Effectful.Mode (ModeKey (..))
import Ghostty.Effectful.Point (Point (..))
import Ghostty.Effectful.Render
import Ghostty.Effectful.Style (CellColor (..), Style (..), Underline (..), defaultStyle)
import Ghostty.Effectful.Terminal
import Ghostty.FFI.Layout (CEnum (..))
import Ghostty.FFI.Render
  ( GhosttyRenderStateData (..),
    GhosttyRenderStateOption (..),
    GhosttyRenderStateRowCellsData (..),
    GhosttyRenderStateRowData (..),
    GhosttyRenderStateRowOption (..),
    ghosttyRenderStateDataEnum,
    ghosttyRenderStateOptionEnum,
    ghosttyRenderStateRowCellsDataEnum,
    ghosttyRenderStateRowDataEnum,
    ghosttyRenderStateRowOptionEnum,
    pattern GhosttyRenderStateDataImageIterator,
    pattern GhosttyRenderStateDataImagePlacementsGeneration,
    pattern GhosttyRenderStateDataOverscan,
    pattern GhosttyRenderStateDataOverscanRequest,
    pattern GhosttyRenderStateDataRowIterator,
    pattern GhosttyRenderStateOptDirty,
    pattern GhosttyRenderStateOptOverscan,
    pattern GhosttyRenderStateRowCellsDataGraphemesBuf,
    pattern GhosttyRenderStateRowCellsDataGraphemesLen,
    pattern GhosttyRenderStateRowCellsDataSelected,
    pattern GhosttyRenderStateRowDataCells,
    pattern GhosttyRenderStateRowDataCellsRaw,
    pattern GhosttyRenderStateRowDataSelection,
    pattern GhosttyRenderStateRowOptDirty,
  )
import Ghostty.FFI.Terminal (GhosttyTerminalScrollViewport (..), ghosttyTerminalScrollViewport)
import Ghostty.FFI.Types (pattern GhosttySuccess)
import Test.Ghostty.Effectful.Support (config, ghostty, grid)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- | Some key, its type forgotten.
data Some f where
  Some :: f a -> Some f

tests :: TestTree
tests =
  testGroup
    "Render"
    [ testGroup "render states" renderStates,
      testGroup "frames" frames,
      testGroup "cells" cellTests,
      testGroup "synchronized output" holds,
      testGroup "keys" keys
    ]

-- * Render states

renderStates :: [TestTree]
renderStates =
  [ testCase "a render state is open inside its scope and freed after" $ do
      r <- ghostty $ withTerminal config $ \t -> do
        inside <- withRenderState t (const openRenderStates)
        after <- openRenderStates
        pure (inside, after)
      r @?= (1, 0),
    testCase "a render state is freed when its scope throws" $ do
      r <- ghostty $ withTerminal config $ \t -> do
        e <- E.try @ErrorCall (withRenderState t $ \_ -> liftIO (throwIO (ErrorCall "boom")))
        n <- openRenderStates
        pure (either (\(ErrorCall m) -> m) (const "no exception") e, n)
      r @?= ("boom", 0),
    testCase "two render states of one terminal each see it" $ do
      r <- ghostty $ withTerminal config $ \t -> do
        _ <- feed t "two"
        withRenderState t $ \a -> withRenderState t $ \b -> do
          ta <- withFrame a texts
          tb <- withFrame b texts
          pure (take 1 ta, take 1 tb)
      r @?= (["two"], ["two"])
  ]

-- * Frames

frames :: [TestTree]
frames =
  [ testCase "a frame's rows are the viewport's, top to bottom" $ do
      r <- rendered "ab\r\ncd" texts
      r @?= ["ab", "cd"] <> replicate 22 "",
    -- PLAN.md M1.7: only dirty rows reported after a partial update. The
    -- cursor stays on row 1, where it was at the end of the first frame,
    -- so no other row is touched.
    testCase "after a frame, only the rows changed since are dirty" $ do
      r <- ghostty $ withTerminal config $ \t -> withRenderState t $ \rs -> do
        _ <- feed t "a\r\nb\r\nc\ESC[2;2H"
        frame1 <- withFrame rs $ \f -> (,) <$> getF f RenderDirty <*> (length <$> dirtyRows f)
        _ <- feed t "x"
        frame2 <- withFrame rs $ \f -> (,) <$> getF f RenderDirty <*> (map rowY <$> dirtyRows f)
        frame3 <- withFrame rs $ \f -> (,) <$> getF f RenderDirty <*> (map rowY <$> dirtyRows f)
        pure (frame1, frame2, frame3)
      r @?= ((FullyDirty, 24), (PartlyDirty, [1]), (NotDirty, [])),
    -- The cursor leaves row 0 for row 1, so both rows changed.
    testCase "a frame that throws leaves its changes for the next" $ do
      r <- ghostty $ withTerminal config $ \t -> withRenderState t $ \rs -> do
        _ <- feed t "a"
        _ <- withFrame rs (const pass)
        _ <- feed t "\r\nb"
        _ <- E.try @ErrorCall (withFrame rs $ \_ -> liftIO (throwIO (ErrorCall "boom")))
        withFrame rs (fmap (map rowY) . dirtyRows)
      r @?= [0, 1],
    testCase "a frame inside another of the same render state is that frame" $ do
      r <- ghostty $ withTerminal config $ \t -> withRenderState t $ \rs -> do
        _ <- feed t "a"
        withFrame rs $ \outer -> do
          _ <- feed t "b"
          inner <- withFrame rs texts
          o <- texts outer
          pure (take 1 inner, take 1 o)
      r @?= (["a"], ["a"]),
    testCase "rows read in any order read right" $ do
      r <- rendered "r0\r\nr1\r\nr2" $ \f -> do
        rs <- allRows f
        let at i = maybe (pure "?") rowText (viaNonEmpty head (drop i rs))
        traverse at [2, 0, 1, 1, 0, 2]
      r @?= ["r2", "r0", "r1", "r1", "r0", "r2"],
    -- PLAN.md M1.7: copied cells survive the next update.
    testCase "cells copied from a frame survive the next update" $ do
      r <- ghostty $ withTerminal config $ \t -> withRenderState t $ \rs -> do
        _ <- feed t "old"
        before <- withFrame rs (topRows 1 cells)
        _ <- feed t "\rnew"
        after <- withFrame rs (topRows 1 cells)
        pure (map lineOf before, map lineOf after)
      r @?= (["old"], ["new"])
  ]

-- * Cells

cellTests :: [TestTree]
cellTests =
  [ testCase "a styled cell carries its style and resolved colour" $ do
      (c, palette) <- rendered "\ESC[1;31mX" $ \f -> (,) <$> firstCell f <*> getF f RenderColorPalette
      c.text @?= "X"
      c.style @?= defaultStyle {foreground = PaletteColor 1, bold = True}
      c.foreground @?= Just (paletteColor palette 1)
      c.background @?= Nothing
      c.width @?= Narrow,
    -- Regression 9 (DESIGN.md §4.3): at libghostty-vt 1.0 a cell with no
    -- colour of its own is GHOSTTY_NO_VALUE, where it was INVALID_VALUE, so
    -- every default-coloured cell read as an invariant failure.
    testCase "regression 9: a cell of the default colours has none of its own" $ do
      c <- rendered "X" firstCell
      (c.text, c.foreground, c.background) @?= ("X", Nothing, Nothing),
    testCase "a cell's attributes and underline read from the style's flag word" $ do
      c <- rendered "\ESC[2;3;9;53;4:3mX" firstCell
      c.style @?= defaultStyle {faint = True, italic = True, strikethrough = True, overline = True, underline = CurlyUnderline},
    testCase "a wide character is a wide cell and a spacer" $ do
      cs <- rendered "\230\151\165x" $ \f -> V.toList . V.take 3 <$> (allRows f >>= maybe (pure V.empty) cells . viaNonEmpty head)
      map (\c -> (c.text, c.width)) cs @?= [("\26085", Wide), ("", SpacerTail), ("x", Narrow)],
    testCase "a combining mark is part of its cell's text" $ do
      c <- rendered "e\204\129" firstCell
      c.text @?= "e\769",
    testCase "a linked cell reads its URI while the terminal is the frame's" $ do
      r <- ghostty $ withTerminal config $ \t -> withRenderState t $ \rs -> do
        _ <- feed t "\ESC]8;;https://example.org\ESC\\L\ESC]8;;\ESC\\n"
        withFrame rs $ \f -> do
          live <- V.toList . V.take 2 <$> cellsOf f 0
          _ <- feed t "more"
          stale <- V.toList . V.take 1 <$> cellsOf f 0
          pure (map (.hyperlink) live, map (.hyperlink) stale)
      r @?= ([Just (Hyperlink (Just "https://example.org")), Nothing], [Just (Hyperlink Nothing)]),
    testCase "a background-only cell has a background and no text" $ do
      c <- rendered "\ESC[44m\ESC[K" firstCell
      (c.text, isJust c.background) @?= ("", True),
    testCase "OSC 133 marks the cells' semantic content" $ do
      cs <- rendered "\ESC]133;A\a> \ESC]133;B\ain\ESC]133;C\a\r\nout" $ \f -> do
        top <- cellsOf f 0
        next <- cellsOf f 1
        pure (map (.semantic) (V.toList (V.take 4 top)), map (.semantic) (V.toList (V.take 1 next)))
      cs @?= ([ContentPrompt, ContentPrompt, ContentInput, ContentInput], [ContentOutput])
  ]

-- * Synchronized output

holds :: [TestTree]
holds =
  [ -- DESIGN.md §4.3: the frame is the one the program had drawn when the
    -- hold began, though the rest of the write was processed after it.
    testCase "a hold shows the frame from when it began, until it ends" $ do
      r <- ghostty $ withTerminal config $ \t -> withRenderState t $ \rs -> do
        evs <- feed t "A\ESC[?2026hB"
        during <- withFrame rs $ \f -> (,frameHeld f) . take 1 <$> texts f
        evs' <- feed t "C\ESC[?2026l"
        after <- withFrame rs $ \f -> (,frameHeld f) . take 1 <$> texts f
        pure (evs, during, evs', after)
      r @?= ([RenderHold True], (["A"], True), [RenderHold False], (["ABC"], False)),
    testCase "a hold its owner ends by turning the mode off shows the terminal" $ do
      r <- ghostty $ withTerminal config $ \t -> withRenderState t $ \rs -> do
        _ <- feed t "A\ESC[?2026hB"
        _ <- withFrame rs (const pass)
        setT t (SetMode ModeSynchronizedOutput) False
        withFrame rs $ \f -> (,frameHeld f) . take 1 <$> texts f
      r @?= (["AB"], False),
    -- Regression 5 (§4.3): turning the mode off left the capture marked
    -- held until the next frame read the mode, so turning it on again
    -- first brought the old hold's frame back.
    testCase "regression 5: a hold turned off and on again by setT shows the terminal" $ do
      r <- ghostty $ withTerminal config $ \t -> withRenderState t $ \rs -> do
        _ <- feed t "A\ESC[?2026hB"
        setT t (SetMode ModeSynchronizedOutput) False
        setT t (SetMode ModeSynchronizedOutput) True
        withFrame rs $ \f -> (,frameHeld f) . take 1 <$> texts f
      r @?= (["AB"], False),
    testCase "a hold that begins while a frame is open is not captured" $ do
      r <- ghostty $ withTerminal config $ \t -> withRenderState t $ \rs -> do
        _ <- feed t "A"
        open <- withFrame rs $ \f -> do
          _ <- feed t "\ESC[?2026hB"
          take 1 <$> texts f
        next <- withFrame rs $ \f -> (,frameHeld f) . take 1 <$> texts f
        pure (open, next)
      r @?= (["A"], (["AB"], False)),
    testCase "a render state opened during a hold shows the terminal" $ do
      r <- ghostty $ withTerminal config $ \t -> do
        _ <- feed t "A\ESC[?2026hB"
        withRenderState t $ \rs -> withFrame rs $ \f -> (,frameHeld f) . take 1 <$> texts f
      r @?= (["AB"], False)
  ]

-- * Keys

keys :: [TestTree]
keys =
  [ testCase "every frame key reads its value on a fresh frame" $ do
      failures <- ghostty $ withTerminal config $ \t -> do
        setT t SetColorForeground (Just fg)
        setT t SetColorBackground (Just bg)
        setT t SetColorCursor (Just cur)
        _ <- feed t "ab\r\nc"
        palette <- getT t ColorPalette
        withRenderState t $ \rs -> withFrame rs $ \f ->
          concat <$> traverse (\(Some k) -> freshFrame palette f k) allFrameData
      failures @?= [],
    -- The viewport is scrolled up into the scrollback through the raw call,
    -- which the high level does not bind yet, so the cursor's row is below
    -- it.
    testCase "the cursor reads as nowhere when it is outside the viewport" $ do
      r <- ghostty $ withTerminal config $ \t -> do
        _ <- feed t (mconcat (replicate 30 "line\r\n"))
        liftIO (with (ScrollViewportDelta (-5)) (ghosttyTerminalScrollViewport t.handle)) >>= liftIO . (@?= GhosttySuccess)
        withRenderState t $ \rs -> withFrame rs $ \f ->
          (,,) <$> getF f RenderCursorViewportX <*> getF f RenderCursorViewportWideTail <*> ((.position) <$> getF f RenderCursor)
      r @?= (Nothing, Nothing, Nothing),
    testCase "every frame key is a constructor, or deferred" $
      sort (map (\(Some k) -> frameNumber (frameKey k)) allFrameData <> map snd deferredFrameData)
        @?= sort [v | (n :: Text, v) <- ghosttyRenderStateDataEnum.values, n /= "GHOSTTY_RENDER_STATE_DATA_INVALID"],
    testCase "every frame key is its header row's name" $
      filter
        (\(c, h) -> h /= Just ("GHOSTTY_RENDER_STATE_DATA_" <> c))
        (map (\(Some k) -> (headerName (fromMaybe "" (stripPrefix "Render" (show k))), rowName ghosttyRenderStateDataEnum (frameNumber (frameKey k)))) allFrameData)
        @?= [],
    testCase "every row key reads its value on a fresh frame" $ do
      failures <- ghostty $ withTerminal config $ \t -> do
        _ <- feed t "abcdefghij\r\n"
        withRenderState t $ \rs -> withFrame rs $ \f -> do
          rows <- allRows f
          concat <$> traverse (\(Some k) -> concat <$> traverse (`freshRow` k) (take 2 rows)) allRowData
      failures @?= [],
    -- The header: a row keeps its id as it moves; one new to the frame has
    -- an id never seen before. (Writing into a row keeps its id too.)
    testCase "a row keeps its identity as the screen scrolls" $ do
      r <- ghostty $ withTerminal config $ \t -> withRenderState t $ \rs -> do
        _ <- feed t (T.encodeUtf8 (T.intercalate "\r\n" [show i | i <- [0 .. 23 :: Int]]))
        ids <- withFrame rs (topRows 24 (`getR` RowId))
        _ <- feed t "\r\nnew"
        ids' <- withFrame rs (topRows 24 (`getR` RowId))
        pure (ordNub ids == ids, take 23 ids' == drop 1 ids, fmap (`elem` ids) (viaNonEmpty last ids'))
      r @?= (True, True, Just False),
    testCase "a soft-wrapped row says so" $ do
      r <- ghostty $ withTerminal (terminalConfig (grid 4 3)) $ \t -> withRenderState t $ \rs -> do
        _ <- feed t "abcdef"
        withFrame rs (topRows 2 (fmap (\fl -> (fl.wrap, fl.wrapContinuation)) . (`getR` RowRaw)))
      r @?= [(True, False), (False, True)],
    testCase "every row key is a constructor, or deferred" $
      sort (map (\(Some k) -> rowNumber (rowKey k)) allRowData <> map snd deferredRowData)
        @?= sort [v | (n :: Text, v) <- ghosttyRenderStateRowDataEnum.values, n /= "GHOSTTY_RENDER_STATE_ROW_DATA_INVALID"],
    testCase "every row key is its header row's name" $
      filter
        (\(c, h) -> h /= Just ("GHOSTTY_RENDER_STATE_ROW_DATA_" <> c))
        (map (\(Some k) -> (headerName (fromMaybe "" (stripPrefix "Row" (show k))), rowName ghosttyRenderStateRowDataEnum (rowNumber (rowKey k)))) allRowData)
        @?= [],
    testCase "every cell key is read by a copy, or deferred" $
      sort (map cellNumber cellKeys <> map snd deferredCellData)
        @?= sort [v | (n :: Text, v) <- ghosttyRenderStateRowCellsDataEnum.values, n /= "GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_INVALID"],
    -- No option has a constructor: a frame owns the dirty state, and no
    -- overscan is requested (DESIGN.md §4.3). INVALID (0) is no option.
    testCase "the render state and row options are all left to frames" $ do
      sort [v | (n :: Text, v) <- ghosttyRenderStateOptionEnum.values, n /= "GHOSTTY_RENDER_STATE_OPT_INVALID"]
        @?= sort [optionNumber GhosttyRenderStateOptDirty, optionNumber GhosttyRenderStateOptOverscan]
      [v | (n :: Text, v) <- ghosttyRenderStateRowOptionEnum.values, n /= "GHOSTTY_RENDER_STATE_ROW_OPT_INVALID"]
        @?= [rowOptionNumber GhosttyRenderStateRowOptDirty]
  ]

-- | Every frame key.
allFrameData :: [Some FrameData]
allFrameData =
  [ Some RenderCols,
    Some RenderRows,
    Some RenderDirty,
    Some RenderColorBackground,
    Some RenderColorForeground,
    Some RenderColorCursor,
    Some RenderColorCursorHasValue,
    Some RenderColorPalette,
    Some RenderCursorVisualStyle,
    Some RenderCursorVisible,
    Some RenderCursorBlinking,
    Some RenderCursorPasswordInput,
    Some RenderCursorViewportHasValue,
    Some RenderCursorViewportX,
    Some RenderCursorViewportY,
    Some RenderCursorViewportWideTail,
    Some RenderCursor,
    Some RenderColors
  ]

-- | The header's frame keys with no constructor, and why (DESIGN.md §4.3).
deferredFrameData :: [(Text, Integer)]
deferredFrameData =
  [ ("ROW_ITERATOR: how rows are read", frameNumber GhosttyRenderStateDataRowIterator),
    ("OVERSCAN: none is requested", frameNumber GhosttyRenderStateDataOverscan),
    ("OVERSCAN_REQUEST: none is requested", frameNumber GhosttyRenderStateDataOverscanRequest),
    ("IMAGE_ITERATOR: PLAN.md M6.15", frameNumber GhosttyRenderStateDataImageIterator),
    ("IMAGE_PLACEMENTS_GENERATION: PLAN.md M6.15", frameNumber GhosttyRenderStateDataImagePlacementsGeneration)
  ]

-- | Every row key.
allRowData :: [Some RowData]
allRowData = [Some RowDirty, Some RowRaw, Some RowViewportY, Some RowId]

-- | The header's row keys with no constructor, and why.
deferredRowData :: [(Text, Integer)]
deferredRowData =
  [ ("CELLS: how cells are read", rowNumber GhosttyRenderStateRowDataCells),
    ("CELLS_RAW: the same cells, packed", rowNumber GhosttyRenderStateRowDataCellsRaw),
    ("SELECTION: PLAN.md M6.5", rowNumber GhosttyRenderStateRowDataSelection)
  ]

-- | The header's cell keys a copy does not read, and why.
deferredCellData :: [(Text, Integer)]
deferredCellData =
  [ ("GRAPHEMES_LEN: GRAPHEMES_UTF8 reads the text", cellNumber GhosttyRenderStateRowCellsDataGraphemesLen),
    ("GRAPHEMES_BUF: GRAPHEMES_UTF8 reads the text", cellNumber GhosttyRenderStateRowCellsDataGraphemesBuf),
    ("SELECTED: PLAN.md M6.5", cellNumber GhosttyRenderStateRowCellsDataSelected)
  ]

-- | Read a frame key of an 80×24 terminal whose colours were set to 'fg',
-- 'bg' and 'cur', fed @ab\\r\\nc@, against what it should be. Total, so a
-- new constructor fails to compile until it has an expectation.
freshFrame :: Palette -> Frame s f -> FrameData a -> Eff '[Ghostty, IOE] [String]
freshFrame palette f k = do
  v <- getF f k
  let is :: (Eq b, Show b) => b -> b -> [String]
      is want got = [show k <> ": " <> show got <> ", expected " <> show want | want /= got]
  case k of
    RenderCols -> pure (is 80 v)
    RenderRows -> pure (is 24 v)
    RenderDirty -> pure (is FullyDirty v)
    RenderColorBackground -> pure (is bg v)
    RenderColorForeground -> pure (is fg v)
    RenderColorCursor -> pure (is (Just cur) v)
    RenderColorCursorHasValue -> pure (is True v)
    RenderColorPalette -> pure (is palette v)
    RenderCursorVisualStyle -> pure (is BlockCursor v)
    RenderCursorVisible -> pure (is True v)
    RenderCursorBlinking -> pure (is False v)
    RenderCursorPasswordInput -> pure (is False v)
    RenderCursorViewportHasValue -> pure (is True v)
    RenderCursorViewportX -> pure (is (Just 1) v)
    RenderCursorViewportY -> pure (is (Just 1) v)
    RenderCursorViewportWideTail -> pure (is (Just False) v)
    RenderCursor ->
      pure
        ( is
            Cursor
              { position = Just CursorPosition {point = Point {col = 1, row = 1}, wideTail = False},
                visible = True,
                blinking = False,
                passwordInput = False,
                shape = BlockCursor
              }
            v
        )
    RenderColors -> pure (is Colors {background = bg, foreground = fg, cursor = Just cur, palette} v)

-- | Read a row key of the top two rows of a frame of a terminal fed one
-- full-width-free line, against what it should be. Total, as 'freshFrame'.
freshRow :: Row s f -> RowData a -> Eff '[Ghostty, IOE] [String]
freshRow r k = do
  v <- getR r k
  let is :: (Eq b, Show b) => b -> b -> [String]
      is want got = [show k <> " at " <> show (rowY r) <> ": " <> show got <> ", expected " <> show want | want /= got]
  pure $ case k of
    RowDirty -> is True v
    RowViewportY -> is (fromIntegral (rowY r)) v
    RowId -> [show k <> ": zero" | v == RowIdentity 0 0]
    RowRaw ->
      is
        RowFlags
          { wrap = False,
            wrapContinuation = False,
            grapheme = False,
            styled = False,
            hasHyperlink = False,
            prompt = NoPrompt,
            kittyVirtualPlaceholder = False,
            dirty = v.dirty
          }
        v

-- * Helpers

-- | Feed an 80×24 terminal, and read one frame of it.
rendered :: ByteString -> (forall s f. Frame s f -> Eff '[Ghostty, IOE] a) -> IO a
rendered bytes k = ghostty $ withTerminal config $ \t -> withRenderState t $ \rs -> do
  _ <- feed t bytes
  withFrame rs k

-- | Something of each of the frame's top rows.
topRows :: Int -> (Row s f -> Eff '[Ghostty, IOE] a) -> Frame s f -> Eff '[Ghostty, IOE] [a]
topRows n g f = allRows f >>= traverse g . take n

-- | Every row's text, trailing blanks dropped.
texts :: Frame s f -> Eff '[Ghostty, IOE] [Text]
texts f = allRows f >>= traverse rowText

-- | A row's text.
rowText :: Row s f -> Eff '[Ghostty, IOE] Text
rowText r = lineOf <$> cells r

-- | Cells' text, trailing blanks dropped.
lineOf :: V.Vector Cell -> Text
lineOf = T.stripEnd . T.concat . map (.text) . V.toList

-- | The cells of a row of the frame.
cellsOf :: Frame s f -> Int -> Eff '[Ghostty, IOE] (V.Vector Cell)
cellsOf f i = allRows f >>= maybe (liftIO (assertFailure "no such row")) cells . viaNonEmpty head . drop i

-- | The frame's top-left cell.
firstCell :: Frame s f -> Eff '[Ghostty, IOE] Cell
firstCell f = cellsOf f 0 >>= maybe (liftIO (assertFailure "no cells")) pure . (V.!? 0)

-- | Colours set before a frame is read.
fg, bg, cur :: Rgb
fg = Rgb 1 2 3
bg = Rgb 4 5 6
cur = Rgb 7 8 9

-- | A constructor's name as the header spells its row (@CursorX@ is
-- @CURSOR_X@).
headerName :: String -> Text
headerName = toText . go . takeWhile (/= ' ')
  where
    go (c : cs) = toUpper c : concatMap (\x -> if isUpper x then ['_', x] else [toUpper x]) cs
    go [] = []

-- | The header's name for a row's number.
rowName :: CEnum -> Integer -> Maybe Text
rowName e n = fst <$> find ((== n) . snd) e.values

-- | Keys' numbers.
frameNumber :: GhosttyRenderStateData -> Integer
frameNumber (GhosttyRenderStateData n) = toInteger n

rowNumber :: GhosttyRenderStateRowData -> Integer
rowNumber (GhosttyRenderStateRowData n) = toInteger n

cellNumber :: GhosttyRenderStateRowCellsData -> Integer
cellNumber (GhosttyRenderStateRowCellsData n) = toInteger n

optionNumber :: GhosttyRenderStateOption -> Integer
optionNumber (GhosttyRenderStateOption n) = toInteger n

rowOptionNumber :: GhosttyRenderStateRowOption -> Integer
rowOptionNumber (GhosttyRenderStateRowOption n) = toInteger n
