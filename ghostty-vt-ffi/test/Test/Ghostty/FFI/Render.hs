-- | The render state binding against the linked library: an update copies
-- the viewport, the dirty iteration reports only changed rows, and cells and
-- rows read back through every reading path the header offers (keyed, raw
-- value, bulk view, sized struct).
--
-- Smoke tests of the raw binding; the frames canopy relies on are tested
-- through @ghostty-effectful@ (M1.7).
module Test.Ghostty.FFI.Render (tests) where

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Foreign.C.Types (CBool)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (toBool, with)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))
import Ghostty.FFI.Color
import Ghostty.FFI.Render
import Ghostty.FFI.Screen
import Ghostty.FFI.Terminal
import Ghostty.FFI.Types
import Test.Ghostty.FFI.Support
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Render"
    [ testCase "an update copies the viewport, one row per screen row" $
        withRendered 10 3 "ab\r\ncd" $ \_ rs -> do
          getState rs GhosttyRenderStateDataCols >>= (@?= (10 :: Word16))
          getState rs GhosttyRenderStateDataRows >>= (@?= (3 :: Word16))
          rowTexts rs >>= (@?= ["ab", "cd", ""]),
      -- The cursor starts on the row written to: moving it would dirty the
      -- row it left as well.
      testCase "after clean, only the row written to is dirty" $
        withRendered 10 3 "a\r\nb\r\nc\ESC[2;2H" $ \t rs -> do
          ghosttyRenderStateClean rs >>= (@?= GhosttySuccess)
          getState rs GhosttyRenderStateDataDirty >>= (@?= GhosttyRenderStateDirtyFalse)
          feed t "x"
          ghosttyRenderStateUpdate rs t >>= (@?= GhosttySuccess)
          getState rs GhosttyRenderStateDataDirty >>= (@?= GhosttyRenderStateDirtyPartial)
          withRows rs dirtyRows >>= (@?= [1]),
      testCase "a cell reads the same keyed, raw, and through the bulk view" $
        -- U+00E9 as UTF-8: a ByteString literal keeps only each Char's low byte.
        withRendered 10 3 "\ESC[31m\xc3\xa9" $ \_ rs -> withRows rs $ \it -> do
          ghosttyRenderStateRowIteratorNext it >>= (@?= 1)
          view :: GhosttyCellsView <- getRow it GhosttyRenderStateRowDataCellsRaw
          view.len @?= 10
          bulk <- peekArray 1 view.ptr
          withCells it $ \cs -> do
            ghosttyRenderStateRowCellsNext cs >>= (@?= 1)
            raw :: GhosttyCell <- getCell cs GhosttyRenderStateRowCellsDataRaw
            bulk @?= [raw]
            cellValue raw GhosttyCellDataCodepoint >>= (@?= (0xe9 :: Word32))
            cellValue raw GhosttyCellDataWide >>= (@?= GhosttyCellWideNarrow)
            cellValue raw GhosttyCellDataHasStyling >>= (@?= (1 :: CBool))
            getCell cs GhosttyRenderStateRowCellsDataGraphemesLen >>= (@?= (1 :: Word32))
            utf8 <- cellUtf8 cs
            utf8 @?= "\xc3\xa9"
            -- Red is palette entry 1, resolved through the palette.
            palette <- withSized ghosttyRenderStateColorsLayout $ \p -> do
              ghosttyRenderStateGet rs GhosttyRenderStateDataColors (castPtr p) >>= (@?= GhosttySuccess)
              (.palette) <$> (peek p :: IO GhosttyRenderStateColors)
            length palette @?= 256
            fg :: GhosttyColorRgb <- getCell cs GhosttyRenderStateRowCellsDataFgColor
            Just fg @?= viaNonEmpty head (drop 1 palette),
      testCase "the cursor reads the same as one struct and as separate keys" $
        withRendered 10 3 "abc\r\nd" $ \_ rs -> do
          cursor <- withSized ghosttyRenderStateCursorLayout $ \p -> do
            ghosttyRenderStateGet rs GhosttyRenderStateDataCursor (castPtr p) >>= (@?= GhosttySuccess)
            peek p :: IO GhosttyRenderStateCursor
          (cursor.viewportHasValue, cursor.viewportX, cursor.viewportY) @?= (1, 1, 1)
          getState rs GhosttyRenderStateDataCursorViewportX >>= (@?= cursor.viewportX)
          getState rs GhosttyRenderStateDataCursorVisualStyle >>= (@?= cursor.visualStyle),
      testCase "a row's raw value reads through ghostty_row_get" $
        withRendered 4 3 "abcdef" $ \_ rs -> withRows rs $ \it -> do
          ghosttyRenderStateRowIteratorNext it >>= (@?= 1)
          top :: GhosttyRow <- getRow it GhosttyRenderStateRowDataRaw
          rowValue top GhosttyRowDataWrap >>= (@?= (1 :: CBool))
          ghosttyRenderStateRowIteratorNext it >>= (@?= 1)
          next :: GhosttyRow <- getRow it GhosttyRenderStateRowDataRaw
          rowValue next GhosttyRowDataWrapContinuation >>= (@?= (1 :: CBool)),
      testCase "an image iterator over a screen with no images is empty" $
        withRendered 10 3 "text" $ \_ rs ->
          bracket (newHandle ghosttyRenderStateImageIteratorNew) ghosttyRenderStateImageIteratorFree $ \it -> do
            with it $ \p -> ghosttyRenderStateGet rs GhosttyRenderStateDataImageIterator (castPtr p) >>= (@?= GhosttySuccess)
            ghosttyRenderStateImageIteratorNext it >>= (@?= 0)
    ]

-- * Helpers

-- | A terminal fed some bytes, and a render state updated from it.
withRendered :: Word16 -> Word16 -> ByteString -> (GhosttyTerminal -> GhosttyRenderState -> Assertion) -> Assertion
withRendered cols rows bytes act =
  withTerminal cols rows $ \t ->
    bracket (newHandle ghosttyRenderStateNew) ghosttyRenderStateFree $ \rs -> do
      feed t bytes
      ghosttyRenderStateUpdate rs t >>= (@?= GhosttySuccess)
      act t rs

-- | A row iterator filled from the render state.
withRows :: GhosttyRenderState -> (GhosttyRenderStateRowIterator -> IO a) -> IO a
withRows rs act =
  bracket (newHandle ghosttyRenderStateRowIteratorNew) ghosttyRenderStateRowIteratorFree $ \it -> do
    withValue it (ghosttyRenderStateGet rs GhosttyRenderStateDataRowIterator . castPtr)
    act it

-- | A cell iterator filled from the iterator's current row.
withCells :: GhosttyRenderStateRowIterator -> (GhosttyRenderStateRowCells -> IO a) -> IO a
withCells it act =
  bracket (newHandle ghosttyRenderStateRowCellsNew) ghosttyRenderStateRowCellsFree $ \cs -> do
    withValue cs (ghosttyRenderStateRowGet it GhosttyRenderStateRowDataCells . castPtr)
    act cs

-- | The rows' text, through each cell's UTF-8 grapheme, trailing blanks
-- dropped.
rowTexts :: GhosttyRenderState -> IO [ByteString]
rowTexts rs = withRows rs $ \it ->
  unfoldWhileM (toBool <$> ghosttyRenderStateRowIteratorNext it) $
    withCells it $ \cs ->
      mconcat <$> unfoldWhileM (toBool <$> ghosttyRenderStateRowCellsNext cs) (cellUtf8 cs)

-- | The positions of the rows that need a redraw.
dirtyRows :: GhosttyRenderStateRowIterator -> IO [Word16]
dirtyRows it = alloca $ \yp -> unfoldWhileM (toBool <$> ghosttyRenderStateRowIteratorNextDirty it yp) (peek yp)

-- | The current cell's grapheme as UTF-8, through a 'GhosttyBuffer'.
cellUtf8 :: GhosttyRenderStateRowCells -> IO ByteString
cellUtf8 cs = allocaArray 16 $ \buf ->
  with (GhosttyBuffer buf 16 0) $ \bp -> do
    ghosttyRenderStateRowCellsGet cs GhosttyRenderStateRowCellsDataGraphemesUtf8 (castPtr bp) >>= (@?= GhosttySuccess)
    n <- (.len) <$> peek bp
    BS.packCStringLen (castPtr buf, fromIntegral n)

getState :: (Storable a) => GhosttyRenderState -> GhosttyRenderStateData -> IO a
getState rs k = outValue (ghosttyRenderStateGet rs k . castPtr)

getRow :: (Storable a) => GhosttyRenderStateRowIterator -> GhosttyRenderStateRowData -> IO a
getRow it k = outValue (ghosttyRenderStateRowGet it k . castPtr)

getCell :: (Storable a) => GhosttyRenderStateRowCells -> GhosttyRenderStateRowCellsData -> IO a
getCell cs k = outValue (ghosttyRenderStateRowCellsGet cs k . castPtr)

cellValue :: (Storable a) => GhosttyCell -> GhosttyCellData -> IO a
cellValue c k = outValue (ghosttyCellGet c k . castPtr)

rowValue :: (Storable a) => GhosttyRow -> GhosttyRowData -> IO a
rowValue r k = outValue (ghosttyRowGet r k . castPtr)
