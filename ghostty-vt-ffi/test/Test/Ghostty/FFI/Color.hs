-- | @color.h@ against the linked library: parsing, palettes, the X11 table,
-- luminance and contrast.
module Test.Ghostty.FFI.Color (tests) where

import Data.ByteString qualified as BS
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CChar, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (..))
import Ghostty.FFI.Color
import Ghostty.FFI.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))

tests :: TestTree
tests =
  testGroup
    "Color"
    [ testCase "parse: hex, short hex, rgb:, an X11 name; and a failure" $ do
        parseWith ghosttyColorParse "#ff8000" >>= (@?= Right (GhosttyColorRgb 255 128 0))
        parseWith ghosttyColorParse "f80" >>= (@?= Right (GhosttyColorRgb 255 136 0))
        parseWith ghosttyColorParse "rgb:00/80/ff" >>= (@?= Right (GhosttyColorRgb 0 128 255))
        parseWith ghosttyColorParse " Red " >>= (@?= Right (GhosttyColorRgb 255 0 0))
        parseWith ghosttyColorParse "not a colour" >>= (@?= Left GhosttyInvalidValue),
      testCase "parse an X11 name, case-insensitively, but not hex" $ do
        parseWith ghosttyColorParseX11 "medium spring green" >>= (@?= Right (GhosttyColorRgb 0 250 154))
        parseWith ghosttyColorParseX11 "MEDIUMSPRINGGREEN" >>= (@?= Right (GhosttyColorRgb 0 250 154))
        parseWith ghosttyColorParseX11 "#ffffff" >>= (@?= Left GhosttyInvalidValue),
      testCase "parse a palette entry" $
        BS.useAsCStringLen "0x0c = #0000ff" $ \(s, n) -> alloca $ \ip -> alloca $ \cp -> do
          ghosttyColorParsePaletteEntry s (fromIntegral n) ip cp >>= (@?= GhosttySuccess)
          (,) <$> peek ip <*> peek cp >>= (@?= (12, GhosttyColorRgb 0 0 255)),
      testCase "rgb_get reads the components" $
        with (GhosttyColorRgb 1 2 3) $ \cp -> alloca $ \r -> alloca $ \g -> alloca $ \b -> do
          ghosttyColorRgbGet cp r g b
          (,,) <$> peek r <*> peek g <*> peek b >>= (@?= (1, 2, 3)),
      testCase "the default palette: the xterm cube and grey ramp" $ do
        pal <- defaultPalette
        length pal @?= 256
        -- Index 16 is the cube's black, 231 its white; 232 the ramp's darkest.
        (pal !!? 16, pal !!? 231, pal !!? 232)
          @?= (Just (GhosttyColorRgb 0 0 0), Just (GhosttyColorRgb 255 255 255), Just (GhosttyColorRgb 8 8 8)),
      testCase "a generated palette keeps 0–15 and the masked indices" $ do
        base <- defaultPalette
        let marked = GhosttyColorRgb 1 2 3
            base' = take 20 base <> [marked] <> drop 21 base
        out <- withArray base' $ \bp ->
          with (GhosttyColorPaletteMask [0x100000]) $ \mp ->
            with (GhosttyColorRgb 0 0 0) $ \bg -> with (GhosttyColorRgb 255 255 255) $ \fg ->
              allocaArray 256 $ \op -> do
                ghosttyColorPaletteGenerate bp mp bg fg 1 op
                peekArray 256 op
        take 16 out @?= take 16 base'
        out !!? 20 @?= Just marked,
      testCase "the X11 table ends after its count, with a null name" $ do
        n <- fromIntegral <$> ghosttyColorX11NameCount
        ConstPtr es <- ghosttyColorX11Names
        entries <- peekArray (n + 1) es
        (all (\e -> e.name /= nullPtr) (take n entries), (.name) <$> viaNonEmpty last entries)
          @?= (True, Just nullPtr),
      testCase "luminance and contrast" $ do
        white <- with (GhosttyColorRgb 255 255 255) ghosttyColorLuminance
        black <- with (GhosttyColorRgb 0 0 0) ghosttyColorLuminance
        (white, black) @?= (1, 0)
        perceived <- with (GhosttyColorRgb 255 255 255) ghosttyColorPerceivedLuminance
        abs (perceived - 1) < 1e-9 @? "perceived luminance of white is 1: " <> show perceived
        contrast <- with (GhosttyColorRgb 0 0 0) $ \a -> with (GhosttyColorRgb 255 255 255) $ \b -> ghosttyColorContrast a b
        contrast @?= 21
    ]

parseWith :: (Ptr CChar -> CSize -> Ptr GhosttyColorRgb -> IO GhosttyResult) -> ByteString -> IO (Either GhosttyResult GhosttyColorRgb)
parseWith call bytes = BS.useAsCStringLen bytes $ \(s, n) -> alloca $ \cp ->
  call s (fromIntegral n) cp >>= \case
    GhosttySuccess -> Right <$> peek cp
    other -> pure (Left other)

defaultPalette :: IO [GhosttyColorRgb]
defaultPalette = allocaArray 256 $ \p -> ghosttyColorPaletteDefault p >> peekArray 256 p
