-- | The standalone parsers and the width tables against the linked library:
-- OSC commands (known, unknown and cancelled), SGR attributes (with the
-- accessor shims), and codepoint and grapheme widths.
module Test.Ghostty.FFI.Parse (tests) where

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool, CChar, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, withArrayLen)
import Foreign.Marshal.Utils (toBool, with)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..))
import Ghostty.FFI.Color (GhosttyColorRgb (..))
import Ghostty.FFI.Layout (CField (..), CStruct (..))
import Ghostty.FFI.Osc
import Ghostty.FFI.Sgr
import Ghostty.FFI.Types
import Ghostty.FFI.Unicode
import Test.Ghostty.FFI.Support (newHandle, outValue, unfoldWhileM)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Parse"
    [ testGroup
        "OSC"
        [ testCase "a window title, read as a GhosttyString" $
            withOsc $ \p -> do
              cmd <- oscCommand p "2;hello" 0x07
              ghosttyOscCommandType cmd >>= (@?= GhosttyOscCommandChangeWindowTitle)
              oscDatum cmd GhosttyOscDataChangeWindowTitleStr >>= ghosttyStringBytes >>= (@?= "hello"),
          testCase "a datum the command does not have is NO_VALUE" $
            withOsc $ \p -> do
              cmd <- oscCommand p "2;hello" 0x07
              alloca $ \(vp :: Ptr GhosttyString) ->
                ghosttyOscCommandData cmd GhosttyOscDataUnknownContent (castPtr vp) >>= (@?= GhosttyNoValue),
          testCase "an unknown number: invalid by default, kept with the option" $
            withOsc $ \p -> do
              oscCommand p "7400;status=busy" 0x07 >>= ghosttyOscCommandType >>= (@?= GhosttyOscCommandInvalid)
              ghosttyOscReset p
              with (1024 :: CSize) $ \v -> ghosttyOscSet p GhosttyOscOptUnknownMaxBytes (castPtr v) >>= (@?= GhosttySuccess)
              cmd <- oscCommand p "7400;status=busy" 0x5c
              ghosttyOscCommandType cmd >>= (@?= GhosttyOscCommandUnknown)
              oscDatum cmd GhosttyOscDataUnknownContent >>= ghosttyStringBytes >>= (@?= "7400;status=busy")
              oscDatum cmd GhosttyOscDataUnknownTruncated >>= (@?= (0 :: CBool))
              oscDatum cmd GhosttyOscDataUnknownTerminator >>= (@?= GhosttyOscTerminatorSt),
          testCase "a cancelled sequence (CAN) is no command" $
            withOsc $ \p -> do
              cmd <- oscCommand p "2;hello" 0x18
              cmd @?= GhosttyOscCommand nullPtr
              ghosttyOscCommandType cmd >>= (@?= GhosttyOscCommandInvalid)
        ],
      testGroup
        "SGR"
        [ testCase "bold, a direct colour, and a colon-separated curly underline" $
            withSgr $ \p -> do
              attrs <- sgrAttributes p [1, 38, 2, 10, 20, 30, 4, 3] (Just ";;;;;;:;")
              attrs @?= [SgrAttrBold, SgrAttrDirectColorFg (GhosttyColorRgb 10 20 30), SgrAttrUnderline GhosttySgrUnderlineCurly],
          testCase "reset restarts the iteration over the same parameters" $
            withSgr $ \p -> do
              _ <- sgrAttributes p [1, 31] Nothing
              ghosttySgrReset p
              rest <- sgrNextAll p
              rest @?= [SgrAttrBold, SgrAttrFg8 1],
          testCase "the accessor shims agree with the record" $
            withSgr $ \p -> do
              _ <- withArrayLen [7] $ \n ps -> ghosttySgrSetParams p ps nullPtr (fromIntegral n)
              alloca $ \ap -> do
                ghosttySgrNext p ap >>= (@?= 1)
                attr <- peek ap
                attr @?= SgrAttrInverse
                ghosttySgrAttributeTag ap >>= (@?= GhosttySgrAttrInverse)
                -- The value union, inside the attribute at its field's offset.
                let offset = maybe 0 (.offset) (find ((== "value") . (.name)) ghosttySgrAttributeLayout.fields)
                ghosttySgrAttributeValue ap >>= (@?= castPtr (ap `plusPtr` offset)),
          -- 38 with a colour space it does not know is unknown, and parsing
          -- resumes after the 38: 9 and 3 are strikethrough and italic.
          testCase "an unknown attribute's lists, through the shims" $
            withSgr $ \p -> do
              attrs <- sgrAttributes p [1, 38, 9, 3] Nothing
              case attrs of
                [SgrAttrBold, SgrAttrUnknown u, SgrAttrStrikethrough, SgrAttrItalic] -> do
                  full <- with u $ \up -> alloca $ \pp -> do
                    n <- ghosttySgrUnknownFull up pp
                    ConstPtr xs <- peek pp
                    peekArray (fromIntegral n) xs
                  partial <- with u $ \up -> alloca $ \pp -> do
                    n <- ghosttySgrUnknownPartial up pp
                    ConstPtr xs <- peek pp
                    peekArray (fromIntegral n) xs
                  fromIntegral u.fullLen @?= length full
                  (full, partial) @?= ([1, 38, 9, 3], [38, 9, 3])
                other -> assertFailure ("attributes: " <> show other)
        ],
      testGroup
        "Unicode"
        [ testCase "codepoint widths: narrow, wide, combining, past U+10FFFF" $
            map ghosttyUnicodeCodepointWidth [0x61, 0x4e2d, 0x0301, 0x110000] @?= [1, 2, 0, 1],
          testCase "grapheme widths: a flag, a combined accent, a ZWJ family" $ do
            grapheme [0x1f1fa, 0x1f1f8, 0x61] >>= (@?= (2, 2))
            grapheme [0x65, 0x0301] >>= (@?= (2, 1))
            grapheme [0x1f468, 0x200d, 0x1f469, 0x200d, 0x1f467] >>= (@?= (5, 2))
            grapheme [] >>= (@?= (0, 0))
        ]
    ]

-- * OSC

withOsc :: (GhosttyOscParser -> IO a) -> IO a
withOsc = bracket (newHandle ghosttyOscNew) ghosttyOscFree

-- | Feed a sequence's bytes and end it with its terminator.
oscCommand :: GhosttyOscParser -> ByteString -> Word8 -> IO GhosttyOscCommand
oscCommand p bytes end = do
  traverse_ (ghosttyOscNext p) (BS.unpack bytes)
  ghosttyOscEnd p end

oscDatum :: (Storable a) => GhosttyOscCommand -> GhosttyOscCommandData -> IO a
oscDatum cmd d = outValue (ghosttyOscCommandData cmd d . castPtr)

-- * SGR

withSgr :: (GhosttySgrParser -> IO a) -> IO a
withSgr = bracket (newHandle ghosttySgrNew) ghosttySgrFree

-- | Set the parameters, with their separators if given, and read every
-- attribute.
sgrAttributes :: GhosttySgrParser -> [Word16] -> Maybe ByteString -> IO [GhosttySgrAttribute]
sgrAttributes p params seps = do
  withArrayLen params $ \n ps -> withSeps $ \sp ->
    ghosttySgrSetParams p ps sp (fromIntegral n) >>= (@?= GhosttySuccess)
  sgrNextAll p
  where
    withSeps :: (Ptr CChar -> IO a) -> IO a
    withSeps act = maybe (act nullPtr) (`BS.useAsCString` act) seps

sgrNextAll :: GhosttySgrParser -> IO [GhosttySgrAttribute]
sgrNextAll p = alloca $ \ap -> unfoldWhileM (toBool <$> ghosttySgrNext p ap) (peek ap)

-- * Unicode

-- | The first cluster's codepoint count and width.
grapheme :: [Word32] -> IO (Int, Word8)
grapheme cps = withArrayLen cps $ \n p -> alloca $ \wp -> do
  poke wp 0
  used <- ghosttyUnicodeGraphemeWidth p (fromIntegral n) wp
  (fromIntegral used,) <$> peek wp
