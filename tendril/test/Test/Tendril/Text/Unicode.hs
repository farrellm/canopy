-- | The generated Unicode tables (DESIGN.md §5.3): what GHC decoded from the
-- table literals is what the generator digested, and a few values by hand.
module Test.Tendril.Text.Unicode (tests) where

import Data.List (lookup)
import Tendril.Text.Unicode.Tables
  ( GraphemeBreak (..),
    IndicConjunctBreak (..),
    emojiVariationBase,
    extendedPictographic,
    graphemeBreak,
    indicConjunctBreak,
    width,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tendril.Text.Unicode.Expected (digests)

tests :: TestTree
tests =
  testGroup
    "Tendril.Text.Unicode.Tables"
    [ testGroup "digests over every code point" (map digestCase digests),
      testCase "the digests name every lookup" $
        map fst digests @?= map fst lookups,
      testCase "grapheme break" $
        map graphemeBreak "\r\n\t\x200D\x0301\x1F1E6\x0600\x0903\x1100\x1160\x11A8\xAC00\xAC01\&a"
          @?= [ CR,
                LF,
                Control,
                ZWJ,
                Extend,
                RegionalIndicator,
                Prepend,
                SpacingMark,
                HangulL,
                HangulV,
                HangulT,
                HangulLV,
                HangulLVT,
                Other
              ],
      testCase "Indic conjunct break" $
        map indicConjunctBreak "\x094D\x0915\x093C\&a"
          @?= [IncbLinker, IncbConsonant, IncbExtend, IncbNone],
      testCase "width" $
        -- NUL, a letter, a wide CJK character, a combining acute, ZWJ, a soft
        -- hyphen, an emoji, a regional indicator, a Hangul medial vowel, an
        -- emoji modifier, the combining enclosing keycap (an enclosing mark, so 0
        -- though uucode gives it 2 alone), a two-em dash, and
        -- the last code point (unassigned).
        map width "\x0000\&a\x6F22\x0301\x200D\x00AD\x1F600\x1F1E6\x1160\x1F3FB\x20E3\x2E3A\x10FFFF"
          @?= [0, 1, 2, 0, 0, 1, 2, 2, 0, 2, 0, 2, 1],
      testCase "a CJK block's unassigned code points default to wide" $
        width '\x2FFFD' @?= 2
    ]

digestCase :: (Text, Word64) -> TestTree
digestCase (name, expected) = testCase (toString name) $
  case lookup name lookups of
    Nothing -> assertFailure ("no lookup named " <> toString name)
    Just f -> digest (map f [minBound .. maxBound]) @?= expected

-- | Each lookup, as the generator digests it: one byte per code point.
lookups :: [(Text, Char -> Word8)]
lookups =
  [ ("graphemeBreak", fromIntegral . fromEnum . graphemeBreak),
    ("indicConjunctBreak", fromIntegral . fromEnum . indicConjunctBreak),
    ("extendedPictographic", bool 0 1 . extendedPictographic),
    ("width", fromIntegral . width),
    ("emojiVariationBase", bool 0 1 . emojiVariationBase)
  ]

-- | 64-bit FNV-1a, the generator's @fnv1a@.
digest :: [Word8] -> Word64
digest = foldl' step 0xcbf29ce484222325
  where
    step h b = (h `xor` fromIntegral b) * 0x100000001b3
