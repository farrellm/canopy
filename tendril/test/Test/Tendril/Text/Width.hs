-- | Display width (DESIGN.md §5.3), by hand for the clusters whose rule is
-- subtle. "Test.Tendril.Text.Oracle" holds them against libghostty's.
module Test.Tendril.Text.Width (tests) where

import Tendril.Text.Unicode.Tables (width)
import Tendril.Text.Units (fromOffset)
import Tendril.Text.Width (WidthMode (..), clusterWidth, textWidth)
import Test.QuickCheck (forAll, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Tendril.Text.Gen (genText)

tests :: TestTree
tests =
  testGroup
    "Tendril.Text.Width"
    [ testCase "clusters" $
        map
          (fromOffset . clusterWidth)
          [ "a",
            "e\x0301", -- a combining mark adds nothing
            "\x1F44D\x1F3FD", -- an emoji and its skin tone
            "\x1F3FB", -- a skin tone alone
            "\x2764", -- a text-default heart
            "\x2764\xFE0F", -- VS16 makes it wide
            "\x231A\xFE0E", -- VS15 makes a wide watch narrow
            "a\xFE0F", -- VS16 after no base is ignored
            "\x1F1FA\x1F1F8", -- a flag
            "\x1F468\x200D\x1F469\x200D\x1F467", -- a ZWJ family
            "\x1100\x1161\x11A8", -- Hangul L V T
            "\x0915\x094D\x0937", -- a Devanagari conjunct (GB9c)
            "\x0600\&1", -- a prepend keeps its width
            "\x0301" -- a lone mark
          ]
          @?= [1, 1, 2, 2, 1, 2, 1, 1, 2, 2, 2, 2, 2, 0],
      testCase "an empty text" $ textWidth Clusters "" @?= textWidth CodePoints "",
      testProperty "without mode 2027, code point widths add" $
        forAll genText $ \t ->
          fromOffset (textWidth CodePoints t) === fromIntegral (sum (map width (toString t)))
    ]
