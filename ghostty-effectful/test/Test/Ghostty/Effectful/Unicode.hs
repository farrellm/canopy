-- | Widths (PLAN.md M1.9, DESIGN.md §4.8): a character's cells, and grapheme
-- clusters as the terminal segments and measures them with mode 2027. The
-- corpus is §11.2's: combining marks, emoji ZWJ sequences, skin tones,
-- flags, variation selectors and Hangul.
module Test.Ghostty.Effectful.Unicode (tests) where

import Ghostty.Effectful.Unicode
import Test.Ghostty.Effectful.Support (ghostty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Unicode"
    [ testCase "a character's width: narrow, wide, combining, zero-width, control" $ do
        r <- ghostty (traverse codepointWidth "a漢\x301\x200d\t👍")
        r @?= [1, 2, 0, 0, 0, 2],
      testCase "clusters and their widths" $ do
        r <- ghostty (traverse (fmap (map (\g -> (g.cluster, g.width))) . graphemes) corpus)
        r
          @?= [ [("e\x301", 1)],
                [("👨\x200d👩\x200d👧", 2)],
                [("👍🏽", 2)],
                [("🇺🇸", 2)],
                [("❤\xfe0f", 2)],
                [("☺\xfe0e", 1)],
                [("\x1100\x1161\x11a8", 2)],
                [("a", 1), ("漢", 2), ("b", 1)]
              ],
      testCase "the clusters are the text, in order" $ do
        let text = mconcat corpus
        r <- ghostty (graphemes text)
        mconcat (map (.cluster) r) @?= text,
      testCase "no text, no clusters" $ do
        r <- ghostty (graphemes "")
        r @?= []
    ]

-- | Clusters of every kind §11.2 names.
corpus :: [Text]
corpus =
  [ "e\x301", -- a combining acute
    "👨\x200d👩\x200d👧", -- a ZWJ family
    "👍🏽", -- a skin tone
    "🇺🇸", -- a flag: two regional indicators
    "❤\xfe0f", -- VS16 makes a narrow emoji base wide
    "☺\xfe0e", -- VS15 keeps it narrow
    "\x1100\x1161\x11a8", -- Hangul jamo: one syllable
    "a漢b" -- three clusters
  ]
