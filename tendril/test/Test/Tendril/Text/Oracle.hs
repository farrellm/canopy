-- | libghostty as the oracle for T0's widths (DESIGN.md §5.3, §11.2): every
-- code point's width, and the clusters and cluster widths of a corpus and of
-- generated texts, agree with the terminal's. A disagreement is tendril's
-- bug, since width is the terminal's decision.
--
-- @ghostty_unicode_grapheme_width@ leaves a cluster of controls (GCB
-- @Control@, @CR@, @LF@) unspecified, so the texts compared have none.
module Test.Tendril.Text.Oracle (tests) where

import Data.Text qualified as T
import Effectful (Eff, IOE, runEff)
import Ghostty.Effectful (Ghostty, runGhostty)
import Ghostty.Effectful.Unicode (Grapheme (..), codepointWidth, graphemes)
import Numeric (showHex)
import Tendril.Text.Grapheme (clusters)
import Tendril.Text.Unicode.Tables (GraphemeBreak (..), graphemeBreak)
import Tendril.Text.Unicode.Tables qualified as Tables
import Tendril.Text.Units (fromOffset)
import Tendril.Text.Width (WidthMode (..), clusterWidth, textWidth)
import Test.QuickCheck (Gen, Property, forAllShrink, ioProperty, shrinkList, (.&&.), (===))
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
import Test.Tendril.Text.Gen (genText)

tests :: TestTree
tests =
  testGroup
    "libghostty oracle"
    [ testCase "every code point's width" $ do
        bad <- oracle $ filterM (\c -> (/= Tables.width c) . fromIntegral <$> codepointWidth c) [minBound .. maxBound]
        case bad of
          [] -> pass
          _ ->
            assertFailure . toString $
              show (length bad) <> " code points disagree, among them " <> unwords (map codePoint (take 20 bad)),
      testCase "a corpus of clusters" $ do
        theirs <- oracle (traverse graphemes corpus)
        [(t, ours t, g) | (t, g) <- zip corpus (map (map pair) theirs), ours t /= g] @?= [],
      adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 1000)) $
        testProperty "generated texts' clusters and widths" sameAsGhostty
    ]

-- | Tendril's clusters of a text, each with its width.
ours :: Text -> [(Text, Word)]
ours = map (\c -> (c, fromOffset (clusterWidth c))) . clusters

codePoint :: Char -> Text
codePoint c = "U+" <> toText (showHex (ord c) "")

pair :: Grapheme -> (Text, Word)
pair g = (g.cluster, fromIntegral g.width)

-- | A text's clusters and their widths are ghostty's, and so is its width
-- in grapheme-cluster mode. Many texts, since the generator weights the hard
-- code points: the million of M2.2's probe agreed, and a tenth of that takes
-- about two seconds. So the count is a thousand times tasty's, which keeps
-- @--quickcheck-tests@ in charge. A failure shrinks by dropping code points, which
-- keeps the controls out.
sameAsGhostty :: Property
sameAsGhostty =
  forAllShrink noControls (map T.pack . shrinkList (const []) . T.unpack) $ \t -> ioProperty $ do
    theirs <- map pair <$> oracle (graphemes t)
    pure $
      ours t === theirs
        .&&. fromOffset (textWidth Clusters t) === sum (map snd theirs)

-- | 'genText' without the controls whose clusters the oracle leaves
-- unspecified.
noControls :: Gen Text
noControls = T.filter (\c -> graphemeBreak c `notElem` [Control, CR, LF]) <$> genText

oracle :: Eff '[Ghostty, IOE] a -> IO a
oracle act =
  runEff (runGhostty act) >>= \case
    Left e -> fail ("libghostty-vt refused: " <> show e)
    Right a -> pure a

-- | §11.2's clusters: emoji ZWJ sequences, flags, combining marks, Hangul,
-- and the cases of the width rule and of D33's tailorings.
corpus :: [Text]
corpus =
  [ -- ZWJ sequences
    "\x1F468\x200D\x1F469\x200D\x1F467\x200D\x1F466", -- a family of four
    "\x1F469\x1F3FD\x200D\x1F4BB", -- a technologist with a skin tone
    "\x1F3F3\xFE0F\x200D\x1F308", -- the rainbow flag: VS16, then ZWJ
    "\x1F3F4\x200D\x2620\xFE0F", -- the pirate flag
    "\x2764\xFE0F\x200D\x1F525", -- a heart on fire
    "\x1F441\xFE0F\x200D\x1F5E8\xFE0F", -- an eye in a speech bubble
    "a\x200D\x1F600", -- a ZWJ after no pictograph does not join
    "\x1F600\x200D", -- a trailing ZWJ
    -- flags: regional-indicator pairs, and an odd one out
    "\x1F1FA\x1F1F8",
    "\x1F1EF\x1F1F5\x1F1EB\x1F1F7",
    "\x1F1FA\x1F1F8\x1F1EB",
    "\x1F1E6",
    "\x1F3F4\xE0067\xE0062\xE0073\xE0063\xE0074\xE007F", -- Scotland: tag sequence
    -- combining marks
    "e\x0301",
    "a\x0300\x0316\x0323\x0331",
    "\x0301", -- a lone mark
    "\x05E9\x05BC\x05C1", -- Hebrew shin with dagesh and shin dot
    "\x0E01\x0E33", -- Thai SARA AM, a spacing mark
    "\x0915\x094D\x0937", -- a Devanagari conjunct (GB9c)
    "\x0915\x093C\x094D\x0937\x093F", -- with a nukta and a vowel sign
    "\x0D15\x0D4D\x0D15", -- Malayalam
    "\x0600\&1", -- an Arabic prepend
    -- Hangul
    "\x1100\x1161\x11A8", -- L V T
    "\x1100\x1100\x1161", -- L L V
    "\xAC00\x11A8", -- LV T
    "\xAC01\x11A8", -- LVT T
    "\xAC01\x1161", -- LVT then V: two clusters
    "\x1161\x11A8",
    "\xAC01\xFE0F\x11A8", -- a stray VS16 inside a syllable (D33)
    -- the width rule
    "\x2764",
    "\x2764\xFE0F",
    "\x231A",
    "\x231A\xFE0E",
    "a\xFE0F",
    "\x1F44D\x1F3FD",
    "q\x1F3FB", -- a skin tone after no modifier base (D33)
    "\x1F3FB",
    "\x6F22\x0301", -- a wide character and a mark
    "1\xFE0F\x20E3", -- a keycap
    "\x2600\xFE0F\x2600", -- a variation base, wide, then text-default
    "\x10400", -- an astral letter
    "\x1F600\x1F3FB" -- a non-base emoji then a skin tone
  ]
