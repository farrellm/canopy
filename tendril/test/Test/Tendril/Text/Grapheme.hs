-- | Grapheme clusters (DESIGN.md §5.3): UAX #29's conformance cases, and
-- properties of segmenting any text.
module Test.Tendril.Text.Grapheme (tests) where

import Data.Text qualified as T
import Tendril.Text.Grapheme (boundaries, clusters)
import Tendril.Text.Unicode.Tables (GraphemeBreak (..), emojiModifier, emojiModifierBase, graphemeBreak)
import Tendril.Text.Units (Offset, Unit (..), fromOffset, offset)
import Test.QuickCheck (forAll, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Tendril.Text.Gen (genText)
import Test.Tendril.Text.Grapheme.Conformance (cases)

tests :: TestTree
tests =
  testGroup
    "Tendril.Text.Grapheme"
    [ testCase ("GraphemeBreakTest.txt, " <> show (length cases) <> " cases") $
        case [n | (n, expected) <- cases, let e = tailored expected, clusters (T.pack (concat e)) /= map T.pack e] of
          [] -> pass
          bad -> assertFailure ("lines that segment differently: " <> show bad),
      -- The tailoring (DESIGN.md §5.3): a skin tone continues a cluster only
      -- after an emoji modifier base, as libghostty-vt segments.
      testCase "a skin tone continues only a modifier base" $
        clusters "\x1F44D\x1F3FBq\x1F3FB\x1F600\x1F3FB\x0915\x094D\x1F3FB\x0915"
          @?= ["\x1F44D\x1F3FB", "q", "\x1F3FB", "\x1F600", "\x1F3FB", "\x0915\x094D", "\x1F3FB", "\x0915"],
      -- The second: a stray variation selector is invisible to the rules.
      testCase "a stray variation selector does not end a Hangul syllable" $
        clusters "\xAC01\xFE0F\x11A8\x2764\xFE0F\x11A8"
          @?= ["\xAC01\xFE0F\x11A8", "\x2764\xFE0F", "\x11A8"],
      testProperty "the clusters are the text, none empty" $
        forAll genText $ \t ->
          let cs = clusters t in (T.concat cs, any T.null cs) === (t, False),
      testProperty "the boundaries are the clusters' ends" $
        forAll genText $ \t ->
          boundaries t === scanl (\o c -> plus o (T.length c)) (offset 0) (clusters t)
    ]

-- | A case's clusters under libghostty-vt's tailoring (DESIGN.md §5.3), as
-- uucode's own conformance test adjusts them: a break before each emoji
-- modifier that does not follow an emoji modifier base or a prepend (GB9b
-- still holds).
tailored :: [[Char]] -> [[Char]]
tailored = concatMap (split [])
  where
    split acc = \case
      p : m : rest
        | emojiModifier m && not (emojiModifierBase p) && graphemeBreak p /= Prepend ->
            reverse (p : acc) : split [] (m : rest)
      c : rest -> split (c : acc) rest
      [] -> [reverse acc]

plus :: Offset 'CodePoint -> Int -> Offset 'CodePoint
plus o n = offset (fromOffset o + fromIntegral n)
