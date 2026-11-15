-- | The kill ring (DESIGN.md §5.4): it keeps at most its capacity, and a
-- full turn of rotations comes back to where it started.
module Test.Tendril.Edit.KillRing (tests) where

import Tendril.Edit.KillRing (capacity, emptyKillRing, entries, kill, rotate)
import Test.QuickCheck (forAll, listOf, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Tendril.Text.Gen (genText)

tests :: TestTree
tests =
  testGroup
    "Tendril.Edit.KillRing"
    [ testCase "a kill beyond the capacity forgets the oldest" $ do
        let ring = foldl' (flip kill) emptyKillRing (map show [1 .. capacity + 1])
        length (entries ring) @?= capacity
        listToMaybe (entries ring) @?= Just (show (capacity + 1))
        viaNonEmpty last (entries ring) @?= Just "2",
      testProperty "as many rotations as entries are no rotation" $
        forAll (listOf genText) $ \ts ->
          let ring = foldl' (flip kill) emptyKillRing ts
           in entries (foldl' (\r _ -> rotate r) ring ts) === entries ring
    ]
