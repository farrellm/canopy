-- | M0 smoke tests: the package builds, links and is wired (DESIGN.md §13).
module Test.Tendril (tests) where

import Effectful (runEff)
import Ghostty.Effectful (buildMetadata)
import Tendril qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "tendril"
    [ testCase "version" (Tendril.version @?= "0.1.0.0"),
      -- §11.2: libghostty is this suite's oracle. M0 checks that it links.
      testCase "the ghostty oracle is available" $ do
        b <- runEff buildMetadata
        assertBool "no build metadata" (isJust b)
    ]
