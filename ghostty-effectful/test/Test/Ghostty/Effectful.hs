-- | M0 smoke tests: the package builds, links and is wired (DESIGN.md §13).
module Test.Ghostty.Effectful (tests) where

import Data.Text qualified as T
import Effectful (runEff)
import Ghostty.Effectful (buildMetadata)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "build"
    [ testCase "the library reports its build" $ do
        b <- runEff buildMetadata
        assertBool (show b) (maybe False (not . T.null) b)
    ]
