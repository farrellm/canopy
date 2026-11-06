-- | M0 smoke tests: the package builds, links and is wired (DESIGN.md §13).
module Test.Canopy (tests) where

import Canopy (components)
import Data.List (lookup)
import Effectful (runEff)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "canopy"
    [ testCase "every component reports a version" $ do
        cs <- runEff components
        map fst cs @?= ["canopy", "tendril", "tree-sitter ABI", "libghostty-vt"]
        lookup "tree-sitter ABI" cs @?= Just "13..15"
    ]
