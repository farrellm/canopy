-- | M0 smoke tests: the package builds, links and is wired (DESIGN.md §13).
module Test.TreeSitter (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TreeSitter (supportedAbi)

tests :: TestTree
tests =
  testGroup
    "tree-sitter-effectful"
    [testCase "ABI range" (supportedAbi @?= (13, 15))]
