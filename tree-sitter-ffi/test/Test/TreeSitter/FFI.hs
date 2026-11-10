-- | M0's wiring checks: the vendored runtime links, and a generated shim is
-- callable. §11.1's shim-by-shim comparison arrives with M4.
module Test.TreeSitter.FFI (tests) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes, toBool)
import Foreign.Ptr (nullPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TreeSitter.FFI.Api
import TreeSitter.FFI.Shims
import TreeSitter.FFI.Types

tests :: TestTree
tests =
  testGroup
    "FFI"
    [ testCase "the runtime loads grammar ABIs 13 to 15 (v0.27.1)" $ do
        (minCompatibleLanguageVersion, languageVersion) @?= (13, 15),
      testCase "a parser is created and deleted" $ do
        p <- tsParserNew
        assertBool "ts_parser_new returned NULL" (p /= nullPtr)
        tsParserDelete p,
      testCase "a zeroed TSNode is null, through its shim" $ do
        isNull <- allocaBytes sizeOfTSNode $ \n -> do
          fillBytes n 0 sizeOfTSNode
          toBool <$> tsffiNodeIsNull n
        assertBool "a zeroed node is not null" isNull
    ]
