module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.TreeSitter.FFI qualified

main :: IO ()
main = defaultMain (testGroup "tree-sitter-ffi" [Test.TreeSitter.FFI.tests])
