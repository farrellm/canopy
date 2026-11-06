module Main (main) where

import Test.Ghostty.FFI.BuildInfo qualified
import Test.Ghostty.FFI.Layout qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "ghostty-vt-ffi" [Test.Ghostty.FFI.BuildInfo.tests, Test.Ghostty.FFI.Layout.tests])
