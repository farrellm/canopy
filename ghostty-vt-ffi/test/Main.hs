module Main (main) where

import Test.Ghostty.FFI.BuildInfo qualified
import Test.Ghostty.FFI.Color qualified
import Test.Ghostty.FFI.Encode qualified
import Test.Ghostty.FFI.Formatter qualified
import Test.Ghostty.FFI.Layout qualified
import Test.Ghostty.FFI.Parse qualified
import Test.Ghostty.FFI.Render qualified
import Test.Ghostty.FFI.Terminal qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "ghostty-vt-ffi"
      [ Test.Ghostty.FFI.BuildInfo.tests,
        Test.Ghostty.FFI.Layout.tests,
        Test.Ghostty.FFI.Terminal.tests,
        Test.Ghostty.FFI.Render.tests,
        Test.Ghostty.FFI.Formatter.tests,
        Test.Ghostty.FFI.Encode.tests,
        Test.Ghostty.FFI.Parse.tests,
        Test.Ghostty.FFI.Color.tests
      ]
