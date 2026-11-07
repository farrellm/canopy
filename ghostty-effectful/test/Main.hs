module Main (main) where

import Test.Ghostty.Effectful qualified
import Test.Ghostty.Effectful.Encode qualified
import Test.Ghostty.Effectful.Events qualified
import Test.Ghostty.Effectful.Format qualified
import Test.Ghostty.Effectful.Keys qualified
import Test.Ghostty.Effectful.Parse qualified
import Test.Ghostty.Effectful.Render qualified
import Test.Ghostty.Effectful.Terminal qualified
import Test.Ghostty.Effectful.TrackedRef qualified
import Test.Ghostty.Effectful.Unicode qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "ghostty-effectful"
      [ Test.Ghostty.Effectful.tests,
        Test.Ghostty.Effectful.Terminal.tests,
        Test.Ghostty.Effectful.Keys.tests,
        Test.Ghostty.Effectful.Events.tests,
        Test.Ghostty.Effectful.TrackedRef.tests,
        Test.Ghostty.Effectful.Render.tests,
        Test.Ghostty.Effectful.Format.tests,
        Test.Ghostty.Effectful.Encode.tests,
        Test.Ghostty.Effectful.Parse.tests,
        Test.Ghostty.Effectful.Unicode.tests
      ]
