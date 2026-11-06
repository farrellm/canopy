module Main (main) where

import Test.Ghostty.Effectful qualified
import Test.Ghostty.Effectful.Terminal qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "ghostty-effectful"
      [Test.Ghostty.Effectful.tests, Test.Ghostty.Effectful.Terminal.tests]
