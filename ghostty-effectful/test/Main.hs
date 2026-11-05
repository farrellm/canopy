module Main (main) where

import Test.Ghostty.Effectful qualified
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain Test.Ghostty.Effectful.tests
