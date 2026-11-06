module Main (main) where

import Test.Tasty (defaultMain)
import Test.TreeSitter qualified

main :: IO ()
main = defaultMain Test.TreeSitter.tests
