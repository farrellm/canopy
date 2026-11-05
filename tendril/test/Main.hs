module Main (main) where

import Test.Tasty (defaultMain)
import Test.Tendril qualified

main :: IO ()
main = defaultMain Test.Tendril.tests
