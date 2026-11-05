module Main (main) where

import Test.Canopy qualified
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain Test.Canopy.tests
