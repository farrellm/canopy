module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tendril qualified
import Test.Tendril.Text.Unicode qualified

main :: IO ()
main =
  defaultMain $
    testGroup "tendril-test" [Test.Tendril.tests, Test.Tendril.Text.Unicode.tests]
