module Main (main) where

import Bench.Tendril.Edit qualified
import Test.Tasty.Bench (bgroup, defaultMain)

main :: IO ()
main = defaultMain [bgroup "tendril" [Bench.Tendril.Edit.benchmarks]]
