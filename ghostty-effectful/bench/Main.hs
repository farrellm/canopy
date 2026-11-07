module Main (main) where

import Bench.Ghostty.Effectful.Feed qualified
import Test.Tasty.Bench (bgroup, defaultMain)

main :: IO ()
main = defaultMain [bgroup "ghostty-effectful" [Bench.Ghostty.Effectful.Feed.benchmarks]]
