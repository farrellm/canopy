-- | The @canopy@ executable. Until M3 it prints its components' versions.
module Main (main) where

import Canopy (components)
import Effectful (runEff)

main :: IO ()
main = runEff components >>= mapM_ (\(name, v) -> putTextLn (name <> " " <> v))
