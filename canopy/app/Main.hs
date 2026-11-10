-- | The @canopy@ executable. Until M3 it prints its components' versions.
module Main (main) where

import Canopy (components)
import Effectful (runEff)

main :: IO ()
main = do
  cs <- runEff components
  for_ cs $ \(name, v) -> putTextLn (name <> " " <> v)
