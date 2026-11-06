-- | @tendril-demo@: tendril as a standalone readline (DESIGN.md §5.1). A stub
-- until M2.
module Main (main) where

import Tendril qualified

main :: IO ()
main = putTextLn ("tendril-demo " <> Tendril.version <> ": the editor arrives in M2")
