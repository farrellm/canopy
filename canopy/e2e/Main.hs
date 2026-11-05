-- | @canopy-e2e@ (DESIGN.md §11.5): real REPLs through the real PTY, with
-- canopy's host side attached to a headless libghostty terminal. Nightly
-- only. Empty until M3.
module Main (main) where

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "canopy-e2e" [])
