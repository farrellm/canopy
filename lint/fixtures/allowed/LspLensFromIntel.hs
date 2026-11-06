-- lint/ has no .cabal file, so ormolu does not know GHC2024 is on here.
{-# LANGUAGE ImportQualifiedPost #-}

-- | Fixture: rule 6, qualified as L in an Intel module.
module Canopy.Intel.Complete () where

import Language.LSP.Protocol.Lens qualified as L
