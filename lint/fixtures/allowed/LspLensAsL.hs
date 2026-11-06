-- lint/ has no .cabal file, so ormolu does not know GHC2024 is on here.
{-# LANGUAGE ImportQualifiedPost #-}

-- | Fixture: rule 6, qualified as L in an LSP module.
module Canopy.Lsp.Client () where

import Language.LSP.Protocol.Lens qualified as L
