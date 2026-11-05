-- lint/ has no .cabal file, so ormolu does not know GHC2024 is on here.
{-# LANGUAGE ImportQualifiedPost #-}

-- | Fixture: rule 6, lsp-types lenses only in the LSP modules.
module Canopy.App.View () where

import Language.LSP.Protocol.Lens qualified as L
