-- | Canopy, the application (DESIGN.md §6–§9).
--
-- M0 skeleton: the version line, which reads one fact from each component so
-- that every package edge of §1.3 is wired.
module Canopy
  ( version,
    components,
  )
where

import Data.Text qualified as T
import Data.Version (showVersion)
import Effectful (Eff, IOE, (:>))
import Ghostty.Effectful (buildMetadata)
import Paths_canopy qualified
import Tendril qualified
import TreeSitter (supportedAbi)

-- | This package's version.
version :: Text
version = toText (showVersion Paths_canopy.version)

-- | Each component and its version: canopy, tendril, the tree-sitter grammar
-- ABI range, and the libghostty-vt build.
components :: (IOE :> es) => Eff es [(Text, Text)]
components = do
  ghostty <- buildMetadata
  let (lo, hi) = supportedAbi
  pure
    [ ("canopy", version),
      ("tendril", Tendril.version),
      ("tree-sitter ABI", T.intercalate ".." [show lo, show hi]),
      ("libghostty-vt", fromMaybe "unknown" ghostty)
    ]
