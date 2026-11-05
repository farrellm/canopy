-- | A readline-style line editor (DESIGN.md §5).
--
-- M0 skeleton: the package exists and is wired; the editor arrives in M2.
module Tendril (version) where

import Data.Version (showVersion)
import Paths_tendril qualified

-- | This package's version.
--
-- >>> version
-- "0.1.0.0"
version :: Text
version = toText (showVersion Paths_tendril.version)
