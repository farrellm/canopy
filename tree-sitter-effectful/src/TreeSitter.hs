-- | The high-level interface to tree-sitter (DESIGN.md §5B).
--
-- M0 skeleton: the supported grammar ABI range, read through the raw binding,
-- so the package edge to @tree-sitter-ffi@ is wired.
module TreeSitter (supportedAbi) where

import TreeSitter.FFI.Types (languageVersion, minCompatibleLanguageVersion)

-- | The grammar ABI versions the runtime loads, oldest and newest. A grammar
-- outside this range is a load error (§5B.4).
--
-- >>> supportedAbi
-- (13,15)
supportedAbi :: (Int, Int)
supportedAbi = (minCompatibleLanguageVersion, languageVersion)
