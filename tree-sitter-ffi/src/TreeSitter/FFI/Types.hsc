-- | tree-sitter's types and constants, from the vendored @api.h@ through
-- @hsc2hs@ (DESIGN.md §5A.2).
module TreeSitter.FFI.Types
  ( -- * Language ABI
    languageVersion,
    minCompatibleLanguageVersion,

    -- * Handles
    TSParser,

    -- * By-value structs
    TSNode,
    sizeOfTSNode,
  )
where

#include <tree_sitter/api.h>

-- | @TREE_SITTER_LANGUAGE_VERSION@: the newest grammar ABI the runtime loads.
languageVersion :: Int
languageVersion = #const TREE_SITTER_LANGUAGE_VERSION

-- | @TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION@: the oldest grammar ABI the
-- runtime loads.
minCompatibleLanguageVersion :: Int
minCompatibleLanguageVersion = #const TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION

-- | @TSParser@, only ever behind a pointer.
data TSParser

-- | @TSNode@, a by-value struct; Haskell holds it in memory it allocates
-- ('sizeOfTSNode' bytes) and passes a pointer to a shim.
data TSNode

-- | @sizeof(TSNode)@.
sizeOfTSNode :: Int
sizeOfTSNode = #size TSNode
