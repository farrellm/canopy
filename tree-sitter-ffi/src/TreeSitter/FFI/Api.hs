{-# LANGUAGE CApiFFI #-}

-- | Imports of the @api.h@ functions that take only scalars and pointers
-- (DESIGN.md §5A.2). Each import records why it is @safe@ or @unsafe@ (§5A.3).
module TreeSitter.FFI.Api
  ( tsParserNew,
    tsParserDelete,
  )
where

import Foreign.Ptr (Ptr)
import TreeSitter.FFI.Types (TSParser)

-- | @ts_parser_new@. @unsafe@: short, and calls no callback.
foreign import capi unsafe "tree_sitter/api.h ts_parser_new"
  tsParserNew :: IO (Ptr TSParser)

-- | @ts_parser_delete@. @unsafe@: short, and calls no callback.
foreign import capi unsafe "tree_sitter/api.h ts_parser_delete"
  tsParserDelete :: Ptr TSParser -> IO ()
