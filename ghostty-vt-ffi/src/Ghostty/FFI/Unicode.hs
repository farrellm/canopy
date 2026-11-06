{-# LANGUAGE CApiFFI #-}

-- | @include/ghostty/vt/unicode.h@: the terminal's own cell widths, of a
-- codepoint and of a grapheme cluster, for predicting where text will land
-- before it is written. These are the tables tendril's widths are tested
-- against (DESIGN.md §11.2).
module Ghostty.FFI.Unicode
  ( -- * Widths
    ghosttyUnicodeCodepointWidth,
    ghosttyUnicodeGraphemeWidth,
  )
where

import Foreign.C.Types (CSize (..))
import Foreign.Ptr (Ptr)

-- | @ghostty_unicode_codepoint_width@: the cells a codepoint takes, 0, 1 or 2,
-- from the table the terminal lays out printed text with. Total: a value
-- past U+10FFFF is 1. Summing these is right only with grapheme clustering
-- (mode 2027) off. Not in 'IO', since the header documents it as pure,
-- allocation-free and thread-safe. @unsafe@: it does not call back into
-- Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_unicode_codepoint_width"
  ghosttyUnicodeCodepointWidth :: Word32 -> Word8

-- | @ghostty_unicode_grapheme_width@: the codepoints in the first grapheme
-- cluster of a sequence, as the terminal segments it with mode 2027 on, and
-- through the out pointer (which may be null) the cluster's width in cells.
-- 0 exactly when the length is 0. Not streaming: a cluster that reaches the
-- end of the input may still grow. @unsafe@: it does not call back into
-- Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_unicode_grapheme_width"
  ghosttyUnicodeGraphemeWidth :: Ptr Word32 -> CSize -> Ptr Word8 -> IO CSize
