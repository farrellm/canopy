{-# LANGUAGE CApiFFI #-}

-- | @include/ghostty/vt/allocator.h@: the allocator every constructor takes,
-- and the library's own @alloc@/@free@.
--
-- The binding never supplies an allocator: every function that takes one is
-- passed 'Foreign.Ptr.nullPtr', the library's default. A Haskell allocator
-- would run Haskell inside every allocation the library makes, which §4.1's
-- third goal rules out. So 'GhosttyAllocator' is not marshalled.
module Ghostty.FFI.Allocator
  ( -- * Allocators
    GhosttyAllocator,

    -- * Calls
    ghosttyAlloc,
    ghosttyFree,
  )
where

import Foreign.C.Types (CSize (..))
import Foreign.Ptr (Ptr)

-- | @GhosttyAllocator@: a custom allocator. Never constructed by this binding,
-- only pointed to, and always by a null pointer.
data GhosttyAllocator

-- | @ghostty_alloc@: allocate @len@ bytes with the allocator (null for the
-- default). Returns null on failure. @unsafe@: it does not call back into
-- Haskell, since the allocator is never a Haskell one.
foreign import capi unsafe "ghostty/vt.h ghostty_alloc"
  ghosttyAlloc :: Ptr GhosttyAllocator -> CSize -> IO (Ptr Word8)

-- | @ghostty_free@: free memory the library allocated, such as
-- @ghostty_terminal_continuation_alloc@'s output, with the same allocator and
-- length. @unsafe@: as 'ghosttyAlloc'.
foreign import capi unsafe "ghostty/vt.h ghostty_free"
  ghosttyFree :: Ptr GhosttyAllocator -> Ptr Word8 -> CSize -> IO ()
