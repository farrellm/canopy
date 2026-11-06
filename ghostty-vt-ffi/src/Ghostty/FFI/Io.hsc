{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | @include/ghostty/vt/io.h@, the part the terminal needs: the writer that
-- @ghostty_terminal_continuation_write@ calls. Readers arrive with the step
-- that first needs one.
module Ghostty.FFI.Io
  ( -- * Writers
    GhosttyWriter (..),
    ghosttyWriterLayout,
    GhosttyWriterFn,
    mkGhosttyWriterFn,
  )
where

import Foreign.C.Types (CBool (..), CSize (..))
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (Storable (..))
import Ghostty.FFI.Layout (CField (..), CStruct (..))

#include <gvtffi_shims.h>

-- | @GhosttyWriterFn@: called with each chunk of output, which is borrowed
-- for the call. Returns false to fail the write (the caller then reports
-- @GHOSTTY_IO_ERROR@).
type GhosttyWriterFn = FunPtr (Ptr () -> Ptr Word8 -> CSize -> IO CBool)

-- | Make a 'GhosttyWriterFn' from a Haskell function. Free it with
-- 'Foreign.Ptr.freeHaskellFunPtr' once nothing can call it. A call that runs
-- it must be @safe@ (§3.3).
foreign import ccall "wrapper"
  mkGhosttyWriterFn :: (Ptr () -> Ptr Word8 -> CSize -> IO CBool) -> IO GhosttyWriterFn

-- | @GhosttyWriter@: A byte destination callback and its opaque context.
data GhosttyWriter = GhosttyWriter
  { -- | @write@.
    write :: GhosttyWriterFn,
    -- | @userdata@.
    userdata :: Ptr ()
  }
  deriving stock (Eq, Show)

instance Storable GhosttyWriter where
  sizeOf _ = (#size GhosttyWriter)
  alignment _ = (#alignment GhosttyWriter)
  peek p =
    GhosttyWriter
      <$> (#peek GhosttyWriter, write) p
      <*> (#peek GhosttyWriter, userdata) p
  poke p s = do
    (#poke GhosttyWriter, write) p s.write
    (#poke GhosttyWriter, userdata) p s.userdata

-- | The layout of @GhosttyWriter@ as compiled, for the layout test (§3.4).
ghosttyWriterLayout :: CStruct
ghosttyWriterLayout =
  CStruct
    { name = "GhosttyWriter",
      size = (#size GhosttyWriter),
      align = (#alignment GhosttyWriter),
      fields =
        [ CField "write" (#offset GhosttyWriter, write) (#const GVTFFI_FIELD_SIZE(GhosttyWriter, write)),
          CField "userdata" (#offset GhosttyWriter, userdata) (#const GVTFFI_FIELD_SIZE(GhosttyWriter, userdata))
        ]
    }

