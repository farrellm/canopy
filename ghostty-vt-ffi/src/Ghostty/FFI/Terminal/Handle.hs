-- | The terminal handle, apart from the rest of @terminal.h@ so that the
-- modules a terminal's calls use ("Ghostty.FFI.GridRef") can name it without
-- a cycle.
module Ghostty.FFI.Terminal.Handle
  ( GhosttyTerminal (..),
    GhosttyTerminalImpl,
  )
where

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)

-- | The C side of a 'GhosttyTerminal'. Never dereferenced.
data GhosttyTerminalImpl

-- | @GhosttyTerminal@: a terminal emulator instance, from
-- @ghostty_terminal_new@ and released by @ghostty_terminal_free@. Not
-- thread-safe: every call on one terminal must come from one thread (§4.6).
newtype GhosttyTerminal = GhosttyTerminal (Ptr GhosttyTerminalImpl)
  deriving stock (Eq, Show)
  deriving newtype (Storable)
