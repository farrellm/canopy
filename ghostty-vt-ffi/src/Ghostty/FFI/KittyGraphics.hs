-- | @include/ghostty/vt/kitty_graphics.h@, the part a terminal's data keys
-- need: the handle to a terminal's Kitty graphics storage, which
-- @GHOSTTY_TERMINAL_DATA_KITTY_GRAPHICS@ returns (@types.h@ declares it). Its images, placements and
-- their queries arrive with PLAN.md's M6.15.
module Ghostty.FFI.KittyGraphics
  ( -- * Handles
    GhosttyKittyGraphics (..),
    GhosttyKittyGraphicsImpl,
  )
where

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)

-- | The C side of a 'GhosttyKittyGraphics'. Never dereferenced.
data GhosttyKittyGraphicsImpl

-- | @GhosttyKittyGraphics@: the Kitty graphics storage of a terminal's active
-- screen, borrowed from the terminal: never freed on its own, and valid until
-- the terminal's next mutating call (a write, a reset).
newtype GhosttyKittyGraphics = GhosttyKittyGraphics (Ptr GhosttyKittyGraphicsImpl)
  deriving stock (Eq, Show)
  deriving newtype (Storable)
