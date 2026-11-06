{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | @include/ghostty/vt/osc.h@, the part the terminal's callbacks need: how an
-- OSC sequence ended. The OSC parser itself arrives with M1.3.
module Ghostty.FFI.Osc
  ( -- * Terminators
    GhosttyOscTerminator (..),
    pattern GhosttyOscTerminatorSt,
    pattern GhosttyOscTerminatorBel,
    pattern GhosttyOscTerminatorUnknown,
    ghosttyOscTerminatorEnum,
  )
where

import Foreign.C.Types (CInt (..))
import Foreign.Storable (Storable (..))
import Ghostty.FFI.Layout (CEnum (..), unnamed)

#include <gvtffi_shims.h>

-- | @GhosttyOscTerminator@: How an OSC sequence was ended.
newtype GhosttyOscTerminator = GhosttyOscTerminator CInt
  deriving stock (Eq, Show)
  deriving newtype (Storable)

-- | @GHOSTTY_OSC_TERMINATOR_ST@: The string terminator (ST): ESC followed by a backslash (0x1B 0x5C).
pattern GhosttyOscTerminatorSt :: GhosttyOscTerminator
pattern GhosttyOscTerminatorSt = GhosttyOscTerminator (#const GHOSTTY_OSC_TERMINATOR_ST)

-- | @GHOSTTY_OSC_TERMINATOR_BEL@: The bell character, BEL (byte 0x07).
pattern GhosttyOscTerminatorBel :: GhosttyOscTerminator
pattern GhosttyOscTerminatorBel = GhosttyOscTerminator (#const GHOSTTY_OSC_TERMINATOR_BEL)

-- | Any value this binding does not name: a newer library's.
pattern GhosttyOscTerminatorUnknown :: CInt -> GhosttyOscTerminator
pattern GhosttyOscTerminatorUnknown n <- GhosttyOscTerminator (unnamed ghosttyOscTerminatorEnum -> Just n)
  where
    GhosttyOscTerminatorUnknown n = GhosttyOscTerminator n

{-# COMPLETE
  GhosttyOscTerminatorSt,
  GhosttyOscTerminatorBel,
  GhosttyOscTerminatorUnknown
  #-}

-- | Every value of @GhosttyOscTerminator@ this binding names, for the layout test (§3.4).
-- It is also what 'GhosttyOscTerminatorUnknown' excludes.
ghosttyOscTerminatorEnum :: CEnum
ghosttyOscTerminatorEnum =
  CEnum
    { name = "GhosttyOscTerminator",
      size = (#size GhosttyOscTerminator),
      values =
        [ ("GHOSTTY_OSC_TERMINATOR_ST", (#const GHOSTTY_OSC_TERMINATOR_ST)),
          ("GHOSTTY_OSC_TERMINATOR_BEL", (#const GHOSTTY_OSC_TERMINATOR_BEL))
        ]
    }

