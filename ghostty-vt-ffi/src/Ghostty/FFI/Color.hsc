{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | @include/ghostty/vt/color.h@, the part terminals and styles need: an RGB
-- colour and a palette index. Colour parsing and palette generation arrive
-- with M1.3.
module Ghostty.FFI.Color
  ( -- * Colours
    GhosttyColorRgb (..),
    ghosttyColorRgbLayout,
    GhosttyColorPaletteIndex,
  )
where

import Foreign.Storable (Storable (..))
import Ghostty.FFI.Layout (CField (..), CStruct (..))

#include <gvtffi_shims.h>

-- | @GhosttyColorRgb@: RGB color value.
data GhosttyColorRgb = GhosttyColorRgb
  { -- | @r@: Red component (0-255)
    r :: Word8,
    -- | @g@: Green component (0-255)
    g :: Word8,
    -- | @b@: Blue component (0-255)
    b :: Word8
  }
  deriving stock (Eq, Show)

instance Storable GhosttyColorRgb where
  sizeOf _ = (#size GhosttyColorRgb)
  alignment _ = (#alignment GhosttyColorRgb)
  peek p =
    GhosttyColorRgb
      <$> (#peek GhosttyColorRgb, r) p
      <*> (#peek GhosttyColorRgb, g) p
      <*> (#peek GhosttyColorRgb, b) p
  poke p s = do
    (#poke GhosttyColorRgb, r) p s.r
    (#poke GhosttyColorRgb, g) p s.g
    (#poke GhosttyColorRgb, b) p s.b

-- | The layout of @GhosttyColorRgb@ as compiled, for the layout test (§3.4).
ghosttyColorRgbLayout :: CStruct
ghosttyColorRgbLayout =
  CStruct
    { name = "GhosttyColorRgb",
      size = (#size GhosttyColorRgb),
      align = (#alignment GhosttyColorRgb),
      fields =
        [ CField "r" (#offset GhosttyColorRgb, r) (#const GVTFFI_FIELD_SIZE(GhosttyColorRgb, r)),
          CField "g" (#offset GhosttyColorRgb, g) (#const GVTFFI_FIELD_SIZE(GhosttyColorRgb, g)),
          CField "b" (#offset GhosttyColorRgb, b) (#const GVTFFI_FIELD_SIZE(GhosttyColorRgb, b))
        ]
    }

-- | @GhosttyColorPaletteIndex@: an index into the 256-colour palette.
type GhosttyColorPaletteIndex = Word8
