{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | @include/ghostty/vt/grid_ref.h@ and @grid_ref_tracked.h@: references to a
-- cell of a terminal's grid.
--
-- An untracked 'GhosttyGridRef' is a snapshot, valid until the next call that
-- mutates its terminal. A 'GhosttyTrackedGridRef' is owned, follows its cell
-- as the terminal scrolls, prunes and reflows, and must be freed. Both are
-- obtained from a terminal: see "Ghostty.FFI.Terminal".
module Ghostty.FFI.GridRef
  ( -- * Untracked references
    GhosttyGridRef (..),
    ghosttyGridRefLayout,
    ghosttyGridRefCell,
    ghosttyGridRefRow,
    ghosttyGridRefGraphemes,
    ghosttyGridRefHyperlinkUri,
    ghosttyGridRefStyle,

    -- * Tracked references
    GhosttyTrackedGridRef (..),
    GhosttyTrackedGridRefImpl,
    ghosttyTrackedGridRefFree,
    ghosttyTrackedGridRefHasValue,
    ghosttyTrackedGridRefPoint,
    ghosttyTrackedGridRefSet,
    ghosttyTrackedGridRefSnapshot,
  )
where

import Foreign.C.Types (CBool (..), CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import Ghostty.FFI.Layout (CField (..), CStruct (..))
import Ghostty.FFI.Point (GhosttyPoint, GhosttyPointCoordinate, GhosttyPointTag (..))
import Ghostty.FFI.Screen (GhosttyCell, GhosttyRow)
import Ghostty.FFI.Style (GhosttyStyle)
import Ghostty.FFI.Terminal.Handle (GhosttyTerminal (..))
import Ghostty.FFI.Types (GhosttyResult (..))

#include <gvtffi_shims.h>

-- | @GhosttyGridRef@: A resolved reference to a terminal cell position.
data GhosttyGridRef = GhosttyGridRef
  { -- | @size@.
    size :: CSize,
    -- | @node@.
    node :: Ptr (),
    -- | @x@.
    x :: Word16,
    -- | @y@.
    y :: Word16
  }
  deriving stock (Eq, Show)

instance Storable GhosttyGridRef where
  sizeOf _ = (#size GhosttyGridRef)
  alignment _ = (#alignment GhosttyGridRef)
  peek p =
    GhosttyGridRef
      <$> (#peek GhosttyGridRef, size) p
      <*> (#peek GhosttyGridRef, node) p
      <*> (#peek GhosttyGridRef, x) p
      <*> (#peek GhosttyGridRef, y) p
  poke p s = do
    (#poke GhosttyGridRef, size) p s.size
    (#poke GhosttyGridRef, node) p s.node
    (#poke GhosttyGridRef, x) p s.x
    (#poke GhosttyGridRef, y) p s.y

-- | The layout of @GhosttyGridRef@ as compiled, for the layout test (§3.4).
ghosttyGridRefLayout :: CStruct
ghosttyGridRefLayout =
  CStruct
    { name = "GhosttyGridRef",
      size = (#size GhosttyGridRef),
      align = (#alignment GhosttyGridRef),
      fields =
        [ CField "size" (#offset GhosttyGridRef, size) (#const GVTFFI_FIELD_SIZE(GhosttyGridRef, size)),
          CField "node" (#offset GhosttyGridRef, node) (#const GVTFFI_FIELD_SIZE(GhosttyGridRef, node)),
          CField "x" (#offset GhosttyGridRef, x) (#const GVTFFI_FIELD_SIZE(GhosttyGridRef, x)),
          CField "y" (#offset GhosttyGridRef, y) (#const GVTFFI_FIELD_SIZE(GhosttyGridRef, y))
        ]
    }

-- | @ghostty_grid_ref_cell@: the cell at the reference. @unsafe@: it does not
-- call back into Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_grid_ref_cell"
  ghosttyGridRefCell :: Ptr GhosttyGridRef -> Ptr GhosttyCell -> IO GhosttyResult

-- | @ghostty_grid_ref_row@: the row the reference is in. @unsafe@: it does not
-- call back into Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_grid_ref_row"
  ghosttyGridRefRow :: Ptr GhosttyGridRef -> Ptr GhosttyRow -> IO GhosttyResult

-- | @ghostty_grid_ref_graphemes@: the cell's grapheme cluster as codepoints,
-- into a buffer of the given length. @GHOSTTY_OUT_OF_SPACE@ reports the length
-- needed in the out count. @unsafe@: it does not call back into Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_grid_ref_graphemes"
  ghosttyGridRefGraphemes :: Ptr GhosttyGridRef -> Ptr Word32 -> CSize -> Ptr CSize -> IO GhosttyResult

-- | @ghostty_grid_ref_hyperlink_uri@: the cell's hyperlink URI, into a buffer,
-- with the same out-of-space protocol as 'ghosttyGridRefGraphemes'. @unsafe@:
-- it does not call back into Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_grid_ref_hyperlink_uri"
  ghosttyGridRefHyperlinkUri :: Ptr GhosttyGridRef -> Ptr Word8 -> CSize -> Ptr CSize -> IO GhosttyResult

-- | @ghostty_grid_ref_style@: the cell's style. @unsafe@: it does not call back
-- into Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_grid_ref_style"
  ghosttyGridRefStyle :: Ptr GhosttyGridRef -> Ptr GhosttyStyle -> IO GhosttyResult

-- | The C side of a 'GhosttyTrackedGridRef'. Never dereferenced.
data GhosttyTrackedGridRefImpl

-- | @GhosttyTrackedGridRef@: an owned reference that follows its cell. Free it
-- with 'ghosttyTrackedGridRefFree'; it may outlive its terminal, after which
-- it has no value.
newtype GhosttyTrackedGridRef = GhosttyTrackedGridRef (Ptr GhosttyTrackedGridRefImpl)
  deriving stock (Eq, Show)
  deriving newtype (Storable)

-- | @ghostty_tracked_grid_ref_free@ (null is a no-op). @unsafe@: it does not
-- call back into Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_tracked_grid_ref_free"
  ghosttyTrackedGridRefFree :: GhosttyTrackedGridRef -> IO ()

-- | @ghostty_tracked_grid_ref_has_value@: whether the reference still names a
-- cell. @unsafe@: it does not call back into Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_tracked_grid_ref_has_value"
  ghosttyTrackedGridRefHasValue :: GhosttyTrackedGridRef -> IO CBool

-- | @ghostty_tracked_grid_ref_point@: the reference's position in a coordinate
-- system, or @GHOSTTY_NO_VALUE@ if it has none there. @unsafe@: it does not
-- call back into Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_tracked_grid_ref_point"
  ghosttyTrackedGridRefPoint :: GhosttyTrackedGridRef -> GhosttyPointTag -> Ptr GhosttyPointCoordinate -> IO GhosttyResult

-- | @ghostty_tracked_grid_ref_set@: move the reference to a point of the
-- terminal that created it. The C function takes the point by value, so this
-- calls a pointer shim (@gvtffi_shims.h@, §3.3). @unsafe@: it does not call
-- back into Haskell.
foreign import capi unsafe "gvtffi_shims.h gvtffi_tracked_grid_ref_set"
  ghosttyTrackedGridRefSet :: GhosttyTrackedGridRef -> GhosttyTerminal -> Ptr GhosttyPoint -> IO GhosttyResult

-- | @ghostty_tracked_grid_ref_snapshot@: an untracked snapshot of the
-- reference, or @GHOSTTY_NO_VALUE@ if it has lost its cell. @unsafe@: it does
-- not call back into Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_tracked_grid_ref_snapshot"
  ghosttyTrackedGridRefSnapshot :: GhosttyTrackedGridRef -> Ptr GhosttyGridRef -> IO GhosttyResult
