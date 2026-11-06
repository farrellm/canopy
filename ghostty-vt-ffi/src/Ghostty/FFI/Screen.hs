-- | @include/ghostty/vt/screen.h@, the part grid references need: the cell and
-- row handles. Reading them (@ghostty_cell_get@, @ghostty_row_get@) arrives
-- with M1.3's render state, which is where cells are read in bulk.
module Ghostty.FFI.Screen
  ( -- * Handles
    GhosttyCell,
    GhosttyRow,
  )
where

-- | @GhosttyCell@: an opaque packed cell value, as @ghostty_grid_ref_cell@
-- returns it.
type GhosttyCell = Word64

-- | @GhosttyRow@: an opaque packed row value, as @ghostty_grid_ref_row@
-- returns it.
type GhosttyRow = Word64
