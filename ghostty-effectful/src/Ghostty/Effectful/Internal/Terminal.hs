-- | A terminal's representation and its raw calls (DESIGN.md §4.2). The
-- public API is "Ghostty.Effectful.Terminal", which exports these types
-- without their constructors; the calls here are what the 'Ghostty'
-- interpreter runs, after its owner-thread check.
module Ghostty.Effectful.Internal.Terminal
  ( -- * Handles
    Terminal (..),

    -- * Sizes
    Extent (..),
    extent,
    fromExtent,
    GridSize (..),
    CellPixels (..),
    noCellPixels,

    -- * Configuration
    TerminalConfig (..),
    terminalConfig,

    -- * Calls
    newTerminal,
    freeTerminal,
    resizeTerminal,
    resetTerminal,
  )
where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Ghostty.Effectful.Error (CallSite (..))
import Ghostty.Effectful.Internal.Error (expectSuccess)
import Ghostty.FFI.Terminal
  ( GhosttyTerminal,
    ghosttyTerminalFree,
    ghosttyTerminalNew,
    ghosttyTerminalReset,
    ghosttyTerminalResize,
  )

-- | A terminal, alive in the region @s@ that
-- 'Ghostty.Effectful.Terminal.withTerminal' opens. The type variable is
-- rank-2 there, as in @ST@, so a 'Terminal' cannot be used after its region
-- has freed it.
newtype Terminal s = Terminal GhosttyTerminal

-- | A number of cells along one axis: never zero, which the library refuses
-- for a terminal's columns or rows.
newtype Extent = Extent Word16
  deriving stock (Eq, Ord, Show)

-- | An extent of @n@ cells, if @n@ is not zero.
--
-- >>> extent 80
-- Just (Extent 80)
-- >>> extent 0
-- Nothing
extent :: Word16 -> Maybe Extent
extent n = if n == 0 then Nothing else Just (Extent n)

-- | The number of cells.
fromExtent :: Extent -> Word16
fromExtent (Extent n) = n

-- | A terminal's grid, in cells.
data GridSize = GridSize
  { -- | columns
    cols :: Extent,
    -- | rows
    rows :: Extent
  }
  deriving stock (Eq, Show)

-- | One cell's size in pixels, which image protocols and size reports use.
-- Zero means unknown.
data CellPixels = CellPixels
  { -- | width
    width :: Word32,
    -- | height
    height :: Word32
  }
  deriving stock (Eq, Show)

-- | A cell size of zero by zero: unknown, the library's own starting value.
noCellPixels :: CellPixels
noCellPixels = CellPixels 0 0

-- | How a terminal starts.
data TerminalConfig = TerminalConfig
  { -- | the grid
    size :: GridSize,
    -- | one cell, in pixels
    cellPixels :: CellPixels
  }
  deriving stock (Eq, Show)

-- | A terminal of the given grid, with the cell size unknown.
terminalConfig :: GridSize -> TerminalConfig
terminalConfig g = TerminalConfig {size = g, cellPixels = noCellPixels}

-- | @ghostty_terminal_new@ with the default allocator (§3.3), at the
-- configured grid. The cell size is the caller's to set, inside the scope
-- that frees the terminal on every path.
newTerminal :: TerminalConfig -> IO GhosttyTerminal
newTerminal cfg = alloca $ \out -> do
  r <- ghosttyTerminalNew nullPtr out (fromExtent cfg.size.cols) (fromExtent cfg.size.rows)
  expectSuccess (CallSite "ghostty_terminal_new") r
  peek out

-- | @ghostty_terminal_free@.
freeTerminal :: GhosttyTerminal -> IO ()
freeTerminal = ghosttyTerminalFree

-- | @ghostty_terminal_resize@. Its one documented refusal, a zero extent or a
-- null terminal, cannot be expressed, so only running out of memory remains.
resizeTerminal :: GhosttyTerminal -> GridSize -> CellPixels -> IO ()
resizeTerminal h g px =
  expectSuccess (CallSite "ghostty_terminal_resize")
    =<< ghosttyTerminalResize h (fromExtent g.cols) (fromExtent g.rows) px.width px.height

-- | @ghostty_terminal_reset@: a full reset (RIS), keeping the size.
resetTerminal :: GhosttyTerminal -> IO ()
resetTerminal = ghosttyTerminalReset
