-- | A terminal's sizes (DESIGN.md §4.2): its grid in cells, never zero
-- along either axis, and one cell in pixels. Here rather than in
-- "Ghostty.Effectful.Internal.Terminal" because events carry them too
-- ("Ghostty.Effectful.Event"), and the terminal's representation holds its
-- events.
module Ghostty.Effectful.Internal.Size
  ( Extent (..),
    extent,
    fromExtent,
    GridSize (..),
    CellPixels (..),
    noCellPixels,
  )
where

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
