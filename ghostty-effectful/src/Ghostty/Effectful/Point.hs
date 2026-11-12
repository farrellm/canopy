-- | Positions in a terminal's grid (DESIGN.md §4.4). A point's coordinate
-- system is in its type, so a viewport row cannot be passed where an
-- active-area row is meant.
module Ghostty.Effectful.Point
  ( Coord (..),
    Point (..),
    SCoord (..),
    KnownCoord (..),
  )
where

-- | A coordinate system of a terminal's grid. Rows count from 0 at the top
-- of each; columns from 0 at the left.
data Coord
  = -- | The active area: the rows the program writes to, as many as the
    -- terminal has.
    Active
  | -- | The rows scrolled into view.
    Viewport
  | -- | The whole screen: scrollback, then the active area.
    Grid
  | -- | The scrollback alone.
    History
  deriving stock (Eq, Show)

-- | A cell's position in the coordinate system @c@. Its fields are strict
-- and unpacked (DESIGN.md §10.7, §12.3): a point is always read whole from
-- the library, and a stream of prompt marks keeps one per event alive for a
-- whole 'Ghostty.Effectful.Terminal.feed'.
data Point (c :: Coord) = Point
  { -- | the column
    col :: {-# UNPACK #-} !Word16,
    -- | the row
    row :: {-# UNPACK #-} !Word32
  }
  deriving stock (Eq, Ord, Show)

-- | The coordinate system @c@, as a value.
data SCoord (c :: Coord) where
  -- | 'Active'
  SActive :: SCoord 'Active
  -- | 'Viewport'
  SViewport :: SCoord 'Viewport
  -- | 'Grid'
  SGrid :: SCoord 'Grid
  -- | 'History'
  SHistory :: SCoord 'History

-- | A coordinate system known at compile time.
class KnownCoord (c :: Coord) where
  -- | its value
  coordSing :: SCoord c

instance KnownCoord 'Active where
  coordSing = SActive

instance KnownCoord 'Viewport where
  coordSing = SViewport

instance KnownCoord 'Grid where
  coordSing = SGrid

instance KnownCoord 'History where
  coordSing = SHistory
