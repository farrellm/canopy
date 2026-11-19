{-# LANGUAGE StrictData #-}

-- | A laid-out screen region (DESIGN.md §5.6): rows of cells at one width, and
-- where the cursor is. "Tendril.Render.Layout" makes one from a
-- "Tendril.Render.Doc", "Tendril.Term.Output" draws one on the host, and the
-- diff between two is what a frame sends.
--
-- A 'Grid' is abstract, and 'grid' is the only way to make one, so every grid
-- holds what the host would show if sent its cells in order: each row is as
-- wide as the grid; a wide glyph is followed by its tail and a tail follows a
-- wide glyph; a glyph is one piece of text as the host measures it in the
-- grid's 'WidthMode' ("Tendril.Text.Width.pieces"), as wide as its cell says,
-- with no control character in it, and not joined by the host to the glyph
-- before it. So no grid can make the host's cursor
-- drift, or send it an escape sequence in a cell's text.
module Tendril.Render.Grid
  ( -- * Grids
    Grid,
    grid,
    emptyGrid,
    rowText,

    -- * Cells
    Cell (..),
    Glyph (..),
    blank,

    -- * Positions
    GridPos (..),
  )
where

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import GHC.Records (HasField (..))
import Tendril.Render.Style (Style, plain)
import Tendril.Text.Unicode.Tables (GraphemeBreak (..), emojiModifierBase, emojiVariationBase, extendedPictographic, graphemeBreak)
import Tendril.Text.Units (Offset, Unit (..), fromOffset, offset)
import Tendril.Text.Width (WidthMode (..), peakWidth, pieces)

-- | What a cell shows.
data Glyph
  = -- | Nothing: a space, in the cell's style. A 'grid' has no 'Narrow'
    -- space, only this.
    Blank
  | -- | A piece of text one cell wide.
    Narrow Text
  | -- | A piece of text two cells wide, in this cell and the next.
    Wide Text
  | -- | The second cell of the 'Wide' glyph before it.
    WideTail
  deriving stock (Eq, Ord, Show)

-- | One cell.
data Cell = Cell
  { -- | What it shows.
    glyph :: Glyph,
    -- | How it looks.
    style :: Style
  }
  deriving stock (Eq, Ord, Show)

-- | A blank cell in a style.
blank :: Style -> Cell
blank = Cell Blank

-- | A cell of a grid: its row from the top, and its column.
data GridPos = GridPos
  { -- | The row, from 0.
    row :: Word,
    -- | The column, from 0.
    col :: Offset 'Column
  }
  deriving stock (Eq, Ord, Show)

-- | Rows of cells at one width, and the cursor. Read it as @g.width@,
-- @g.mode@, @g.rows@ and @g.cursor@.
data Grid = Grid WidthMode (Offset 'Column) (Seq (Seq Cell)) (Maybe GridPos)
  deriving stock (Eq, Show)

instance HasField "mode" Grid WidthMode where
  getField (Grid m _ _ _) = m

-- | The grid's width, in cells; at least 1.
instance HasField "width" Grid (Offset 'Column) where
  getField (Grid _ w _ _) = w

-- | The rows, top to bottom, each exactly 'width' cells.
instance HasField "rows" Grid (Seq (Seq Cell)) where
  getField (Grid _ _ rs _) = rs

-- | Where the cursor is, if it is shown: always a cell of the grid.
instance HasField "cursor" Grid (Maybe GridPos) where
  getField (Grid _ _ _ c) = c

-- | A grid of these rows and cursor, for a host that measures text in the
-- mode, at the width (1 if it is 0). A glyph's text is what the host keeps
-- of it: a variation selector the host ignores is dropped, and so, one column
-- wide in grapheme-cluster mode, is everything after a cluster's first code
-- point. Whatever would break the grid's
-- guarantees is made blank, keeping its style: a glyph the host would draw
-- otherwise than its cell says (a control character in it, two pieces of
-- text, a width that is not the cell's, a join to the cell before it, a
-- wide character made narrow in the last column, which the host wraps), a
-- wide glyph whose tail is missing
-- or past the width, and a tail with no wide glyph before it. A space is
-- blank too, since the host draws them alike. A short row is
-- filled with blank cells and a long one is cut. A cursor outside the grid is
-- none.
grid :: WidthMode -> Offset 'Column -> [[Cell]] -> Maybe GridPos -> Grid
grid mode w0 rows cursor = Grid mode w (Seq.fromList (map fitRow rows)) (cursor >>= inside)
  where
    w = offset (max 1 (fromOffset w0))
    width = fromOffset w
    height = fromIntegral (length rows)
    inside p = p <$ guard (p.row < height && p.col < w)
    fitRow cs = Seq.fromList (take (fromIntegral width) (go 0 Nothing cs <> repeat (blank plain)))
    -- The text before a cell, as the host has it: the glyph before it on
    -- the row, a space for a blank, nothing at the row's start.
    go :: Word -> Maybe Text -> [Cell] -> [Cell]
    go col prev = \case
      [] -> []
      c : rest -> case onText kept c.glyph of
        Wide t
          | col + 1 < width,
            fits prev t 2,
            Cell WideTail s : rest' <- rest ->
              Cell (Wide t) c.style : Cell WideTail s : go (col + 2) (Just t) rest'
        Narrow t
          | t /= " ",
            fits prev t 1,
            -- The host places a cluster by the width it has while it reads
            -- it: one that is wide before a VS15 makes it narrow wraps from
            -- the last column.
            col + 1 < width || width == 1 || mode == CodePoints || peakWidth t < offset 2 ->
              Cell (Narrow t) c.style : go (col + 1) (Just t) rest
        _ -> blank c.style : go (col + 1) (Just " ") rest
    -- What the host keeps of a glyph's text (libghostty-vt 1.0's print).
    -- A variation selector it ignores is in no cell: in grapheme-cluster
    -- mode one after anything but an emoji variation base (D33), and
    -- otherwise one in a cell whose first code point is not an
    -- Extended_Pictographic that is not an Emoji_Modifier_Base. And one
    -- column wide in grapheme-cluster mode the only column is always waiting
    -- to wrap, and nothing joins a cell there: a narrow cluster is its first
    -- code point.
    kept t = case mode of
      Clusters | width == 1 -> T.take 1 t
      Clusters -> T.pack (afterBase Nothing (toString t))
      CodePoints -> case T.uncons t of
        Just (c0, _) | extendedPictographic c0 && not (emojiModifierBase c0) -> t
        _ -> T.filter (not . selector) t
    afterBase prev = \case
      [] -> []
      ch : chs
        | selector ch -> if maybe False emojiVariationBase prev then ch : afterBase (Just ch) chs else afterBase prev chs
        | otherwise -> ch : afterBase (Just ch) chs
    selector ch = ch == '\xFE0E' || ch == '\xFE0F'
    -- One piece of the width, with no control in it, that the host does not
    -- join to the text before it: it would join a spacing mark to a space,
    -- or a regional indicator to one.
    fits prev t n =
      T.all (\ch -> graphemeBreak ch `notElem` [Control, CR, LF]) t
        && pieces mode t == [(t, offset n)]
        && maybe True (\p -> fmap fst (viaNonEmpty last (pieces mode (p <> t))) == Just t) prev

-- | A glyph with its text changed.
onText :: (Text -> Text) -> Glyph -> Glyph
onText f = \case
  Narrow t -> Narrow (f t)
  Wide t -> Wide (f t)
  g -> g

-- | A grid with no rows, and no cursor.
emptyGrid :: WidthMode -> Offset 'Column -> Grid
emptyGrid mode w = grid mode w [] Nothing

-- | A row's text, as plain text: a blank is a space and a tail is nothing,
-- so the text is as wide as the row.
rowText :: Seq Cell -> Text
rowText = foldMap glyphText . toList
  where
    glyphText c = case c.glyph of
      Blank -> " "
      Narrow t -> t
      Wide t -> t
      WideTail -> ""
