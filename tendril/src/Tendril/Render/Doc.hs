{-# LANGUAGE StrictData #-}

-- | What the editor draws, before it is laid out (DESIGN.md §5.6): lines of
-- styled text, the cursor's place among them, and boxes over them for
-- popups. "Tendril.Render.Layout" turns a 'Doc' into a grid of cells at the
-- host's width.
--
-- A 'Doc' knows nothing of the width. Its lines wrap where the layout says,
-- and the cursor and a box under it are placed in the text, not in cells, so
-- a widget builds its 'Doc' from the buffer without measuring it.
module Tendril.Render.Doc
  ( -- * Documents
    Doc (..),
    doc,

    -- * Lines
    Line (..),
    Inline (..),
    styled,
    text,
    cursorHere,

    -- * Boxes
    Box (..),
    Anchor (..),
  )
where

import Tendril.Render.Style (Style, plain)
import Tendril.Text.Units (Offset, Unit (..))

-- | A piece of a line.
data Inline
  = -- | Text in a style. A newline in it breaks the line, as a new 'Line'
    -- would; a tab moves to the next multiple of eight columns of its row;
    -- any other control character is drawn visibly, as @^[@ or @\<U+0085\>@,
    -- and is never sent to the host.
    Run Style Text
  | -- | Where the cursor is: before the text that follows. The first in the
    -- 'Doc' is the cursor; any later one is ignored.
    Cursor
  deriving stock (Eq, Show)

-- | One line of the document. It wraps at the layout's width, onto as many
-- rows as it needs; an empty line is one empty row.
newtype Line = Line
  { -- | Its pieces, left to right.
    inlines :: [Inline]
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

-- | Where a box is drawn.
data Anchor
  = -- | On the row below the cursor, from the cursor's column, moved left as
    -- far as it needs to fit the width: a completion menu. Below the last
    -- row, at the first column, if the document has no cursor.
    BelowCursor
  | -- | At a row and column of the laid-out grid, moved left to fit the width.
    At Word (Offset 'Column)
  deriving stock (Eq, Show)

-- | A rectangle drawn over the document: a popup. It is as wide as its widest
-- line, or the width if that is less, and as tall as its lines. Its lines do
-- not wrap: a line too long for it is cut, and a shorter one is filled to the
-- box's width with the fill style. The grid grows downwards to hold it.
data Box = Box
  { -- | Where the box is.
    anchor :: Anchor,
    -- | The style of the box's cells that no line reaches.
    fill :: Style,
    -- | Its lines, top to bottom. A newline or a cursor in one is ignored.
    body :: [Line]
  }
  deriving stock (Eq, Show)

-- | A document: lines, top to bottom, and the boxes drawn over them, each
-- over the ones before it.
data Doc = Doc
  { -- | The text.
    lines :: [Line],
    -- | The popups.
    boxes :: [Box]
  }
  deriving stock (Eq, Show)

-- | A document of lines and no box.
doc :: [Line] -> Doc
doc ls = Doc {lines = ls, boxes = []}

-- | A line of one run.
styled :: Style -> Text -> Line
styled s t = Line [Run s t]

-- | A line of one run in the host's default style.
text :: Text -> Line
text = styled plain

-- | A line that is only the cursor.
cursorHere :: Line
cursorHere = Line [Cursor]
