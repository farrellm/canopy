-- | A cell of a frame (DESIGN.md §4.3): a plain Haskell value, copied out of
-- the render state, so it carries no frame tag and outlives the frame it came
-- from.
module Ghostty.Effectful.Cell
  ( Cell (..),
    CellWidth (..),
    SemanticContent (..),
    Hyperlink (..),
  )
where

import Ghostty.Effectful.Color (Rgb)
import Ghostty.Effectful.Style (Style)

-- | One cell of a row.
data Cell = Cell
  { -- | the grapheme cluster, base codepoint first; empty for a blank cell
    -- and for the spacer after a wide character
    text :: Text,
    -- | how much of a wide character the cell is
    width :: CellWidth,
    -- | the cell's SGR style
    style :: Style,
    -- | the foreground colour, resolved through the palette; 'Nothing' for
    -- the terminal's default. Bold brightening is not applied.
    foreground :: Maybe Rgb,
    -- | the background colour, resolved through the palette, whether the
    -- style or the cell's own content sets it; 'Nothing' for the terminal's
    -- default
    background :: Maybe Rgb,
    -- | the OSC 8 link the cell is part of, if any
    hyperlink :: Maybe Hyperlink,
    -- | whether DECSCA protects the cell from selective erase
    protected :: Bool,
    -- | what the program said the cell is, by OSC 133
    semantic :: SemanticContent
  }
  deriving stock (Eq, Show)

-- | A cell's part in a character's width.
data CellWidth
  = -- | a character one column wide, or a blank
    Narrow
  | -- | the first column of a character two columns wide
    Wide
  | -- | the second column of a wide character, which has no text of its own
    SpacerTail
  | -- | the last column of a row, left empty because the wide character
    -- that would have started there wrapped to the next row
    SpacerHead
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | What a cell holds, by the program's OSC 133 marks.
data SemanticContent
  = -- | output, the default
    ContentOutput
  | -- | part of the user's input
    ContentInput
  | -- | part of a prompt
    ContentPrompt
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | An OSC 8 hyperlink.
newtype Hyperlink = Hyperlink
  { -- | the link's URI. The render state records only that a cell has a
    -- link; the URI is read from the terminal, so it is 'Nothing' when the
    -- terminal has moved past the frame: a frame held by synchronized
    -- output, or a terminal written to since the frame was taken.
    uri :: Maybe Text
  }
  deriving stock (Eq, Show)
