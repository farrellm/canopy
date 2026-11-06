-- | The SGR style of a cell or of the cursor's pen (DESIGN.md §4.3, §4.4): a
-- plain Haskell value, copied out of the library.
module Ghostty.Effectful.Style
  ( Style (..),
    defaultStyle,
    CellColor (..),
    Underline (..),
  )
where

import Ghostty.Effectful.Color (Rgb)

-- | A colour as a style names it.
data CellColor
  = -- | none set: the terminal's default for the role
    DefaultColor
  | -- | an index into the terminal's palette
    PaletteColor Word8
  | -- | a direct colour
    RgbColor Rgb
  deriving stock (Eq, Show)

-- | An underline's shape (SGR 4 and its subparameters).
data Underline
  = -- | none
    NoUnderline
  | -- | single
    SingleUnderline
  | -- | double
    DoubleUnderline
  | -- | curly
    CurlyUnderline
  | -- | dotted
    DottedUnderline
  | -- | dashed
    DashedUnderline
  deriving stock (Eq, Show, Enum, Bounded)

-- | A style: colours, attributes and underline.
data Style = Style
  { -- | the foreground colour
    foreground :: CellColor,
    -- | the background colour
    background :: CellColor,
    -- | the underline's colour
    underlineColor :: CellColor,
    -- | SGR 1
    bold :: Bool,
    -- | SGR 3
    italic :: Bool,
    -- | SGR 2
    faint :: Bool,
    -- | SGR 5
    blink :: Bool,
    -- | SGR 7
    inverse :: Bool,
    -- | SGR 8
    invisible :: Bool,
    -- | SGR 9
    strikethrough :: Bool,
    -- | SGR 53
    overline :: Bool,
    -- | SGR 4
    underline :: Underline
  }
  deriving stock (Eq, Show)

-- | No colours set, no attributes: what SGR 0 leaves.
defaultStyle :: Style
defaultStyle =
  Style
    { foreground = DefaultColor,
      background = DefaultColor,
      underlineColor = DefaultColor,
      bold = False,
      italic = False,
      faint = False,
      blink = False,
      inverse = False,
      invisible = False,
      strikethrough = False,
      overline = False,
      underline = NoUnderline
    }
