{-# LANGUAGE StrictData #-}

-- | How a cell looks (DESIGN.md §5.6): its colours, its attributes and its
-- link. A 'Style' is what one SGR state and one OSC 8 link set on the host,
-- and every cell of a "Tendril.Render.Grid" carries one.
--
-- A colour is the host's default, an entry of the host's palette, or a direct
-- colour. Tendril never resolves a palette entry: the host's palette is the
-- user's theme, and an application that wants a colour of its own (canopy's
-- child, whose palette is not the host's) resolves it to 'Rgb' itself.
module Tendril.Render.Style
  ( -- * Styles
    Style (..),
    plain,

    -- * Colours
    Color (..),

    -- * Underlines
    Underline (..),
  )
where

-- | A colour, as the host draws it.
data Color
  = -- | The host's default for the role: its foreground for text, its
    -- background behind it, the text's colour for an underline.
    Default
  | -- | An entry of the host's 256-colour palette; 0–15 are the user's
    -- theme.
    Palette Word8
  | -- | A direct colour: red, green, blue.
    Rgb Word8 Word8 Word8
  deriving stock (Eq, Ord, Show)

-- | An underline's shape.
data Underline
  = -- | No underline.
    NoUnderline
  | -- | A single line.
    Single
  | -- | Two lines.
    Double
  | -- | A wavy line.
    Curly
  | -- | A dotted line.
    Dotted
  | -- | A dashed line.
    Dashed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | How a cell looks. Build one from 'plain' with record update:
--
-- > plain {bold = True, foreground = Palette 2}
data Style = Style
  { -- | The text's colour.
    foreground :: Color,
    -- | The colour behind the text.
    background :: Color,
    -- | The underline's colour; 'Default' is the text's.
    underlineColor :: Color,
    -- | Bold, or bright.
    bold :: Bool,
    -- | Dim.
    faint :: Bool,
    -- | Italic.
    italic :: Bool,
    -- | The underline, if any.
    underline :: Underline,
    -- | Blinking.
    blink :: Bool,
    -- | Foreground and background swapped.
    inverse :: Bool,
    -- | Drawn in the background colour: there, but not seen.
    invisible :: Bool,
    -- | Struck through.
    strikethrough :: Bool,
    -- | A line above.
    overline :: Bool,
    -- | An OSC 8 hyperlink's URI. A cell is clickable in a host that knows
    -- OSC 8, and looks the same in one that does not.
    link :: Maybe Text
  }
  deriving stock (Eq, Ord, Show)

-- | The host's default look: no colour, no attribute, no link.
plain :: Style
plain =
  Style
    { foreground = Default,
      background = Default,
      underlineColor = Default,
      bold = False,
      faint = False,
      italic = False,
      underline = NoUnderline,
      blink = False,
      inverse = False,
      invisible = False,
      strikethrough = False,
      overline = False,
      link = Nothing
    }
