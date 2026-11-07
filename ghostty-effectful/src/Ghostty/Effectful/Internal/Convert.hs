{-# LANGUAGE PatternSynonyms #-}

-- | Between the raw binding's values and the high level's (DESIGN.md §4.4):
-- colours, styles, points and modes. A C enum value the binding does not
-- name is 'UnknownEnum', which the version check and the layout test rule
-- out (§3.3).
module Ghostty.Effectful.Internal.Convert
  ( -- * Enums
    unknownEnum,
    enumName,

    -- * Colours and styles
    fromRgb,
    toRgb,
    fromStyle,
    fromUnderline,

    -- * Points
    pointTag,
    toPoint,
    fromCoordinate,

    -- * Modes
    modeCode,

    -- * Sized and frozen structs
    sized,
    withSize,
    emptyRef,
  )
where

import Control.Exception (throwIO)
import Data.Bits ((.&.))
import Foreign.C.Types (CInt, CSize)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable (..))
import Ghostty.Effectful.Color (Rgb (..))
import Ghostty.Effectful.Error (CallSite, GhosttyError (..))
import Ghostty.Effectful.Mode (ModeKey (..))
import Ghostty.Effectful.Point (KnownCoord (..), Point (..), SCoord (..))
import Ghostty.Effectful.Style (CellColor (..), Style (..), Underline (..))
import Ghostty.FFI.Color (GhosttyColorRgb (..))
import Ghostty.FFI.GridRef (GhosttyGridRef (..))
import Ghostty.FFI.Layout (CEnum (..))
import Ghostty.FFI.Modes
import Ghostty.FFI.Point
  ( GhosttyPoint (..),
    GhosttyPointCoordinate (..),
    GhosttyPointTag,
    pattern GhosttyPointTagActive,
    pattern GhosttyPointTagHistory,
    pattern GhosttyPointTagScreen,
    pattern GhosttyPointTagViewport,
  )
import Ghostty.FFI.Sgr
  ( GhosttySgrUnderline (..),
    ghosttySgrUnderlineEnum,
    pattern GhosttySgrUnderlineCurly,
    pattern GhosttySgrUnderlineDashed,
    pattern GhosttySgrUnderlineDotted,
    pattern GhosttySgrUnderlineDouble,
    pattern GhosttySgrUnderlineNone,
    pattern GhosttySgrUnderlineSingle,
    pattern GhosttySgrUnderlineUnknown,
  )
import Ghostty.FFI.Style
  ( GhosttyStyle (..),
    GhosttyStyleColor (..),
    GhosttyStyleColorTag (..),
    GhosttyStyleFlags,
    ghosttyStyleColorTagEnum,
    ghosttyStyleUnderline,
    pattern GhosttyStyleFlagBlink,
    pattern GhosttyStyleFlagBold,
    pattern GhosttyStyleFlagFaint,
    pattern GhosttyStyleFlagInverse,
    pattern GhosttyStyleFlagInvisible,
    pattern GhosttyStyleFlagItalic,
    pattern GhosttyStyleFlagOverline,
    pattern GhosttyStyleFlagStrikethrough,
  )

-- | Throw 'UnknownEnum' for a value of the described enum.
unknownEnum :: CallSite -> CEnum -> CInt -> IO a
unknownEnum site e n = throwIO (UnknownEnum site e.name (toInteger n))

-- | The full C name of an enum's value (@GHOSTTY_TERMINAL_DATA_COLS@), or
-- the number if the binding does not name it.
enumName :: CEnum -> CInt -> Text
enumName e n = maybe (show n) fst (find ((== toInteger n) . snd) e.values)

-- | A colour from the binding's.
fromRgb :: GhosttyColorRgb -> Rgb
fromRgb c = Rgb {red = c.r, green = c.g, blue = c.b}

-- | A colour as the binding's.
toRgb :: Rgb -> GhosttyColorRgb
toRgb c = GhosttyColorRgb {r = c.red, g = c.green, b = c.blue}

-- | A style from the binding's: its colours, and its flag word decoded, the
-- attributes from their bits and the underline from its field (bits 8–10).
fromStyle :: CallSite -> GhosttyStyle -> IO Style
fromStyle site s = do
  foreground <- color s.fgColor
  background <- color s.bgColor
  underlineColor <- color s.underlineColor
  underline <- fromUnderline site (GhosttySgrUnderline (ghosttyStyleUnderline s.flags))
  pure
    Style
      { foreground,
        background,
        underlineColor,
        bold = flag GhosttyStyleFlagBold,
        italic = flag GhosttyStyleFlagItalic,
        faint = flag GhosttyStyleFlagFaint,
        blink = flag GhosttyStyleFlagBlink,
        inverse = flag GhosttyStyleFlagInverse,
        invisible = flag GhosttyStyleFlagInvisible,
        strikethrough = flag GhosttyStyleFlagStrikethrough,
        overline = flag GhosttyStyleFlagOverline,
        underline
      }
  where
    flag :: GhosttyStyleFlags -> Bool
    flag f = s.flags .&. f /= 0
    color = \case
      StyleColorNone -> pure DefaultColor
      StyleColorPalette i -> pure (PaletteColor i)
      StyleColorRgb c -> pure (RgbColor (fromRgb c))
      StyleColorOther (GhosttyStyleColorTag n) -> unknownEnum site ghosttyStyleColorTagEnum n

-- | An underline style from the binding's.
fromUnderline :: CallSite -> GhosttySgrUnderline -> IO Underline
fromUnderline site = \case
  GhosttySgrUnderlineNone -> pure NoUnderline
  GhosttySgrUnderlineSingle -> pure SingleUnderline
  GhosttySgrUnderlineDouble -> pure DoubleUnderline
  GhosttySgrUnderlineCurly -> pure CurlyUnderline
  GhosttySgrUnderlineDotted -> pure DottedUnderline
  GhosttySgrUnderlineDashed -> pure DashedUnderline
  GhosttySgrUnderlineUnknown n -> unknownEnum site ghosttySgrUnderlineEnum n

-- | The tag of a coordinate system.
pointTag :: SCoord c -> GhosttyPointTag
pointTag = \case
  SActive -> GhosttyPointTagActive
  SViewport -> GhosttyPointTagViewport
  SGrid -> GhosttyPointTagScreen
  SHistory -> GhosttyPointTagHistory

-- | A point as the binding's.
toPoint :: forall c. (KnownCoord c) => Point c -> GhosttyPoint
toPoint p =
  GhosttyPoint
    { tag = pointTag (coordSing @c),
      coordinate = GhosttyPointCoordinate {x = p.col, y = p.row}
    }

-- | A point from a coordinate the library returned in the system @c@.
fromCoordinate :: GhosttyPointCoordinate -> Point c
fromCoordinate c = Point {col = c.x, row = c.y}

-- | A mode's packed number.
modeCode :: ModeKey r -> GhosttyMode
modeCode = \case
  ModeDisableKeyboard -> GhosttyModeDisableKeyboard
  ModeInsert -> GhosttyModeInsert
  ModeSendReceiveMode -> GhosttyModeSendReceiveMode
  ModeLinefeed -> GhosttyModeLinefeed
  ModeCursorKeys -> GhosttyModeCursorKeys
  Mode132Column -> GhosttyMode132Column
  ModeSlowScroll -> GhosttyModeSlowScroll
  ModeReverseColors -> GhosttyModeReverseColors
  ModeOrigin -> GhosttyModeOrigin
  ModeWraparound -> GhosttyModeWraparound
  ModeAutorepeat -> GhosttyModeAutorepeat
  ModeMouseEventX10 -> GhosttyModeMouseEventX10
  ModeCursorBlinking -> GhosttyModeCursorBlinking
  ModeCursorVisible -> GhosttyModeCursorVisible
  ModeEnableMode3 -> GhosttyModeEnableMode3
  ModeReverseWrap -> GhosttyModeReverseWrap
  ModeAltScreenLegacy -> GhosttyModeAltScreenLegacy
  ModeKeypadKeys -> GhosttyModeKeypadKeys
  ModeBackarrowKeyMode -> GhosttyModeBackarrowKeyMode
  ModeEnableLeftAndRightMargin -> GhosttyModeEnableLeftAndRightMargin
  ModeMouseEventNormal -> GhosttyModeMouseEventNormal
  ModeMouseEventButton -> GhosttyModeMouseEventButton
  ModeMouseEventAny -> GhosttyModeMouseEventAny
  ModeFocusEvent -> GhosttyModeFocusEvent
  ModeMouseFormatUtf8 -> GhosttyModeMouseFormatUtf8
  ModeMouseFormatSgr -> GhosttyModeMouseFormatSgr
  ModeMouseAlternateScroll -> GhosttyModeMouseAlternateScroll
  ModeMouseFormatUrxvt -> GhosttyModeMouseFormatUrxvt
  ModeMouseFormatSgrPixels -> GhosttyModeMouseFormatSgrPixels
  ModeIgnoreKeypadWithNumlock -> GhosttyModeIgnoreKeypadWithNumlock
  ModeAltEscPrefix -> GhosttyModeAltEscPrefix
  ModeAltSendsEscape -> GhosttyModeAltSendsEscape
  ModeReverseWrapExtended -> GhosttyModeReverseWrapExtended
  ModeAltScreen -> GhosttyModeAltScreen
  ModeSaveCursor -> GhosttyModeSaveCursor
  ModeAltScreenSaveCursorClearEnter -> GhosttyModeAltScreenSaveCursorClearEnter
  ModeBracketedPaste -> GhosttyModeBracketedPaste
  ModeSynchronizedOutput -> GhosttyModeSynchronizedOutput
  ModeGraphemeCluster -> GhosttyModeGraphemeCluster
  ModeReportColorScheme -> GhosttyModeReportColorScheme
  ModeReportVisibility -> GhosttyModeReportVisibility
  ModeInBandSizeReports -> GhosttyModeInBandSizeReports
  ModeKittyPasteEvents -> GhosttyModeKittyPasteEvents

-- | A sized struct's own size, for @GHOSTTY_INIT_SIZED@: the template at
-- the size of the struct it makes ('sizeOf' ignores its argument).
sized :: (Storable b) => (CSize -> b) -> CSize
sized mk = fromIntegral (sizeOf (mk 0))

-- | A sized struct made at its own size: @withSize (\\size -> S {size, ..})@
-- writes the struct once, where @S {size = sized (\\n -> S {size = n, ..}),
-- ..}@ writes it twice.
withSize :: (Storable b) => (CSize -> b) -> b
withSize mk = mk (sized mk)

-- | An empty grid reference, zeroed as @{0}@ does: @GhosttyGridRef@ is
-- frozen, with no @size@ field.
emptyRef :: GhosttyGridRef
emptyRef = GhosttyGridRef {node = nullPtr, x = 0, y = 0}
