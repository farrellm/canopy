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

    -- * Points
    pointTag,
    toPoint,
    fromCoordinate,

    -- * Modes
    modeCode,

    -- * Sized structs
    sized,
    refTemplate,
  )
where

import Control.Exception (throwIO)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Utils (toBool)
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
    ghosttyStyleColorTagEnum,
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

-- | A style from the binding's.
fromStyle :: CallSite -> GhosttyStyle -> IO Style
fromStyle site s = do
  foreground <- color s.fgColor
  background <- color s.bgColor
  underlineColor <- color s.underlineColor
  underline <- case GhosttySgrUnderline s.underline of
    GhosttySgrUnderlineNone -> pure NoUnderline
    GhosttySgrUnderlineSingle -> pure SingleUnderline
    GhosttySgrUnderlineDouble -> pure DoubleUnderline
    GhosttySgrUnderlineCurly -> pure CurlyUnderline
    GhosttySgrUnderlineDotted -> pure DottedUnderline
    GhosttySgrUnderlineDashed -> pure DashedUnderline
    GhosttySgrUnderlineUnknown n -> unknownEnum site ghosttySgrUnderlineEnum n
  pure
    Style
      { foreground,
        background,
        underlineColor,
        bold = toBool s.bold,
        italic = toBool s.italic,
        faint = toBool s.faint,
        blink = toBool s.blink,
        inverse = toBool s.inverse,
        invisible = toBool s.invisible,
        strikethrough = toBool s.strikethrough,
        overline = toBool s.overline,
        underline
      }
  where
    color = \case
      StyleColorNone -> pure DefaultColor
      StyleColorPalette i -> pure (PaletteColor i)
      StyleColorRgb c -> pure (RgbColor (fromRgb c))
      StyleColorOther (GhosttyStyleColorTag n) -> unknownEnum site ghosttyStyleColorTagEnum n

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
  ModeKam -> GhosttyModeKam
  ModeInsert -> GhosttyModeInsert
  ModeSrm -> GhosttyModeSrm
  ModeLinefeed -> GhosttyModeLinefeed
  ModeDecckm -> GhosttyModeDecckm
  Mode132Column -> GhosttyMode132Column
  ModeSlowScroll -> GhosttyModeSlowScroll
  ModeReverseColors -> GhosttyModeReverseColors
  ModeOrigin -> GhosttyModeOrigin
  ModeWraparound -> GhosttyModeWraparound
  ModeAutorepeat -> GhosttyModeAutorepeat
  ModeX10Mouse -> GhosttyModeX10Mouse
  ModeCursorBlinking -> GhosttyModeCursorBlinking
  ModeCursorVisible -> GhosttyModeCursorVisible
  ModeEnableMode3 -> GhosttyModeEnableMode3
  ModeReverseWrap -> GhosttyModeReverseWrap
  ModeAltScreenLegacy -> GhosttyModeAltScreenLegacy
  ModeKeypadKeys -> GhosttyModeKeypadKeys
  ModeBackarrowKeyMode -> GhosttyModeBackarrowKeyMode
  ModeLeftRightMargin -> GhosttyModeLeftRightMargin
  ModeNormalMouse -> GhosttyModeNormalMouse
  ModeButtonMouse -> GhosttyModeButtonMouse
  ModeAnyMouse -> GhosttyModeAnyMouse
  ModeFocusEvent -> GhosttyModeFocusEvent
  ModeUtf8Mouse -> GhosttyModeUtf8Mouse
  ModeSgrMouse -> GhosttyModeSgrMouse
  ModeAltScroll -> GhosttyModeAltScroll
  ModeUrxvtMouse -> GhosttyModeUrxvtMouse
  ModeSgrPixelsMouse -> GhosttyModeSgrPixelsMouse
  ModeNumlockKeypad -> GhosttyModeNumlockKeypad
  ModeAltEscPrefix -> GhosttyModeAltEscPrefix
  ModeAltSendsEsc -> GhosttyModeAltSendsEsc
  ModeReverseWrapExt -> GhosttyModeReverseWrapExt
  ModeAltScreen -> GhosttyModeAltScreen
  ModeSaveCursor -> GhosttyModeSaveCursor
  ModeAltScreenSave -> GhosttyModeAltScreenSave
  ModeBracketedPaste -> GhosttyModeBracketedPaste
  ModeSyncOutput -> GhosttyModeSyncOutput
  ModeGraphemeCluster -> GhosttyModeGraphemeCluster
  ModeColorSchemeReport -> GhosttyModeColorSchemeReport
  ModeVisibilityReport -> GhosttyModeVisibilityReport
  ModeInBandResize -> GhosttyModeInBandResize
  ModePasteEvents -> GhosttyModePasteEvents

-- | A sized struct's own size, for @GHOSTTY_INIT_SIZED@: the template at
-- the size of the struct it makes ('sizeOf' ignores its argument).
sized :: (Storable b) => (CSize -> b) -> CSize
sized mk = fromIntegral (sizeOf (mk 0))

-- | An empty grid reference of the given size.
refTemplate :: CSize -> GhosttyGridRef
refTemplate n = GhosttyGridRef {size = n, node = nullPtr, x = 0, y = 0}
