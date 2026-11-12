{-# LANGUAGE PatternSynonyms #-}

-- | Between the raw binding's values and the high level's (DESIGN.md §4.4):
-- colours, styles, points and modes. A C enum value the binding does not
-- name is 'UnknownEnum', which the version check and the layout test rule
-- out (§3.3).
module Ghostty.Effectful.Internal.Convert
  ( -- * Enums
    unknownEnum,
    unknownEnumError,
    enumName,
    enumSite,
    getSite,
    oscTerminator,

    -- * Strings and flags
    stringText,
    utf8Text,
    withString,
    emptyString,
    flagBits,
    unfoldWhileM,
    kittyFlagBits,
    fromKittyFlagBits,

    -- * Colours and styles
    fromRgb,
    toRgb,
    readPalette,
    fromPalette,
    colorSchemeCode,
    toCursorStyle,
    fromCursorStyle,
    fromStyle,
    fromUnderline,

    -- * Points
    pointTag,
    toPoint,
    fromCoordinate,

    -- * Modes
    modeCode,

    -- * Sized and frozen structs
    withSize,
    emptyRef,
  )
where

import Control.Exception (throwIO)
import Data.Bits (Bits, complement, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Ghostty.Effectful.Color (Palette, Rgb (..), paletteFromList)
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), ResultCode (..))
import Ghostty.Effectful.Event (OscTerminator (..))
import Ghostty.Effectful.Internal.Error (expectSuccess, orInvariant)
import Ghostty.Effectful.Mode (ModeKey (..))
import Ghostty.Effectful.Point (KnownCoord (..), Point (..), SCoord (..))
import Ghostty.Effectful.Style (CellColor (..), Style (..), Underline (..))
import Ghostty.Effectful.Terminal.Keys (ColorScheme (..), CursorShape (..), KittyFlags (..))
import Ghostty.FFI.Color (GhosttyColorRgb (..))
import Ghostty.FFI.Device (GhosttyColorScheme, pattern GhosttyColorSchemeDark, pattern GhosttyColorSchemeLight)
import Ghostty.FFI.GridRef (GhosttyGridRef (..))
import Ghostty.FFI.Key.Encoder
  ( GhosttyKittyKeyFlags,
    pattern GhosttyKittyKeyAll,
    pattern GhosttyKittyKeyDisambiguate,
    pattern GhosttyKittyKeyReportAll,
    pattern GhosttyKittyKeyReportAlternates,
    pattern GhosttyKittyKeyReportAssociated,
    pattern GhosttyKittyKeyReportEvents,
  )
import Ghostty.FFI.Layout (CEnum (..))
import Ghostty.FFI.Modes
import Ghostty.FFI.Osc (GhosttyOscTerminator, pattern GhosttyOscTerminatorBel, pattern GhosttyOscTerminatorSt, pattern GhosttyOscTerminatorUnknown)
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
import Ghostty.FFI.Terminal (GhosttyTerminalData (..), ghosttyTerminalDataEnum)
import Ghostty.FFI.Types
  ( GhosttyCursorStyle,
    GhosttyResult,
    GhosttyString (..),
    ghosttyCursorStyleEnum,
    ghosttyStringBytes,
    pattern GhosttyCursorStyleBar,
    pattern GhosttyCursorStyleBlock,
    pattern GhosttyCursorStyleBlockHollow,
    pattern GhosttyCursorStyleUnderline,
    pattern GhosttyCursorStyleUnknown,
  )

-- | Throw 'UnknownEnum' for a value of the described enum.
unknownEnum :: CallSite -> CEnum -> CInt -> IO a
unknownEnum site e n = throwIO (unknownEnumError site e n)

-- | The error 'unknownEnum' throws, for a caller that keeps it.
unknownEnumError :: CallSite -> CEnum -> CInt -> GhosttyError
unknownEnumError site e n = UnknownEnum site e.name (toInteger n)

-- | The full C name of an enum's value (@GHOSTTY_TERMINAL_DATA_COLS@), or
-- the number if the binding does not name it.
enumName :: CEnum -> CInt -> Text
enumName e n = maybe (show n) fst (find ((== toInteger n) . snd) e.values)

-- | How an OSC ended, from the binding's; 'Left' with a value it does not
-- name.
oscTerminator :: GhosttyOscTerminator -> Either CInt OscTerminator
oscTerminator = \case
  GhosttyOscTerminatorSt -> Right TerminatedBySt
  GhosttyOscTerminatorBel -> Right TerminatedByBel
  GhosttyOscTerminatorUnknown n -> Left n

-- | The call site of a call that takes an enum value, named with it:
-- @ghostty_terminal_get GHOSTTY_TERMINAL_DATA_COLS@.
enumSite :: Text -> CEnum -> CInt -> CallSite
enumSite call e n = CallSite (call <> " " <> enumName e n)

-- | The call site of @ghostty_terminal_get@ with the key.
getSite :: GhosttyTerminalData -> CallSite
getSite k = enumSite "ghostty_terminal_get" ghosttyTerminalDataEnum (coerce k)

-- | A borrowed string, copied out and decoded leniently.
stringText :: GhosttyString -> IO Text
stringText s = decodeUtf8 <$> ghosttyStringBytes s

-- | Borrowed UTF-8 bytes decoded now, so the memory can be reused when this
-- returns: one copy, into the 'Text', and none for no bytes. (Copying into
-- a 'ByteString' first is a pinned allocation and a second copy, and a
-- lazy decode would keep it.)
utf8Text :: Ptr Word8 -> Int -> IO Text
utf8Text p n
  | n <= 0 = pure mempty
  | otherwise = do
      bs <- BSU.unsafePackCStringLen (castPtr p, n)
      pure $! decodeUtf8 bs
{-# INLINE utf8Text #-}

-- | Run with a borrowed 'GhosttyString' of the bytes, alive until the
-- action returns. Empty bytes are 'emptyString'.
withString :: ByteString -> (Ptr GhosttyString -> IO a) -> IO a
withString bs k
  | BS.null bs = with emptyString k
  | otherwise = BSU.unsafeUseAsCStringLen bs $ \(p, n) ->
      with GhosttyString {ptr = castPtr p, len = fromIntegral n} k

-- | No bytes, at no address: a string of length zero, whose pointer is
-- not read.
emptyString :: GhosttyString
emptyString = GhosttyString {ptr = nullPtr, len = 0}

-- | The flags whose condition holds, combined.
flagBits :: (Bits b, Num b) => [(Bool, b)] -> b
flagBits fs = foldl' (.|.) 0 [b | (True, b) <- fs]

-- | Run the action while the test holds, collecting its results in order:
-- an iterator's @next@ and its current value.
unfoldWhileM :: (Monad m) => m Bool -> m a -> m [a]
unfoldWhileM more act = go []
  where
    go acc = ifM more (act >>= \a -> go (a : acc)) (pure (reverse acc))

-- | The Kitty keyboard flags as the header's bits.
kittyFlagBits :: KittyFlags -> GhosttyKittyKeyFlags
kittyFlagBits f =
  flagBits
    [ (f.disambiguate, GhosttyKittyKeyDisambiguate),
      (f.reportEvents, GhosttyKittyKeyReportEvents),
      (f.reportAlternates, GhosttyKittyKeyReportAlternates),
      (f.reportAll, GhosttyKittyKeyReportAll),
      (f.reportAssociated, GhosttyKittyKeyReportAssociated)
    ]

-- | The Kitty keyboard flags from the header's bits; 'UnknownEnum' for a
-- bit the header does not name.
fromKittyFlagBits :: CallSite -> GhosttyKittyKeyFlags -> IO KittyFlags
fromKittyFlagBits site w
  | w .&. complement GhosttyKittyKeyAll /= 0 = throwIO (UnknownEnum site "GhosttyKittyKeyFlags" (toInteger w))
  | otherwise =
      pure
        KittyFlags
          { disambiguate = has GhosttyKittyKeyDisambiguate,
            reportEvents = has GhosttyKittyKeyReportEvents,
            reportAlternates = has GhosttyKittyKeyReportAlternates,
            reportAll = has GhosttyKittyKeyReportAll,
            reportAssociated = has GhosttyKittyKeyReportAssociated
          }
  where
    has b = w .&. b /= 0

-- | A colour from the binding's.
fromRgb :: GhosttyColorRgb -> Rgb
fromRgb c = Rgb {red = c.r, green = c.g, blue = c.b}

-- | A colour as the binding's.
toRgb :: Rgb -> GhosttyColorRgb
toRgb c = GhosttyColorRgb {r = c.red, g = c.green, b = c.blue}

-- | The 256-colour palette a call writes to its out-parameter.
readPalette :: CallSite -> (Ptr GhosttyColorRgb -> IO GhosttyResult) -> IO Palette
readPalette site call = allocaArray 256 $ \p -> do
  expectSuccess site =<< call p
  fromPalette site =<< peekArray 256 p

-- | A palette from the binding's 256 colours; 'Invariant' for any other
-- count.
fromPalette :: CallSite -> [GhosttyColorRgb] -> IO Palette
fromPalette site = orInvariant site ResultInvalidValue . paletteFromList . map fromRgb

-- | A colour scheme as the binding's.
colorSchemeCode :: ColorScheme -> GhosttyColorScheme
colorSchemeCode = \case
  Light -> GhosttyColorSchemeLight
  Dark -> GhosttyColorSchemeDark

-- | A cursor shape as the binding's.
toCursorStyle :: CursorShape -> GhosttyCursorStyle
toCursorStyle = \case
  BarCursor -> GhosttyCursorStyleBar
  BlockCursor -> GhosttyCursorStyleBlock
  UnderlineCursor -> GhosttyCursorStyleUnderline
  HollowBlockCursor -> GhosttyCursorStyleBlockHollow

-- | A cursor shape from the binding's.
fromCursorStyle :: CallSite -> GhosttyCursorStyle -> IO CursorShape
fromCursorStyle site = \case
  GhosttyCursorStyleBar -> pure BarCursor
  GhosttyCursorStyleBlock -> pure BlockCursor
  GhosttyCursorStyleUnderline -> pure UnderlineCursor
  GhosttyCursorStyleBlockHollow -> pure HollowBlockCursor
  GhosttyCursorStyleUnknown n -> unknownEnum site ghosttyCursorStyleEnum n

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
