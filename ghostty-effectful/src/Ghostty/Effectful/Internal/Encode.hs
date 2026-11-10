{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}

-- | The encoders' representations and their raw calls (DESIGN.md §4.8). The
-- public API is "Ghostty.Effectful.Encode", which exports these types
-- without their constructors.
--
-- A key or mouse encoder is a handle, scoped like a terminal (§4.2), and
-- owns the one event handle it encodes from: an encode writes the event's
-- fields onto it and encodes it, so no event handle is made per event. The
-- reports and paste encoding have no handle. Every call here but 'pasteInto'
-- is @unsafe@ and calls nothing back (§3.3).
module Ghostty.Effectful.Internal.Encode
  ( -- * Key encoders
    KeyEncoder (..),
    KeyOptions (..),
    defaultKeyOptions,
    OptionAsAlt (..),
    openKeyEncoder,
    closeKeyEncoder,
    setKeyOptions,
    keyOptionsFromTerminal,
    encodeKeyEvent,
    keyCode,
    modsBits,

    -- * Mouse encoders
    MouseEncoder (..),
    MouseOptions (..),
    mouseOptions,
    MouseTracking (..),
    MouseFormat (..),
    MouseGeometry (..),
    Padding (..),
    noPadding,
    gridGeometry,
    openMouseEncoder,
    closeMouseEncoder,
    setMouseOptions,
    mouseOptionsFromTerminal,
    resetMouse,
    encodeMouseEvent,

    -- * Reports
    Focus (..),
    encodeFocusEvent,
    SizeReportStyle (..),
    encodeSizeReportOf,
    ModeNumber (..),
    ModeFamily (..),
    modeNumber,
    knownMode,
    ModeReport (..),
    encodeModeReportOf,
    encodeColorSchemeReportOf,

    -- * Paste
    Rejected (..),
    Bracketing (..),
    PasteSafety (..),
    pasteSafe,
    encodePasteBytes,
    pasteInto,
  )
where

import Control.Exception (finally, onException)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Text qualified as T
import Foreign.C.Types (CBool, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool, toBool, with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable, peek)
import Ghostty.Effectful.Error (CallSite (..), ResultCode (..))
import Ghostty.Effectful.Event (TerminalEvent)
import Ghostty.Effectful.Input
import Ghostty.Effectful.Internal.Convert (colorSchemeCode, emptyString, flagBits, kittyFlagBits, modeCode, withSize, withString)
import Ghostty.Effectful.Internal.Error (bytesVia, configured, expectSuccess, newHandle, succeedsUnless)
import Ghostty.Effectful.Internal.Feed (drainEvents)
import Ghostty.Effectful.Internal.Size (CellPixels (..), Extent, GridSize (..), fromExtent)
import Ghostty.Effectful.Internal.Terminal (Terminal (..))
import Ghostty.Effectful.Mode (ModeKey)
import Ghostty.Effectful.Terminal.Keys (ColorScheme (..), KittyFlags (..), noKittyFlags)
import Ghostty.FFI.ColorScheme (ghosttyColorSchemeReportEncode)
import Ghostty.FFI.Focus (ghosttyFocusEncode, pattern GhosttyFocusGained, pattern GhosttyFocusLost)
import Ghostty.FFI.Io (GhosttyMimeReader (..), GhosttyMimeReaderFn)
import Ghostty.FFI.Key.Encoder
import Ghostty.FFI.Key.Event
import Ghostty.FFI.Modes
  ( GhosttyMode,
    GhosttyModeReportState,
    ghosttyModeNew,
    ghosttyModeReportEncode,
    pattern GhosttyModeReportNotRecognized,
    pattern GhosttyModeReportPermanentlyReset,
    pattern GhosttyModeReportPermanentlySet,
    pattern GhosttyModeReportReset,
    pattern GhosttyModeReportSet,
  )
import Ghostty.FFI.Mouse.Encoder
import Ghostty.FFI.Mouse.Event
import Ghostty.FFI.Paste (GhosttyPaste (..), ghosttyPasteEncode, ghosttyPasteIsSafe, ghosttyTerminalPaste, pattern GhosttyPasteSourceClipboard)
import Ghostty.FFI.SizeReport
import Ghostty.FFI.Terminal.Callbacks (pattern GhosttyClipboardLocationStandard)
import Ghostty.FFI.Types (GhosttyResult)

-- * Buffers

-- | The bytes an encode call writes, which reports their length (or the
-- length it needs) through an out-parameter ('bytesVia').
encodeVia :: CallSite -> (Ptr Word8 -> CSize -> Ptr CSize -> IO GhosttyResult) -> IO ByteString
encodeVia site call = alloca $ \written -> bytesVia site $ \buf cap -> do
  r <- call buf (fromIntegral cap) written
  (r,) . fromIntegral <$> peek written

-- * Key encoders

-- | A key encoder, alive in the region @k@ that
-- 'Ghostty.Effectful.Encode.withKeyEncoder' opens, with the event handle it
-- encodes from. Its role is nominal, so 'coerce' cannot take it out of its
-- region.
type role KeyEncoder nominal

data KeyEncoder k = KeyEncoder
  { -- | the library's encoder
    encoder :: GhosttyKeyEncoder,
    -- | the event each encode fills in
    event :: GhosttyKeyEvent
  }

-- | How keys are encoded: the terminal modes and keyboard protocols that
-- change a key's bytes. 'Ghostty.Effectful.Encode.keyOptionsFrom' sets them
-- from what a terminal's program asked for.
data KeyOptions = KeyOptions
  { -- | DEC mode 1, DECCKM: application cursor keys
    cursorKeyApplication :: Bool,
    -- | DEC mode 66, DECNKM: application keypad
    keypadKeyApplication :: Bool,
    -- | DEC mode 1035: the keypad's application mode is off while Num Lock is on
    ignoreKeypadWithNumlock :: Bool,
    -- | DEC mode 1036: Alt sends an ESC prefix
    altEscPrefix :: Bool,
    -- | xterm's modifyOtherKeys, mode 2
    modifyOtherKeys2 :: Bool,
    -- | the Kitty keyboard protocol's flags; none is the legacy encoding
    kittyFlags :: KittyFlags,
    -- | whether macOS's Option key is Alt
    macosOptionAsAlt :: OptionAsAlt,
    -- | DEC mode 67, DECBKM: Backspace sends BS (0x08) rather than DEL (0x7f)
    backarrowKeyMode :: Bool
  }
  deriving stock (Eq, Show)

-- | The library's defaults: every mode off, the legacy encoding, and Option
-- not Alt.
defaultKeyOptions :: KeyOptions
defaultKeyOptions =
  KeyOptions
    { cursorKeyApplication = False,
      keypadKeyApplication = False,
      ignoreKeypadWithNumlock = False,
      altEscPrefix = False,
      modifyOtherKeys2 = False,
      kittyFlags = noKittyFlags,
      macosOptionAsAlt = OptionNotAlt,
      backarrowKeyMode = False
    }

-- | Which of macOS's Option keys are Alt.
data OptionAsAlt
  = -- | neither
    OptionNotAlt
  | -- | both
    OptionAlt
  | -- | the left one
    OptionLeftAlt
  | -- | the right one
    OptionRightAlt
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | @ghostty_key_encoder_new@ and @ghostty_key_event_new@, with the options
-- set. Either handle is freed if the other cannot be made.
openKeyEncoder :: KeyOptions -> IO (KeyEncoder k)
openKeyEncoder o = do
  encoder <- newHandle (CallSite "ghostty_key_encoder_new") ghosttyKeyEncoderNew
  event <- newHandle (CallSite "ghostty_key_event_new") ghosttyKeyEventNew `onException` ghosttyKeyEncoderFree encoder
  configured (`setKeyOptions` o) closeKeyEncoder KeyEncoder {encoder, event}

-- | Free the event and the encoder.
closeKeyEncoder :: KeyEncoder k -> IO ()
closeKeyEncoder enc = ghosttyKeyEventFree enc.event >> ghosttyKeyEncoderFree enc.encoder

-- | @ghostty_key_encoder_set@, for every option. Each value is one the option
-- takes, so a refusal is an invariant.
setKeyOptions :: KeyEncoder k -> KeyOptions -> IO ()
setKeyOptions enc o = do
  flag GhosttyKeyEncoderOptCursorKeyApplication o.cursorKeyApplication
  flag GhosttyKeyEncoderOptKeypadKeyApplication o.keypadKeyApplication
  flag GhosttyKeyEncoderOptIgnoreKeypadWithNumlock o.ignoreKeypadWithNumlock
  flag GhosttyKeyEncoderOptAltEscPrefix o.altEscPrefix
  flag GhosttyKeyEncoderOptModifyOtherKeysState2 o.modifyOtherKeys2
  set GhosttyKeyEncoderOptKittyFlags (kittyFlagBits o.kittyFlags)
  set GhosttyKeyEncoderOptMacosOptionAsAlt (optionAsAlt o.macosOptionAsAlt)
  flag GhosttyKeyEncoderOptBackarrowKeyMode o.backarrowKeyMode
  where
    set :: (Storable a) => GhosttyKeyEncoderOption -> a -> IO ()
    set opt v = with v $ \p -> expectSuccess (CallSite "ghostty_key_encoder_set") =<< ghosttyKeyEncoderSet enc.encoder opt (castPtr p)
    flag opt b = set opt (fromBool b :: CBool)

-- | @ghostty_key_encoder_set_from_terminal@: the options the terminal's
-- modes and keyboard flags set. Option-as-Alt is not a terminal's, and
-- becomes 'OptionNotAlt'.
keyOptionsFromTerminal :: KeyEncoder k -> Terminal s -> IO ()
keyOptionsFromTerminal enc t =
  expectSuccess (CallSite "ghostty_key_encoder_set_from_terminal")
    =<< ghosttyKeyEncoderSetFromTerminal enc.encoder t.handle

-- | Fill in the encoder's event, and @ghostty_key_encoder_encode@ it. The
-- event borrows the text, which is alive until the encode returns.
encodeKeyEvent :: KeyEncoder k -> KeyEvent -> IO ByteString
encodeKeyEvent enc e = do
  -- Each setter refuses only a null event or a value it does not know, and
  -- every value here is one the binding names: a refusal is an invariant.
  setEvent "ghostty_key_event_set_action" . ghosttyKeyEventSetAction enc.event $ case e.action of
    KeyRelease -> GhosttyKeyActionRelease
    KeyPress -> GhosttyKeyActionPress
    KeyRepeat -> GhosttyKeyActionRepeat
  setEvent "ghostty_key_event_set_key" $ ghosttyKeyEventSetKey enc.event (keyCode e.key)
  setEvent "ghostty_key_event_set_mods" $ ghosttyKeyEventSetMods enc.event (modsBits e.mods)
  setEvent "ghostty_key_event_set_consumed_mods" $ ghosttyKeyEventSetConsumedMods enc.event (modsBits e.consumedMods)
  setEvent "ghostty_key_event_set_composing" $ ghosttyKeyEventSetComposing enc.event (fromBool e.composing)
  setEvent "ghostty_key_event_set_unshifted_codepoint" $
    ghosttyKeyEventSetUnshiftedCodepoint enc.event (maybe 0 (fromIntegral . ord) e.unshifted)
  -- The header asks for no C0 control or DEL in the text: the encoder
  -- derives a control key from the key itself.
  let utf8 = encodeUtf8 (T.filter (\c -> c >= ' ' && c /= '\DEL') e.text) :: ByteString
      encode = encodeVia (CallSite "ghostty_key_encoder_encode") (ghosttyKeyEncoderEncode enc.encoder enc.event)
      setText = setEvent "ghostty_key_event_set_utf8" . ghosttyKeyEventSetUtf8 enc.event
  withString utf8 $ \p -> do
    -- An empty string clears the text.
    setText p
    -- The event keeps the pointer; leave none behind that outlives it,
    -- even when the encode throws. Empty text left nothing to clear.
    if BS.null utf8 then encode else encode `finally` with emptyString setText

-- | The binding's key.
keyCode :: Key -> GhosttyKey
keyCode = \case
  KeyUnidentified -> GhosttyKeyUnidentified
  KeyBackquote -> GhosttyKeyBackquote
  KeyBackslash -> GhosttyKeyBackslash
  KeyBracketLeft -> GhosttyKeyBracketLeft
  KeyBracketRight -> GhosttyKeyBracketRight
  KeyComma -> GhosttyKeyComma
  KeyDigit0 -> GhosttyKeyDigit0
  KeyDigit1 -> GhosttyKeyDigit1
  KeyDigit2 -> GhosttyKeyDigit2
  KeyDigit3 -> GhosttyKeyDigit3
  KeyDigit4 -> GhosttyKeyDigit4
  KeyDigit5 -> GhosttyKeyDigit5
  KeyDigit6 -> GhosttyKeyDigit6
  KeyDigit7 -> GhosttyKeyDigit7
  KeyDigit8 -> GhosttyKeyDigit8
  KeyDigit9 -> GhosttyKeyDigit9
  KeyEqual -> GhosttyKeyEqual
  KeyIntlBackslash -> GhosttyKeyIntlBackslash
  KeyIntlRo -> GhosttyKeyIntlRo
  KeyIntlYen -> GhosttyKeyIntlYen
  KeyA -> GhosttyKeyA
  KeyB -> GhosttyKeyB
  KeyC -> GhosttyKeyC
  KeyD -> GhosttyKeyD
  KeyE -> GhosttyKeyE
  KeyF -> GhosttyKeyF
  KeyG -> GhosttyKeyG
  KeyH -> GhosttyKeyH
  KeyI -> GhosttyKeyI
  KeyJ -> GhosttyKeyJ
  KeyK -> GhosttyKeyK
  KeyL -> GhosttyKeyL
  KeyM -> GhosttyKeyM
  KeyN -> GhosttyKeyN
  KeyO -> GhosttyKeyO
  KeyP -> GhosttyKeyP
  KeyQ -> GhosttyKeyQ
  KeyR -> GhosttyKeyR
  KeyS -> GhosttyKeyS
  KeyT -> GhosttyKeyT
  KeyU -> GhosttyKeyU
  KeyV -> GhosttyKeyV
  KeyW -> GhosttyKeyW
  KeyX -> GhosttyKeyX
  KeyY -> GhosttyKeyY
  KeyZ -> GhosttyKeyZ
  KeyMinus -> GhosttyKeyMinus
  KeyPeriod -> GhosttyKeyPeriod
  KeyQuote -> GhosttyKeyQuote
  KeySemicolon -> GhosttyKeySemicolon
  KeySlash -> GhosttyKeySlash
  KeyAltLeft -> GhosttyKeyAltLeft
  KeyAltRight -> GhosttyKeyAltRight
  KeyBackspace -> GhosttyKeyBackspace
  KeyCapsLock -> GhosttyKeyCapsLock
  KeyContextMenu -> GhosttyKeyContextMenu
  KeyControlLeft -> GhosttyKeyControlLeft
  KeyControlRight -> GhosttyKeyControlRight
  KeyEnter -> GhosttyKeyEnter
  KeyMetaLeft -> GhosttyKeyMetaLeft
  KeyMetaRight -> GhosttyKeyMetaRight
  KeyShiftLeft -> GhosttyKeyShiftLeft
  KeyShiftRight -> GhosttyKeyShiftRight
  KeySpace -> GhosttyKeySpace
  KeyTab -> GhosttyKeyTab
  KeyConvert -> GhosttyKeyConvert
  KeyKanaMode -> GhosttyKeyKanaMode
  KeyNonConvert -> GhosttyKeyNonConvert
  KeyDelete -> GhosttyKeyDelete
  KeyEnd -> GhosttyKeyEnd
  KeyHelp -> GhosttyKeyHelp
  KeyHome -> GhosttyKeyHome
  KeyInsert -> GhosttyKeyInsert
  KeyPageDown -> GhosttyKeyPageDown
  KeyPageUp -> GhosttyKeyPageUp
  KeyArrowDown -> GhosttyKeyArrowDown
  KeyArrowLeft -> GhosttyKeyArrowLeft
  KeyArrowRight -> GhosttyKeyArrowRight
  KeyArrowUp -> GhosttyKeyArrowUp
  KeyNumLock -> GhosttyKeyNumLock
  KeyNumpad0 -> GhosttyKeyNumpad0
  KeyNumpad1 -> GhosttyKeyNumpad1
  KeyNumpad2 -> GhosttyKeyNumpad2
  KeyNumpad3 -> GhosttyKeyNumpad3
  KeyNumpad4 -> GhosttyKeyNumpad4
  KeyNumpad5 -> GhosttyKeyNumpad5
  KeyNumpad6 -> GhosttyKeyNumpad6
  KeyNumpad7 -> GhosttyKeyNumpad7
  KeyNumpad8 -> GhosttyKeyNumpad8
  KeyNumpad9 -> GhosttyKeyNumpad9
  KeyNumpadAdd -> GhosttyKeyNumpadAdd
  KeyNumpadBackspace -> GhosttyKeyNumpadBackspace
  KeyNumpadClear -> GhosttyKeyNumpadClear
  KeyNumpadClearEntry -> GhosttyKeyNumpadClearEntry
  KeyNumpadComma -> GhosttyKeyNumpadComma
  KeyNumpadDecimal -> GhosttyKeyNumpadDecimal
  KeyNumpadDivide -> GhosttyKeyNumpadDivide
  KeyNumpadEnter -> GhosttyKeyNumpadEnter
  KeyNumpadEqual -> GhosttyKeyNumpadEqual
  KeyNumpadMemoryAdd -> GhosttyKeyNumpadMemoryAdd
  KeyNumpadMemoryClear -> GhosttyKeyNumpadMemoryClear
  KeyNumpadMemoryRecall -> GhosttyKeyNumpadMemoryRecall
  KeyNumpadMemoryStore -> GhosttyKeyNumpadMemoryStore
  KeyNumpadMemorySubtract -> GhosttyKeyNumpadMemorySubtract
  KeyNumpadMultiply -> GhosttyKeyNumpadMultiply
  KeyNumpadParenLeft -> GhosttyKeyNumpadParenLeft
  KeyNumpadParenRight -> GhosttyKeyNumpadParenRight
  KeyNumpadSubtract -> GhosttyKeyNumpadSubtract
  KeyNumpadSeparator -> GhosttyKeyNumpadSeparator
  KeyNumpadUp -> GhosttyKeyNumpadUp
  KeyNumpadDown -> GhosttyKeyNumpadDown
  KeyNumpadRight -> GhosttyKeyNumpadRight
  KeyNumpadLeft -> GhosttyKeyNumpadLeft
  KeyNumpadBegin -> GhosttyKeyNumpadBegin
  KeyNumpadHome -> GhosttyKeyNumpadHome
  KeyNumpadEnd -> GhosttyKeyNumpadEnd
  KeyNumpadInsert -> GhosttyKeyNumpadInsert
  KeyNumpadDelete -> GhosttyKeyNumpadDelete
  KeyNumpadPageUp -> GhosttyKeyNumpadPageUp
  KeyNumpadPageDown -> GhosttyKeyNumpadPageDown
  KeyEscape -> GhosttyKeyEscape
  KeyF1 -> GhosttyKeyF1
  KeyF2 -> GhosttyKeyF2
  KeyF3 -> GhosttyKeyF3
  KeyF4 -> GhosttyKeyF4
  KeyF5 -> GhosttyKeyF5
  KeyF6 -> GhosttyKeyF6
  KeyF7 -> GhosttyKeyF7
  KeyF8 -> GhosttyKeyF8
  KeyF9 -> GhosttyKeyF9
  KeyF10 -> GhosttyKeyF10
  KeyF11 -> GhosttyKeyF11
  KeyF12 -> GhosttyKeyF12
  KeyF13 -> GhosttyKeyF13
  KeyF14 -> GhosttyKeyF14
  KeyF15 -> GhosttyKeyF15
  KeyF16 -> GhosttyKeyF16
  KeyF17 -> GhosttyKeyF17
  KeyF18 -> GhosttyKeyF18
  KeyF19 -> GhosttyKeyF19
  KeyF20 -> GhosttyKeyF20
  KeyF21 -> GhosttyKeyF21
  KeyF22 -> GhosttyKeyF22
  KeyF23 -> GhosttyKeyF23
  KeyF24 -> GhosttyKeyF24
  KeyF25 -> GhosttyKeyF25
  KeyFn -> GhosttyKeyFn
  KeyFnLock -> GhosttyKeyFnLock
  KeyPrintScreen -> GhosttyKeyPrintScreen
  KeyScrollLock -> GhosttyKeyScrollLock
  KeyPause -> GhosttyKeyPause
  KeyBrowserBack -> GhosttyKeyBrowserBack
  KeyBrowserFavorites -> GhosttyKeyBrowserFavorites
  KeyBrowserForward -> GhosttyKeyBrowserForward
  KeyBrowserHome -> GhosttyKeyBrowserHome
  KeyBrowserRefresh -> GhosttyKeyBrowserRefresh
  KeyBrowserSearch -> GhosttyKeyBrowserSearch
  KeyBrowserStop -> GhosttyKeyBrowserStop
  KeyEject -> GhosttyKeyEject
  KeyLaunchApp1 -> GhosttyKeyLaunchApp1
  KeyLaunchApp2 -> GhosttyKeyLaunchApp2
  KeyLaunchMail -> GhosttyKeyLaunchMail
  KeyMediaPlayPause -> GhosttyKeyMediaPlayPause
  KeyMediaSelect -> GhosttyKeyMediaSelect
  KeyMediaStop -> GhosttyKeyMediaStop
  KeyMediaTrackNext -> GhosttyKeyMediaTrackNext
  KeyMediaTrackPrevious -> GhosttyKeyMediaTrackPrevious
  KeyPower -> GhosttyKeyPower
  KeySleep -> GhosttyKeySleep
  KeyAudioVolumeDown -> GhosttyKeyAudioVolumeDown
  KeyAudioVolumeMute -> GhosttyKeyAudioVolumeMute
  KeyAudioVolumeUp -> GhosttyKeyAudioVolumeUp
  KeyWakeUp -> GhosttyKeyWakeUp
  KeyCopy -> GhosttyKeyCopy
  KeyCut -> GhosttyKeyCut
  KeyPaste -> GhosttyKeyPaste

-- | The modifiers as the header's bits: a modifier's own bit, and its side's
-- bit when it is the right one.
modsBits :: Mods -> GhosttyMods
modsBits m =
  flagBits $
    concatMap
      held
      [ (m.shift, GhosttyModsShift, GhosttyModsShiftSide),
        (m.ctrl, GhosttyModsCtrl, GhosttyModsCtrlSide),
        (m.alt, GhosttyModsAlt, GhosttyModsAltSide),
        (m.super, GhosttyModsSuper, GhosttyModsSuperSide)
      ]
      <> [(m.capsLock, GhosttyModsCapsLock), (m.numLock, GhosttyModsNumLock)]
  where
    held (side, own, right) = [(isJust side, own), (side == Just RightSide, right)]

-- | The binding's value for macOS's Option.
optionAsAlt :: OptionAsAlt -> GhosttyOptionAsAlt
optionAsAlt = \case
  OptionNotAlt -> GhosttyOptionAsAltFalse
  OptionAlt -> GhosttyOptionAsAltTrue
  OptionLeftAlt -> GhosttyOptionAsAltLeft
  OptionRightAlt -> GhosttyOptionAsAltRight

-- * Mouse encoders

-- | A mouse encoder, alive in the region @m@ that
-- 'Ghostty.Effectful.Encode.withMouseEncoder' opens, with the event handle
-- it encodes from. Its role is nominal, as 'KeyEncoder''s is.
type role MouseEncoder nominal

data MouseEncoder m = MouseEncoder
  { -- | the library's encoder
    encoder :: GhosttyMouseEncoder,
    -- | the event each encode fills in
    event :: GhosttyMouseEvent
  }

-- | How mouse events are encoded: which the program asked for, in which
-- format, and the geometry that turns pixels into cells.
data MouseOptions = MouseOptions
  { -- | which events are reported
    tracking :: MouseTracking,
    -- | how they are written
    format :: MouseFormat,
    -- | the surface the positions are on
    geometry :: MouseGeometry,
    -- | whether a button is held, which decides whether a motion is a drag
    anyButtonPressed :: Bool,
    -- | report a motion only when it reaches another cell (or pixel, for
    -- 'SgrPixelsFormat'): consecutive motions within one are dropped
    trackLastCell :: Bool
  }
  deriving stock (Eq, Show)

-- | No tracking (nothing is reported until the program asks), the X10
-- format, no button held, and every motion reported, on the given surface.
mouseOptions :: MouseGeometry -> MouseOptions
mouseOptions g =
  MouseOptions
    { tracking = NoTracking,
      format = X10Format,
      geometry = g,
      anyButtonPressed = False,
      trackLastCell = False
    }

-- | Which mouse events a program asked for.
data MouseTracking
  = -- | none
    NoTracking
  | -- | presses only (mode 9)
    X10Tracking
  | -- | presses and releases (mode 1000)
    NormalTracking
  | -- | and motion while a button is held (mode 1002)
    ButtonTracking
  | -- | and every motion (mode 1003)
    AnyTracking
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | How mouse events are written.
data MouseFormat
  = -- | X10's: a byte per coordinate
    X10Format
  | -- | UTF-8 coordinates (mode 1005)
    Utf8Format
  | -- | SGR (mode 1006): @CSI < b ; x ; y M@ or @m@
    SgrFormat
  | -- | URxvt's (mode 1015)
    UrxvtFormat
  | -- | SGR in pixels (mode 1016)
    SgrPixelsFormat
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The surface a mouse moves over, in pixels. A cell is never zero
-- pixels along either axis, which the library would ignore.
data MouseGeometry = MouseGeometry
  { -- | the whole surface's width
    screenWidth :: Word32,
    -- | its height
    screenHeight :: Word32,
    -- | a cell's width
    cellWidth :: Extent,
    -- | a cell's height
    cellHeight :: Extent,
    -- | the margins around the grid
    padding :: Padding
  }
  deriving stock (Eq, Show)

-- | The margins between a surface's edges and its grid, in pixels.
data Padding = Padding
  { -- | above
    top :: Word32,
    -- | below
    bottom :: Word32,
    -- | to the right
    right :: Word32,
    -- | to the left
    left :: Word32
  }
  deriving stock (Eq, Show)

-- | No margins.
noPadding :: Padding
noPadding = Padding 0 0 0 0

-- | A surface that is exactly a grid of cells of the given size, with no
-- margins: a terminal's, in a host that draws whole cells.
gridGeometry :: GridSize -> Extent -> Extent -> MouseGeometry
gridGeometry g w h =
  MouseGeometry
    { screenWidth = fromIntegral (fromExtent g.cols) * fromIntegral (fromExtent w),
      screenHeight = fromIntegral (fromExtent g.rows) * fromIntegral (fromExtent h),
      cellWidth = w,
      cellHeight = h,
      padding = noPadding
    }

-- | @ghostty_mouse_encoder_new@ and @ghostty_mouse_event_new@, with the
-- options set. Either handle is freed if the other cannot be made.
openMouseEncoder :: MouseOptions -> IO (MouseEncoder m)
openMouseEncoder o = do
  encoder <- newHandle (CallSite "ghostty_mouse_encoder_new") ghosttyMouseEncoderNew
  event <- newHandle (CallSite "ghostty_mouse_event_new") ghosttyMouseEventNew `onException` ghosttyMouseEncoderFree encoder
  configured (`setMouseOptions` o) closeMouseEncoder MouseEncoder {encoder, event}

-- | Free the event and the encoder.
closeMouseEncoder :: MouseEncoder m -> IO ()
closeMouseEncoder enc = ghosttyMouseEventFree enc.event >> ghosttyMouseEncoderFree enc.encoder

-- | @ghostty_mouse_encoder_set@, for every option. A change of tracking,
-- format or geometry forgets the last cell reported. Each value is one the
-- option takes (the geometry's cell sizes are 'Extent's, never zero), so a
-- refusal is an invariant.
setMouseOptions :: MouseEncoder m -> MouseOptions -> IO ()
setMouseOptions enc o = do
  set GhosttyMouseEncoderOptEvent (trackingCode o.tracking)
  set GhosttyMouseEncoderOptFormat (formatCode o.format)
  set GhosttyMouseEncoderOptSize (geometrySize o.geometry)
  set GhosttyMouseEncoderOptAnyButtonPressed (fromBool o.anyButtonPressed :: CBool)
  set GhosttyMouseEncoderOptTrackLastCell (fromBool o.trackLastCell :: CBool)
  where
    set :: (Storable a) => GhosttyMouseEncoderOption -> a -> IO ()
    set opt v = with v $ \p -> expectSuccess (CallSite "ghostty_mouse_encoder_set") =<< ghosttyMouseEncoderSet enc.encoder opt (castPtr p)

-- | @ghostty_mouse_encoder_set_from_terminal@: the tracking and format
-- the terminal's program asked for. The geometry and the held button stay;
-- the last cell reported is forgotten.
mouseOptionsFromTerminal :: MouseEncoder m -> Terminal s -> IO ()
mouseOptionsFromTerminal enc t =
  expectSuccess (CallSite "ghostty_mouse_encoder_set_from_terminal")
    =<< ghosttyMouseEncoderSetFromTerminal enc.encoder t.handle

-- | @ghostty_mouse_encoder_reset@: forget the last cell reported.
resetMouse :: MouseEncoder m -> IO ()
resetMouse enc = expectSuccess (CallSite "ghostty_mouse_encoder_reset") =<< ghosttyMouseEncoderReset enc.encoder

-- | Fill in the encoder's event, and @ghostty_mouse_encoder_encode@ it.
-- An event the options do not report encodes as nothing.
encodeMouseEvent :: MouseEncoder m -> MouseEvent -> IO ByteString
encodeMouseEvent enc e = do
  -- As for keys: every value here is one the binding names, so a refusal
  -- is an invariant.
  setEvent "ghostty_mouse_event_set_action" . ghosttyMouseEventSetAction enc.event $ case e.action of
    MousePress -> GhosttyMouseActionPress
    MouseRelease -> GhosttyMouseActionRelease
    MouseMotion -> GhosttyMouseActionMotion
  case e.button of
    Nothing -> setEvent "ghostty_mouse_event_clear_button" $ ghosttyMouseEventClearButton enc.event
    Just b -> setEvent "ghostty_mouse_event_set_button" $ ghosttyMouseEventSetButton enc.event (buttonCode b)
  setEvent "ghostty_mouse_event_set_mods" $ ghosttyMouseEventSetMods enc.event (modsBits e.mods)
  setEvent "ghostty_mouse_event_set_position" $
    with (GhosttyMousePosition (realToFrac e.position.x) (realToFrac e.position.y)) $
      ghosttyMouseEventSetPosition enc.event
  encodeVia (CallSite "ghostty_mouse_encoder_encode") (ghosttyMouseEncoderEncode enc.encoder enc.event)

-- | An event setter's call, which refuses only what the binding never passes:
-- any refusal is 'Invariant' at the named function.
setEvent :: Text -> IO GhosttyResult -> IO ()
setEvent site call = expectSuccess (CallSite site) =<< call

-- | The binding's tracking mode.
trackingCode :: MouseTracking -> GhosttyMouseTrackingMode
trackingCode = \case
  NoTracking -> GhosttyMouseTrackingNone
  X10Tracking -> GhosttyMouseTrackingX10
  NormalTracking -> GhosttyMouseTrackingNormal
  ButtonTracking -> GhosttyMouseTrackingButton
  AnyTracking -> GhosttyMouseTrackingAny

-- | The binding's format.
formatCode :: MouseFormat -> GhosttyMouseFormat
formatCode = \case
  X10Format -> GhosttyMouseFormatX10
  Utf8Format -> GhosttyMouseFormatUtf8
  SgrFormat -> GhosttyMouseFormatSgr
  UrxvtFormat -> GhosttyMouseFormatUrxvt
  SgrPixelsFormat -> GhosttyMouseFormatSgrPixels

-- | The binding's geometry, a sized struct.
geometrySize :: MouseGeometry -> GhosttyMouseEncoderSize
geometrySize g = withSize $ \size ->
  GhosttyMouseEncoderSize
    { size,
      screenWidth = g.screenWidth,
      screenHeight = g.screenHeight,
      cellWidth = fromIntegral (fromExtent g.cellWidth),
      cellHeight = fromIntegral (fromExtent g.cellHeight),
      paddingTop = g.padding.top,
      paddingBottom = g.padding.bottom,
      paddingRight = g.padding.right,
      paddingLeft = g.padding.left
    }

-- | The binding's button.
buttonCode :: MouseButton -> GhosttyMouseButton
buttonCode = \case
  MouseUnknownButton -> GhosttyMouseButtonUnknownValue
  MouseLeft -> GhosttyMouseButtonLeft
  MouseRight -> GhosttyMouseButtonRight
  MouseMiddle -> GhosttyMouseButtonMiddle
  MouseFour -> GhosttyMouseButtonFour
  MouseFive -> GhosttyMouseButtonFive
  MouseSix -> GhosttyMouseButtonSix
  MouseSeven -> GhosttyMouseButtonSeven
  MouseEight -> GhosttyMouseButtonEight
  MouseNine -> GhosttyMouseButtonNine
  MouseTen -> GhosttyMouseButtonTen
  MouseEleven -> GhosttyMouseButtonEleven

-- * Reports

-- | A change of focus, which a program that set mode 1004 is told of.
data Focus
  = -- | the terminal gained focus (@CSI I@)
    FocusGained
  | -- | it lost focus (@CSI O@)
    FocusLost
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | @ghostty_focus_encode@.
encodeFocusEvent :: Focus -> IO ByteString
encodeFocusEvent f = encodeVia (CallSite "ghostty_focus_encode") $ ghosttyFocusEncode $ case f of
  FocusGained -> GhosttyFocusGained
  FocusLost -> GhosttyFocusLost

-- | Which size report to write.
data SizeReportStyle
  = -- | the in-band report of mode 2048: @CSI 48 ; rows ; cols ; height ; width t@
    ReportInBand
  | -- | the text area in pixels, the answer to @CSI 14 t@: @CSI 4 ; height ; width t@
    ReportTextAreaPixels
  | -- | a cell in pixels, the answer to @CSI 16 t@: @CSI 6 ; height ; width t@
    ReportCellPixels
  | -- | the text area in cells, the answer to @CSI 18 t@: @CSI 8 ; rows ; cols t@
    ReportTextAreaCells
  | -- | the screen in cells, the answer to @CSI 19 t@: @CSI 9 ; rows ; cols t@.
    -- The physical screen is not known, so this is the text area too.
    ReportScreenCells
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | @ghostty_size_report_encode@, through its by-value shim (§3.3).
encodeSizeReportOf :: SizeReportStyle -> GridSize -> CellPixels -> IO ByteString
encodeSizeReportOf style g px =
  with (GhosttySizeReportSize (fromExtent g.rows) (fromExtent g.cols) px.width px.height) $ \sp ->
    encodeVia (CallSite "ghostty_size_report_encode") (ghosttySizeReportEncode code sp)
  where
    code = case style of
      ReportInBand -> GhosttySizeReportMode2048
      ReportTextAreaPixels -> GhosttySizeReportCsi14T
      ReportCellPixels -> GhosttySizeReportCsi16T
      ReportTextAreaCells -> GhosttySizeReportCsi18T
      ReportScreenCells -> GhosttySizeReportCsi19T

-- | A mode by number, whether the library recognises it or not: the
-- header's packed @GhosttyMode@, a 15-bit number and whether it is ANSI.
newtype ModeNumber = ModeNumber GhosttyMode
  deriving stock (Eq, Show)

-- | Which numbering a mode's number is in.
data ModeFamily
  = -- | set with @CSI n h@
    AnsiMode
  | -- | set with @CSI ? n h@
    DecPrivateMode
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The mode of the given number, if it fits the header's 15 bits.
modeNumber :: ModeFamily -> Word16 -> Maybe ModeNumber
modeNumber family n =
  ModeNumber (ghosttyModeNew n (fromBool (family == AnsiMode))) <$ guard (n <= 0x7fff)

-- | A mode the library recognises, by its number.
knownMode :: ModeKey r -> ModeNumber
knownMode = ModeNumber . modeCode

-- | What a DECRPM reply says of a mode.
data ModeReport
  = -- | the terminal does not know the mode
    ModeNotRecognized
  | -- | it is set
    ModeIsSet
  | -- | it is reset
    ModeIsReset
  | -- | it is set and cannot be changed
    ModePermanentlySet
  | -- | it is reset and cannot be changed
    ModePermanentlyReset
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | @ghostty_mode_report_encode@: @CSI ? n ; s $ y@ for a DEC private mode,
-- @CSI n ; s $ y@ for an ANSI one.
encodeModeReportOf :: ModeNumber -> ModeReport -> IO ByteString
encodeModeReportOf (ModeNumber m) s =
  encodeVia (CallSite "ghostty_mode_report_encode") (ghosttyModeReportEncode m (reportState s))
  where
    reportState :: ModeReport -> GhosttyModeReportState
    reportState = \case
      ModeNotRecognized -> GhosttyModeReportNotRecognized
      ModeIsSet -> GhosttyModeReportSet
      ModeIsReset -> GhosttyModeReportReset
      ModePermanentlySet -> GhosttyModeReportPermanentlySet
      ModePermanentlyReset -> GhosttyModeReportPermanentlyReset

-- | @ghostty_color_scheme_report_encode@: the reply to a colour-scheme
-- query (@CSI ? 996 n@), or the unsolicited report of mode 2031.
encodeColorSchemeReportOf :: ColorScheme -> IO ByteString
encodeColorSchemeReportOf s =
  encodeVia (CallSite "ghostty_color_scheme_report_encode") (ghosttyColorSchemeReportEncode (colorSchemeCode s))

-- * Paste

-- | A paste refused because its text could inject commands: a line break
-- outside bracketed paste mode, or the bracketed paste terminator inside it
-- (DESIGN.md §4.7, §8.5). Nothing was written. Ask, then paste again with
-- 'AllowUnsafe'.
data Rejected = Rejected
  deriving stock (Eq, Show)

-- | Whether a paste is framed by the bracketed paste markers.
data Bracketing
  = -- | framed, as in bracketed paste mode (2004)
    Bracketed
  | -- | not framed: line feeds become carriage returns
    Unbracketed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Whether to paste text that could inject commands.
data PasteSafety
  = -- | refuse it ('Rejected')
    RefuseUnsafe
  | -- | paste it
    AllowUnsafe
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | @ghostty_paste_is_safe@: whether the text holds nothing that could
-- inject commands in any mode: no line break, no bracketed paste
-- terminator. Stricter than 'pasteInto', which allows line breaks inside a
-- bracketed paste.
pasteSafe :: ByteString -> IO Bool
pasteSafe b = BSU.unsafeUseAsCStringLen b $ \(p, n) -> toBool <$> ghosttyPasteIsSafe p (fromIntegral n)

-- | @ghostty_paste_encode@: the text as a paste writes it, without a
-- terminal: unsafe control bytes become spaces, and it is bracketed or
-- has its line feeds made carriage returns. The call changes its input in
-- place, so each attempt is given a fresh copy ('BS.useAsCStringLen'
-- copies).
encodePasteBytes :: Bracketing -> ByteString -> IO ByteString
encodePasteBytes bracketing b =
  encodeVia (CallSite "ghostty_paste_encode") $ \buf len written ->
    BS.useAsCStringLen b $ \(p, n) ->
      ghosttyPasteEncode p (fromIntegral n) (fromBool (bracketing == Bracketed)) buf len written

-- | The C MIME reader of @cbits/gveff_paste.c@: writes the @GhosttyString@
-- its userdata points at. A MIME reader receives structs by value, which no
-- Haskell function can (§3.3).
foreign import capi "gveff_paste.h &gveff_paste_reader"
  gveffPasteReader :: GhosttyMimeReaderFn

-- | @ghostty_terminal_paste@ of one @text/plain@ representation, framed by
-- the terminal's modes, as a user's paste from the clipboard. The encoding
-- goes to the program through the @WRITE_PTY@ callback, so the result is
-- the events the paste caused (a 'Ghostty.Effectful.Event.WritePty', or
-- none for empty text), as 'feed''s is. @CLIPBOARD_READ@ is never
-- installed, so a paste is never a Kitty paste event (DESIGN.md §4.5).
pasteInto :: Terminal s -> PasteSafety -> ByteString -> IO (Either Rejected [TerminalEvent])
pasteInto t safety text =
  withString text $ \textp ->
    withString "text/plain" $ \mimep -> do
      let request = withSize $ \size ->
            GhosttyPaste
              { size,
                location = GhosttyClipboardLocationStandard,
                source = GhosttyPasteSourceClipboard,
                mimes = mimep,
                mimesLen = 1,
                reader = GhosttyMimeReader {read = gveffPasteReader, userdata = castPtr textp},
                allowUnsafe = fromBool (safety == AllowUnsafe)
              }
      r <- with request $ \rp -> ghosttyTerminalPaste t.handle rp nullPtr
      succeedsUnless ResultRejected site r >>= bool (pure (Left Rejected)) (Right <$> drainEvents t)
  where
    site = CallSite "ghostty_terminal_paste"
