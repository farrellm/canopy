-- | Terminal modes (ANSI and DEC private) that libghostty-vt recognises
-- (DESIGN.md §4.4).
--
-- A mode's type says whether its reset default can be configured
-- ('Ghostty.Effectful.Terminal.SetModeDefault'). The library refuses that
-- for the modes whose set or reset does more than flip a bit (switching
-- screens, the mouse protocols, synchronized output, …), so those are
-- @'ModeKey' ''Fixed'@ and cannot be passed. Every mode's current value can
-- be read and set. The test suite checks the index of every mode against the
-- linked library, and that this list is every mode the library recognises.
module Ghostty.Effectful.Mode
  ( Resettable (..),
    ModeKey (..),
    configurableMode,
    SomeModeKey (..),
    allModes,
  )
where

-- | Whether a mode's reset default may be configured.
data Resettable
  = -- | it may
    Configurable
  | -- | it may not: setting the mode has effects beyond the mode's bit
    Fixed

-- | A mode. The constructors are the header's @GHOSTTY_MODE_*@ names.
data ModeKey (r :: Resettable) where
  -- | ANSI 2, KAM: keyboard action (locks the keyboard).
  ModeDisableKeyboard :: ModeKey 'Configurable
  -- | ANSI 4, IRM: insert.
  ModeInsert :: ModeKey 'Configurable
  -- | ANSI 12, SRM: send/receive (local echo off when set).
  ModeSendReceiveMode :: ModeKey 'Configurable
  -- | ANSI 20, LNM: linefeed implies carriage return.
  ModeLinefeed :: ModeKey 'Configurable
  -- | DEC 1, DECCKM: application cursor keys.
  ModeCursorKeys :: ModeKey 'Configurable
  -- | DEC 3, DECCOLM: 132 columns.
  Mode132Column :: ModeKey 'Fixed
  -- | DEC 4: smooth scroll.
  ModeSlowScroll :: ModeKey 'Configurable
  -- | DEC 5, DECSCNM: reverse video.
  ModeReverseColors :: ModeKey 'Configurable
  -- | DEC 6, DECOM: origin.
  ModeOrigin :: ModeKey 'Fixed
  -- | DEC 7, DECAWM: autowrap.
  ModeWraparound :: ModeKey 'Configurable
  -- | DEC 8, DECARM: autorepeat.
  ModeAutorepeat :: ModeKey 'Configurable
  -- | DEC 9: X10 mouse reporting.
  ModeMouseEventX10 :: ModeKey 'Fixed
  -- | DEC 12: cursor blinking.
  ModeCursorBlinking :: ModeKey 'Fixed
  -- | DEC 25, DECTCEM: cursor visible.
  ModeCursorVisible :: ModeKey 'Configurable
  -- | DEC 40: allow DECCOLM.
  ModeEnableMode3 :: ModeKey 'Configurable
  -- | DEC 45: reverse wraparound.
  ModeReverseWrap :: ModeKey 'Configurable
  -- | DEC 47: the alternate screen, legacy.
  ModeAltScreenLegacy :: ModeKey 'Fixed
  -- | DEC 66, DECNKM: application keypad.
  ModeKeypadKeys :: ModeKey 'Configurable
  -- | DEC 67, DECBKM: backspace sends BS rather than DEL.
  ModeBackarrowKeyMode :: ModeKey 'Configurable
  -- | DEC 69, DECLRMM: left and right margins.
  ModeEnableLeftAndRightMargin :: ModeKey 'Fixed
  -- | DEC 1000: normal mouse tracking.
  ModeMouseEventNormal :: ModeKey 'Fixed
  -- | DEC 1002: button-event mouse tracking.
  ModeMouseEventButton :: ModeKey 'Fixed
  -- | DEC 1003: any-event mouse tracking.
  ModeMouseEventAny :: ModeKey 'Fixed
  -- | DEC 1004: focus events.
  ModeFocusEvent :: ModeKey 'Configurable
  -- | DEC 1005: UTF-8 mouse encoding.
  ModeMouseFormatUtf8 :: ModeKey 'Fixed
  -- | DEC 1006: SGR mouse encoding.
  ModeMouseFormatSgr :: ModeKey 'Fixed
  -- | DEC 1007: alternate scroll.
  ModeMouseAlternateScroll :: ModeKey 'Configurable
  -- | DEC 1015: urxvt mouse encoding.
  ModeMouseFormatUrxvt :: ModeKey 'Fixed
  -- | DEC 1016: SGR-pixels mouse encoding.
  ModeMouseFormatSgrPixels :: ModeKey 'Fixed
  -- | DEC 1035: ignore the keypad's application mode with Num Lock.
  ModeIgnoreKeypadWithNumlock :: ModeKey 'Configurable
  -- | DEC 1036: Alt prefixes ESC.
  ModeAltEscPrefix :: ModeKey 'Configurable
  -- | DEC 1039: Alt sends ESC.
  ModeAltSendsEscape :: ModeKey 'Configurable
  -- | DEC 1045: extended reverse wraparound.
  ModeReverseWrapExtended :: ModeKey 'Configurable
  -- | DEC 1047: the alternate screen.
  ModeAltScreen :: ModeKey 'Fixed
  -- | DEC 1048: save the cursor.
  ModeSaveCursor :: ModeKey 'Fixed
  -- | DEC 1049: the alternate screen, saving the cursor and clearing.
  ModeAltScreenSaveCursorClearEnter :: ModeKey 'Fixed
  -- | DEC 2004: bracketed paste.
  ModeBracketedPaste :: ModeKey 'Configurable
  -- | DEC 2026: synchronized output.
  ModeSynchronizedOutput :: ModeKey 'Fixed
  -- | DEC 2027: grapheme clustering.
  ModeGraphemeCluster :: ModeKey 'Configurable
  -- | DEC 2031: colour-scheme change reports.
  ModeReportColorScheme :: ModeKey 'Configurable
  -- | DEC 2033: visibility reports.
  ModeReportVisibility :: ModeKey 'Fixed
  -- | DEC 2048: in-band size reports.
  ModeInBandSizeReports :: ModeKey 'Configurable
  -- | DEC 5522: kitty clipboard paste events.
  ModeKittyPasteEvents :: ModeKey 'Configurable

deriving stock instance Eq (ModeKey r)

deriving stock instance Show (ModeKey r)

-- | The mode, if its reset default may be configured.
configurableMode :: ModeKey r -> Maybe (ModeKey 'Configurable)
configurableMode k = case k of
  ModeDisableKeyboard -> Just k
  ModeInsert -> Just k
  ModeSendReceiveMode -> Just k
  ModeLinefeed -> Just k
  ModeCursorKeys -> Just k
  Mode132Column -> Nothing
  ModeSlowScroll -> Just k
  ModeReverseColors -> Just k
  ModeOrigin -> Nothing
  ModeWraparound -> Just k
  ModeAutorepeat -> Just k
  ModeMouseEventX10 -> Nothing
  ModeCursorBlinking -> Nothing
  ModeCursorVisible -> Just k
  ModeEnableMode3 -> Just k
  ModeReverseWrap -> Just k
  ModeAltScreenLegacy -> Nothing
  ModeKeypadKeys -> Just k
  ModeBackarrowKeyMode -> Just k
  ModeEnableLeftAndRightMargin -> Nothing
  ModeMouseEventNormal -> Nothing
  ModeMouseEventButton -> Nothing
  ModeMouseEventAny -> Nothing
  ModeFocusEvent -> Just k
  ModeMouseFormatUtf8 -> Nothing
  ModeMouseFormatSgr -> Nothing
  ModeMouseAlternateScroll -> Just k
  ModeMouseFormatUrxvt -> Nothing
  ModeMouseFormatSgrPixels -> Nothing
  ModeIgnoreKeypadWithNumlock -> Just k
  ModeAltEscPrefix -> Just k
  ModeAltSendsEscape -> Just k
  ModeReverseWrapExtended -> Just k
  ModeAltScreen -> Nothing
  ModeSaveCursor -> Nothing
  ModeAltScreenSaveCursorClearEnter -> Nothing
  ModeBracketedPaste -> Just k
  ModeSynchronizedOutput -> Nothing
  ModeGraphemeCluster -> Just k
  ModeReportColorScheme -> Just k
  ModeReportVisibility -> Nothing
  ModeInBandSizeReports -> Just k
  ModeKittyPasteEvents -> Just k

-- | A mode of either kind.
data SomeModeKey where
  -- | the mode
  SomeModeKey :: ModeKey r -> SomeModeKey

deriving stock instance Show SomeModeKey

-- | Every mode, in the header's order.
allModes :: [SomeModeKey]
allModes =
  [ SomeModeKey ModeDisableKeyboard,
    SomeModeKey ModeInsert,
    SomeModeKey ModeSendReceiveMode,
    SomeModeKey ModeLinefeed,
    SomeModeKey ModeCursorKeys,
    SomeModeKey Mode132Column,
    SomeModeKey ModeSlowScroll,
    SomeModeKey ModeReverseColors,
    SomeModeKey ModeOrigin,
    SomeModeKey ModeWraparound,
    SomeModeKey ModeAutorepeat,
    SomeModeKey ModeMouseEventX10,
    SomeModeKey ModeCursorBlinking,
    SomeModeKey ModeCursorVisible,
    SomeModeKey ModeEnableMode3,
    SomeModeKey ModeReverseWrap,
    SomeModeKey ModeAltScreenLegacy,
    SomeModeKey ModeKeypadKeys,
    SomeModeKey ModeBackarrowKeyMode,
    SomeModeKey ModeEnableLeftAndRightMargin,
    SomeModeKey ModeMouseEventNormal,
    SomeModeKey ModeMouseEventButton,
    SomeModeKey ModeMouseEventAny,
    SomeModeKey ModeFocusEvent,
    SomeModeKey ModeMouseFormatUtf8,
    SomeModeKey ModeMouseFormatSgr,
    SomeModeKey ModeMouseAlternateScroll,
    SomeModeKey ModeMouseFormatUrxvt,
    SomeModeKey ModeMouseFormatSgrPixels,
    SomeModeKey ModeIgnoreKeypadWithNumlock,
    SomeModeKey ModeAltEscPrefix,
    SomeModeKey ModeAltSendsEscape,
    SomeModeKey ModeReverseWrapExtended,
    SomeModeKey ModeAltScreen,
    SomeModeKey ModeSaveCursor,
    SomeModeKey ModeAltScreenSaveCursorClearEnter,
    SomeModeKey ModeBracketedPaste,
    SomeModeKey ModeSynchronizedOutput,
    SomeModeKey ModeGraphemeCluster,
    SomeModeKey ModeReportColorScheme,
    SomeModeKey ModeReportVisibility,
    SomeModeKey ModeInBandSizeReports,
    SomeModeKey ModeKittyPasteEvents
  ]
