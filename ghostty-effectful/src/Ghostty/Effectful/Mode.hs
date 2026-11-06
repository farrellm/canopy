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
  ModeKam :: ModeKey 'Configurable
  -- | ANSI 4, IRM: insert.
  ModeInsert :: ModeKey 'Configurable
  -- | ANSI 12, SRM: send/receive (local echo off when set).
  ModeSrm :: ModeKey 'Configurable
  -- | ANSI 20, LNM: linefeed implies carriage return.
  ModeLinefeed :: ModeKey 'Configurable
  -- | DEC 1, DECCKM: application cursor keys.
  ModeDecckm :: ModeKey 'Configurable
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
  ModeX10Mouse :: ModeKey 'Fixed
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
  ModeLeftRightMargin :: ModeKey 'Fixed
  -- | DEC 1000: normal mouse tracking.
  ModeNormalMouse :: ModeKey 'Fixed
  -- | DEC 1002: button-event mouse tracking.
  ModeButtonMouse :: ModeKey 'Fixed
  -- | DEC 1003: any-event mouse tracking.
  ModeAnyMouse :: ModeKey 'Fixed
  -- | DEC 1004: focus events.
  ModeFocusEvent :: ModeKey 'Configurable
  -- | DEC 1005: UTF-8 mouse encoding.
  ModeUtf8Mouse :: ModeKey 'Fixed
  -- | DEC 1006: SGR mouse encoding.
  ModeSgrMouse :: ModeKey 'Fixed
  -- | DEC 1007: alternate scroll.
  ModeAltScroll :: ModeKey 'Configurable
  -- | DEC 1015: urxvt mouse encoding.
  ModeUrxvtMouse :: ModeKey 'Fixed
  -- | DEC 1016: SGR-pixels mouse encoding.
  ModeSgrPixelsMouse :: ModeKey 'Fixed
  -- | DEC 1035: ignore the keypad's application mode with Num Lock.
  ModeNumlockKeypad :: ModeKey 'Configurable
  -- | DEC 1036: Alt prefixes ESC.
  ModeAltEscPrefix :: ModeKey 'Configurable
  -- | DEC 1039: Alt sends ESC.
  ModeAltSendsEsc :: ModeKey 'Configurable
  -- | DEC 1045: extended reverse wraparound.
  ModeReverseWrapExt :: ModeKey 'Configurable
  -- | DEC 1047: the alternate screen.
  ModeAltScreen :: ModeKey 'Fixed
  -- | DEC 1048: save the cursor.
  ModeSaveCursor :: ModeKey 'Fixed
  -- | DEC 1049: the alternate screen, saving the cursor and clearing.
  ModeAltScreenSave :: ModeKey 'Fixed
  -- | DEC 2004: bracketed paste.
  ModeBracketedPaste :: ModeKey 'Configurable
  -- | DEC 2026: synchronized output.
  ModeSyncOutput :: ModeKey 'Fixed
  -- | DEC 2027: grapheme clustering.
  ModeGraphemeCluster :: ModeKey 'Configurable
  -- | DEC 2031: colour-scheme change reports.
  ModeColorSchemeReport :: ModeKey 'Configurable
  -- | DEC 2033: visibility reports.
  ModeVisibilityReport :: ModeKey 'Fixed
  -- | DEC 2048: in-band size reports.
  ModeInBandResize :: ModeKey 'Configurable
  -- | DEC 5522: kitty clipboard paste events.
  ModePasteEvents :: ModeKey 'Configurable

deriving stock instance Eq (ModeKey r)

deriving stock instance Show (ModeKey r)

-- | The mode, if its reset default may be configured.
configurableMode :: ModeKey r -> Maybe (ModeKey 'Configurable)
configurableMode k = case k of
  ModeKam -> Just k
  ModeInsert -> Just k
  ModeSrm -> Just k
  ModeLinefeed -> Just k
  ModeDecckm -> Just k
  Mode132Column -> Nothing
  ModeSlowScroll -> Just k
  ModeReverseColors -> Just k
  ModeOrigin -> Nothing
  ModeWraparound -> Just k
  ModeAutorepeat -> Just k
  ModeX10Mouse -> Nothing
  ModeCursorBlinking -> Nothing
  ModeCursorVisible -> Just k
  ModeEnableMode3 -> Just k
  ModeReverseWrap -> Just k
  ModeAltScreenLegacy -> Nothing
  ModeKeypadKeys -> Just k
  ModeBackarrowKeyMode -> Just k
  ModeLeftRightMargin -> Nothing
  ModeNormalMouse -> Nothing
  ModeButtonMouse -> Nothing
  ModeAnyMouse -> Nothing
  ModeFocusEvent -> Just k
  ModeUtf8Mouse -> Nothing
  ModeSgrMouse -> Nothing
  ModeAltScroll -> Just k
  ModeUrxvtMouse -> Nothing
  ModeSgrPixelsMouse -> Nothing
  ModeNumlockKeypad -> Just k
  ModeAltEscPrefix -> Just k
  ModeAltSendsEsc -> Just k
  ModeReverseWrapExt -> Just k
  ModeAltScreen -> Nothing
  ModeSaveCursor -> Nothing
  ModeAltScreenSave -> Nothing
  ModeBracketedPaste -> Just k
  ModeSyncOutput -> Nothing
  ModeGraphemeCluster -> Just k
  ModeColorSchemeReport -> Just k
  ModeVisibilityReport -> Nothing
  ModeInBandResize -> Just k
  ModePasteEvents -> Just k

-- | A mode of either kind.
data SomeModeKey where
  -- | the mode
  SomeModeKey :: ModeKey r -> SomeModeKey

deriving stock instance Show SomeModeKey

-- | Every mode, in the header's order.
allModes :: [SomeModeKey]
allModes =
  [ SomeModeKey ModeKam,
    SomeModeKey ModeInsert,
    SomeModeKey ModeSrm,
    SomeModeKey ModeLinefeed,
    SomeModeKey ModeDecckm,
    SomeModeKey Mode132Column,
    SomeModeKey ModeSlowScroll,
    SomeModeKey ModeReverseColors,
    SomeModeKey ModeOrigin,
    SomeModeKey ModeWraparound,
    SomeModeKey ModeAutorepeat,
    SomeModeKey ModeX10Mouse,
    SomeModeKey ModeCursorBlinking,
    SomeModeKey ModeCursorVisible,
    SomeModeKey ModeEnableMode3,
    SomeModeKey ModeReverseWrap,
    SomeModeKey ModeAltScreenLegacy,
    SomeModeKey ModeKeypadKeys,
    SomeModeKey ModeBackarrowKeyMode,
    SomeModeKey ModeLeftRightMargin,
    SomeModeKey ModeNormalMouse,
    SomeModeKey ModeButtonMouse,
    SomeModeKey ModeAnyMouse,
    SomeModeKey ModeFocusEvent,
    SomeModeKey ModeUtf8Mouse,
    SomeModeKey ModeSgrMouse,
    SomeModeKey ModeAltScroll,
    SomeModeKey ModeUrxvtMouse,
    SomeModeKey ModeSgrPixelsMouse,
    SomeModeKey ModeNumlockKeypad,
    SomeModeKey ModeAltEscPrefix,
    SomeModeKey ModeAltSendsEsc,
    SomeModeKey ModeReverseWrapExt,
    SomeModeKey ModeAltScreen,
    SomeModeKey ModeSaveCursor,
    SomeModeKey ModeAltScreenSave,
    SomeModeKey ModeBracketedPaste,
    SomeModeKey ModeSyncOutput,
    SomeModeKey ModeGraphemeCluster,
    SomeModeKey ModeColorSchemeReport,
    SomeModeKey ModeVisibilityReport,
    SomeModeKey ModeInBandResize,
    SomeModeKey ModePasteEvents
  ]
