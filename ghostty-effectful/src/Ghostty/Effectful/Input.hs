{-# LANGUAGE DuplicateRecordFields #-}

-- | What the input encoders take (DESIGN.md §4.8, "Ghostty.Effectful.Encode"):
-- keys, modifiers, and key and mouse events, as plain values. An encoder
-- turns one of these into the bytes a terminal program reads.
module Ghostty.Effectful.Input
  ( -- * Keys
    Key (..),
    KeyAction (..),
    KeyEvent (..),
    keyPress,

    -- * Modifiers
    Mods (..),
    Side (..),
    noMods,

    -- * Mice
    MouseEvent (..),
    MouseAction (..),
    MouseButton (..),
    MousePosition (..),
  )
where

-- | A physical key, independent of the keyboard layout (the W3C UI Events
-- @code@ values): 'KeyA' is the key where a US layout has @a@, whatever the
-- layout prints there. The constructors are the header's @GHOSTTY_KEY_*@
-- names, and a test holds them against the header's enum.
data Key
  = KeyUnidentified
  | KeyBackquote
  | KeyBackslash
  | KeyBracketLeft
  | KeyBracketRight
  | KeyComma
  | KeyDigit0
  | KeyDigit1
  | KeyDigit2
  | KeyDigit3
  | KeyDigit4
  | KeyDigit5
  | KeyDigit6
  | KeyDigit7
  | KeyDigit8
  | KeyDigit9
  | KeyEqual
  | KeyIntlBackslash
  | KeyIntlRo
  | KeyIntlYen
  | KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | KeyMinus
  | KeyPeriod
  | KeyQuote
  | KeySemicolon
  | KeySlash
  | KeyAltLeft
  | KeyAltRight
  | KeyBackspace
  | KeyCapsLock
  | KeyContextMenu
  | KeyControlLeft
  | KeyControlRight
  | KeyEnter
  | KeyMetaLeft
  | KeyMetaRight
  | KeyShiftLeft
  | KeyShiftRight
  | KeySpace
  | KeyTab
  | KeyConvert
  | KeyKanaMode
  | KeyNonConvert
  | KeyDelete
  | KeyEnd
  | KeyHelp
  | KeyHome
  | KeyInsert
  | KeyPageDown
  | KeyPageUp
  | KeyArrowDown
  | KeyArrowLeft
  | KeyArrowRight
  | KeyArrowUp
  | KeyNumLock
  | KeyNumpad0
  | KeyNumpad1
  | KeyNumpad2
  | KeyNumpad3
  | KeyNumpad4
  | KeyNumpad5
  | KeyNumpad6
  | KeyNumpad7
  | KeyNumpad8
  | KeyNumpad9
  | KeyNumpadAdd
  | KeyNumpadBackspace
  | KeyNumpadClear
  | KeyNumpadClearEntry
  | KeyNumpadComma
  | KeyNumpadDecimal
  | KeyNumpadDivide
  | KeyNumpadEnter
  | KeyNumpadEqual
  | KeyNumpadMemoryAdd
  | KeyNumpadMemoryClear
  | KeyNumpadMemoryRecall
  | KeyNumpadMemoryStore
  | KeyNumpadMemorySubtract
  | KeyNumpadMultiply
  | KeyNumpadParenLeft
  | KeyNumpadParenRight
  | KeyNumpadSubtract
  | KeyNumpadSeparator
  | KeyNumpadUp
  | KeyNumpadDown
  | KeyNumpadRight
  | KeyNumpadLeft
  | KeyNumpadBegin
  | KeyNumpadHome
  | KeyNumpadEnd
  | KeyNumpadInsert
  | KeyNumpadDelete
  | KeyNumpadPageUp
  | KeyNumpadPageDown
  | KeyEscape
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | KeyF25
  | KeyFn
  | KeyFnLock
  | KeyPrintScreen
  | KeyScrollLock
  | KeyPause
  | KeyBrowserBack
  | KeyBrowserFavorites
  | KeyBrowserForward
  | KeyBrowserHome
  | KeyBrowserRefresh
  | KeyBrowserSearch
  | KeyBrowserStop
  | KeyEject
  | KeyLaunchApp1
  | KeyLaunchApp2
  | KeyLaunchMail
  | KeyMediaPlayPause
  | KeyMediaSelect
  | KeyMediaStop
  | KeyMediaTrackNext
  | KeyMediaTrackPrevious
  | KeyPower
  | KeySleep
  | KeyAudioVolumeDown
  | KeyAudioVolumeMute
  | KeyAudioVolumeUp
  | KeyWakeUp
  | KeyCopy
  | KeyCut
  | KeyPaste
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Whether a key went down, came up, or is held.
data KeyAction
  = -- | released
    KeyRelease
  | -- | pressed
    KeyPress
  | -- | held down, repeating
    KeyRepeat
  deriving stock (Eq, Show, Enum, Bounded)

-- | Which of a pair of modifier keys.
data Side
  = -- | the left one, or the platform could not tell
    LeftSide
  | -- | the right one
    RightSide
  deriving stock (Eq, Show, Enum, Bounded)

-- | The modifiers held during an event. A modifier's side is known only
-- while it is held, so each is a 'Maybe' 'Side' rather than two flags.
data Mods = Mods
  { -- | Shift
    shift :: Maybe Side,
    -- | Control
    ctrl :: Maybe Side,
    -- | Alt, or Option
    alt :: Maybe Side,
    -- | Super, Command or Windows
    super :: Maybe Side,
    -- | Caps Lock is on
    capsLock :: Bool,
    -- | Num Lock is on
    numLock :: Bool
  }
  deriving stock (Eq, Show)

-- | No modifier held, and neither lock on.
noMods :: Mods
noMods = Mods {shift = Nothing, ctrl = Nothing, alt = Nothing, super = Nothing, capsLock = False, numLock = False}

-- | A key event, as a platform reports one.
data KeyEvent = KeyEvent
  { -- | down, up or repeating
    action :: KeyAction,
    -- | the physical key
    key :: Key,
    -- | the modifiers held
    mods :: Mods,
    -- | the modifiers the layout used to produce the text (Shift for @A@),
    -- which the Kitty protocol reports apart from the rest
    consumedMods :: Mods,
    -- | whether the key is part of an input method's composition
    composing :: Bool,
    -- | the text the key produces in the current layout, before Control or
    -- Meta change it; empty for none. C0 control characters
    -- (U+0000–U+001F) and DEL, which the header asks not to be given, are
    -- dropped: those keys are encoded from 'key'. Nor should platform
    -- function-key codes be here.
    text :: Text,
    -- | the character the key produces without Shift; 'Nothing' (as
    -- @'Just' '\\NUL'@) for none. The Kitty protocol finds a text key by
    -- it: without it, the key's press is sent as its text, and its release
    -- not at all.
    unshifted :: Maybe Char
  }
  deriving stock (Eq, Show)

-- | A press of the key with no modifiers, no text and no unshifted
-- character: what a key without text, an arrow or a function key, sends.
--
-- >>> (keyPress KeyArrowUp).action
-- KeyPress
keyPress :: Key -> KeyEvent
keyPress k =
  KeyEvent
    { action = KeyPress,
      key = k,
      mods = noMods,
      consumedMods = noMods,
      composing = False,
      text = "",
      unshifted = Nothing
    }

-- | A mouse event, at a position on the terminal's surface in pixels.
data MouseEvent = MouseEvent
  { -- | pressed, released or moved
    action :: MouseAction,
    -- | the button; 'Nothing' for a motion with no button held
    button :: Maybe MouseButton,
    -- | the modifiers held
    mods :: Mods,
    -- | where, in pixels from the surface's top left
    position :: MousePosition
  }
  deriving stock (Eq, Show)

-- | What a mouse did.
data MouseAction
  = -- | a button was pressed
    MousePress
  | -- | a button was released
    MouseRelease
  | -- | the mouse moved
    MouseMotion
  deriving stock (Eq, Show, Enum, Bounded)

-- | A mouse button. Four and five are the scroll wheel's up and down, six
-- and seven its left and right.
data MouseButton
  = -- | a button the platform could not name
    MouseUnknownButton
  | -- | the left button
    MouseLeft
  | -- | the right button
    MouseRight
  | -- | the middle button
    MouseMiddle
  | -- | button 4: wheel up
    MouseFour
  | -- | button 5: wheel down
    MouseFive
  | -- | button 6: wheel left
    MouseSix
  | -- | button 7: wheel right
    MouseSeven
  | -- | button 8, often back
    MouseEight
  | -- | button 9, often forward
    MouseNine
  | -- | button 10
    MouseTen
  | -- | button 11
    MouseEleven
  deriving stock (Eq, Show, Enum, Bounded)

-- | A point on the terminal's surface, in pixels from its top left,
-- padding included.
data MousePosition = MousePosition
  { -- | across
    x :: Float,
    -- | down
    y :: Float
  }
  deriving stock (Eq, Show)
