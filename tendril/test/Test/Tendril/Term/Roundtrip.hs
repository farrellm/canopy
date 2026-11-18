{-# LANGUAGE DuplicateRecordFields #-}

-- | The decoder's roundtrip (DESIGN.md §5.7, §11.2, M2.8): a key or mouse
-- event, as a platform reports it, under any keyboard or mouse mode, is
-- encoded by libghostty-vt's encoders, decoded by "Tendril.Term.Input", and
-- comes back as the event it means, in D36's normal form.
--
-- What comes back is what the mode can carry, and 'expected' says what that
-- is: the model of each encoding, written from the protocols and checked
-- against libghostty-vt 1.0 on every key, modifier set, lock state, action
-- and mode (M2.8). Where a mode cannot carry an event (super outside the
-- kitty protocol, xterm's C0 table, a release outside it), the event is
-- 'Lost', with the reason, and all that is checked is that its bytes
-- decode cleanly: at most one key, nothing 'Unknown'.
--
-- Each event's bytes are decoded alone, with the escape timeout after them,
-- as a driver reads keys typed apart.
module Test.Tendril.Term.Roundtrip (tests) where

import Data.ByteString qualified as BS
import Data.Char (isAlpha, isLower)
import Data.List (zipWith3)
import Data.Text qualified as T
import Effectful (Eff, IOE, runEff)
import Ghostty.Effectful (Ghostty, runGhostty)
import Ghostty.Effectful.Encode qualified as G
import Ghostty.Effectful.Terminal (GridSize (..), extent, fromExtent)
import Ghostty.Effectful.Terminal.Keys (KittyFlags (..))
import Tendril.Keymap qualified as K
import Tendril.Term.Input qualified as I
import Test.QuickCheck
  ( Gen,
    Property,
    checkCoverage,
    chooseInt,
    counterexample,
    cover,
    elements,
    forAllShow,
    frequency,
    ioProperty,
    oneof,
    suchThatMap,
    tabulate,
    (===),
  )
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

tests :: TestTree
tests =
  testGroup
    "decoder roundtrip through libghostty"
    [ adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 1000)) $
        testProperty "keys come back as they were meant" keysRoundtrip,
      adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 100)) $
        testProperty "mouse reports come back as they were" mouseRoundtrip
    ]

-- * Keys

-- | A key on a US keyboard.
data PhysicalKey
  = -- | a key that types: its character, and its character with shift
    TextKey G.Key Char Char
  | -- | a keypad key that types its character, with Num Lock on
    PadKey G.Key Char
  | -- | a key that types nothing, and the key tendril names it
    Named G.Key K.Key
  deriving stock (Show)

textKeys :: [PhysicalKey]
textKeys =
  zipWith3 TextKey [G.KeyA .. G.KeyZ] ['a' .. 'z'] ['A' .. 'Z']
    <> zipWith3 TextKey [G.KeyDigit0 .. G.KeyDigit9] ['0' .. '9'] ")!@#$%^&*("
    <> [ TextKey G.KeyBackquote '`' '~',
         TextKey G.KeyMinus '-' '_',
         TextKey G.KeyEqual '=' '+',
         TextKey G.KeyBracketLeft '[' '{',
         TextKey G.KeyBracketRight ']' '}',
         TextKey G.KeyBackslash '\\' '|',
         TextKey G.KeySemicolon ';' ':',
         TextKey G.KeyQuote '\'' '"',
         TextKey G.KeyComma ',' '<',
         TextKey G.KeyPeriod '.' '>',
         TextKey G.KeySlash '/' '?',
         TextKey G.KeySpace ' ' ' '
       ]

padKeys :: [PhysicalKey]
padKeys =
  zipWith PadKey [G.KeyNumpad0 .. G.KeyNumpad9] ['0' .. '9']
    <> [ PadKey G.KeyNumpadDecimal '.',
         PadKey G.KeyNumpadDivide '/',
         PadKey G.KeyNumpadMultiply '*',
         PadKey G.KeyNumpadSubtract '-',
         PadKey G.KeyNumpadAdd '+',
         PadKey G.KeyNumpadEqual '='
       ]

-- | The keypad's keys with Num Lock off, which are its logical keys.
padNamedKeys :: [PhysicalKey]
padNamedKeys =
  [ Named G.KeyNumpadEnter K.Enter,
    Named G.KeyNumpadUp K.ArrowUp,
    Named G.KeyNumpadDown K.ArrowDown,
    Named G.KeyNumpadLeft K.ArrowLeft,
    Named G.KeyNumpadRight K.ArrowRight,
    Named G.KeyNumpadBegin K.Begin,
    Named G.KeyNumpadHome K.Home,
    Named G.KeyNumpadEnd K.End,
    Named G.KeyNumpadInsert K.InsertKey,
    Named G.KeyNumpadDelete K.DeleteKey,
    Named G.KeyNumpadPageUp K.PageUp,
    Named G.KeyNumpadPageDown K.PageDown
  ]

namedKeys :: [PhysicalKey]
namedKeys =
  [ Named G.KeyEnter K.Enter,
    Named G.KeyTab K.Tab,
    Named G.KeyBackspace K.Backspace,
    Named G.KeyEscape K.Escape,
    Named G.KeyInsert K.InsertKey,
    Named G.KeyDelete K.DeleteKey,
    Named G.KeyHome K.Home,
    Named G.KeyEnd K.End,
    Named G.KeyPageUp K.PageUp,
    Named G.KeyPageDown K.PageDown,
    Named G.KeyArrowUp K.ArrowUp,
    Named G.KeyArrowDown K.ArrowDown,
    Named G.KeyArrowLeft K.ArrowLeft,
    Named G.KeyArrowRight K.ArrowRight
  ]
    <> zipWith Named [G.KeyF1 .. G.KeyF25] (map K.F [1 .. 25])

-- | Keys the kitty protocol alone reports.
kittyKeys :: [PhysicalKey]
kittyKeys =
  [ Named G.KeyScrollLock K.ScrollLock,
    Named G.KeyPrintScreen K.PrintScreen,
    Named G.KeyPause K.Pause,
    Named G.KeyContextMenu K.ContextMenu
  ]
    <> map (uncurry Named) reportAllKeys
    <> map (uncurry Named) mediaKeys

-- | Keys the kitty protocol reports only with every key as an escape.
reportAllKeys :: [(G.Key, K.Key)]
reportAllKeys =
  [ (G.KeyCapsLock, K.CapsLock),
    (G.KeyNumLock, K.NumLock),
    (G.KeyShiftLeft, K.Modifier K.LeftShift),
    (G.KeyShiftRight, K.Modifier K.RightShift),
    (G.KeyControlLeft, K.Modifier K.LeftControl),
    (G.KeyControlRight, K.Modifier K.RightControl),
    (G.KeyAltLeft, K.Modifier K.LeftAlt),
    (G.KeyAltRight, K.Modifier K.RightAlt),
    (G.KeyMetaLeft, K.Modifier K.LeftSuper),
    (G.KeyMetaRight, K.Modifier K.RightSuper)
  ]

-- | Media keys, which libghostty-vt 1.0 encodes in no mode: they are here so
-- that the property says so, and the unit tests decode their kitty codes.
mediaKeys :: [(G.Key, K.Key)]
mediaKeys =
  [ (G.KeyMediaPlayPause, K.Media K.MediaPlayPause),
    (G.KeyMediaStop, K.Media K.MediaStop),
    (G.KeyMediaTrackNext, K.Media K.MediaTrackNext),
    (G.KeyMediaTrackPrevious, K.Media K.MediaTrackPrevious),
    (G.KeyAudioVolumeDown, K.Media K.VolumeDown),
    (G.KeyAudioVolumeUp, K.Media K.VolumeUp),
    (G.KeyAudioVolumeMute, K.Media K.VolumeMute)
  ]

-- | A key event as a platform reports it: the key, the modifiers and locks,
-- and whether it went down, repeats or came up.
data Stroke = Stroke
  { key :: PhysicalKey,
    mods :: G.Mods,
    action :: G.KeyAction
  }
  deriving stock (Show)

-- | The modifiers held, as tendril names them.
held :: G.Mods -> K.Mods
held m = K.Mods {ctrl = isJust m.ctrl, alt = isJust m.alt, shift = isJust m.shift, super = isJust m.super}

-- | The character a stroke types: with shift, the shifted one, and with
-- Caps Lock, a letter's other case.
typedChar :: Stroke -> Maybe Char
typedChar s = case s.key of
  TextKey _ u sh
    | isLower u && s.mods.capsLock -> Just (if isJust s.mods.shift then u else sh)
    | isJust s.mods.shift -> Just sh
    | otherwise -> Just u
  PadKey _ c -> Just c
  Named {} -> Nothing

-- | The event a platform gives the encoder: the text the key types, and its
-- unshifted character, which the kitty encoder finds a text key by (§11.2).
platform :: Stroke -> G.KeyEvent
platform s = case s.key of
  TextKey k u _ ->
    let c = fromMaybe u (typedChar s)
     in typing k c u (G.noMods {G.shift = if c /= u then s.mods.shift else Nothing})
  PadKey k c -> typing k c c G.noMods
  Named k _ -> (G.keyPress k) {G.composing = False, G.action = s.action, G.mods = s.mods}
  where
    typing k c u consumed =
      (G.keyPress k)
        { G.composing = False,
          G.action = s.action,
          G.mods = s.mods,
          G.consumedMods = consumed,
          G.text = T.singleton c,
          G.unshifted = Just u
        }

-- | What decoding a stroke's bytes should give.
data Expect
  = -- | exactly these events
    Decodes [I.InputEvent]
  | -- | the mode cannot carry the stroke, for this reason
    Lost Text

-- | A press of a chord, with nothing else reported.
pressOf :: K.KeyChord -> Expect
pressOf c = Decodes [I.Key (I.keyEvent c)]

-- | The chord a text key's stroke means, where the terminal says what it
-- typed: the character, with shift cleared where it changed it; Caps Lock
-- gives a letter its other case only with no ctrl, alt or super (§5.7).
meant :: Stroke -> Char -> Char -> K.KeyChord
meant s u sh
  | isLower u && s.mods.capsLock && not (others m) = K.chord m {K.shift = False} (K.Char (if m.shift then u else sh))
  | m.shift && sh /= u = K.chord m {K.shift = False} (K.Char sh)
  | otherwise = K.chord m (K.Char u)
  where
    m = held s.mods

-- | Ctrl, alt or super, which make a key not type.
others :: K.Mods -> Bool
others m = m.ctrl || m.alt || m.super

-- | Whether the options are the kitty protocol's: any flag is.
kitty :: G.KeyOptions -> Bool
kitty o = o.kittyFlags /= KittyFlags False False False False False

-- | What decoding a stroke's bytes, encoded under the options, should give.
expected :: G.KeyOptions -> Stroke -> Expect
expected o s
  | kitty o = kittyExpect o s
  | otherwise = legacyExpect o s

-- | The legacy encoding and xterm's modifyOtherKeys. Neither reports a
-- release, a repeat apart from a press, super, or the locks; alt is an ESC
-- prefix (mode 1036) or modifyOtherKeys' parameter.
legacyExpect :: G.KeyOptions -> Stroke -> Expect
legacyExpect o s
  | s.action == G.KeyRelease = Decodes []
  | m.super = Lost "super outside the kitty protocol"
  | m.alt && not o.altEscPrefix && not mok = Lost "alt without mode 1036 or modifyOtherKeys"
  | otherwise = case s.key of
      Named gk k
        | gk `elem` map fst (reportAllKeys <> mediaKeys)
            || gk `elem` [G.KeyScrollLock, G.KeyPrintScreen, G.KeyPause] ->
            Decodes []
        | gk == G.KeyContextMenu -> Lost "the menu key is xterm's F16"
        | gk == G.KeyNumpadEnter && m /= K.noMods -> Lost "modifiers on the keypad"
        | gk == G.KeyBackspace && o.backarrowKeyMode -> Lost "DECBKM's Backspace is 0x08, which is C-h"
        | gk == G.KeyBackspace && m.ctrl -> Lost "Ctrl+Backspace is 0x08, which is C-h"
        | gk == G.KeyBackspace && m.shift && not mok -> Lost "Shift+Backspace is DEL"
        | otherwise -> pressOf (K.chord m k)
      PadKey _ c
        | m /= K.noMods -> Lost "modifiers on the keypad"
        | otherwise -> pressOf (K.key (K.Char c))
      TextKey _ u sh
        | s.mods.capsLock && isAlpha u && (m.ctrl || m.alt || m.shift) ->
            Lost "Caps Lock with a modifier outside the kitty protocol"
        | m.ctrl && not mok && not (isAlpha c || u == ' ') ->
            Lost "ctrl on a character with no C0 byte of its own"
        | sh == u -> pressOf (K.chord m {K.shift = False} (K.Char u))
        | otherwise -> pressOf (meant s u sh)
        where
          c = fromMaybe u (typedChar s)
  where
    m = held s.mods
    mok = o.modifyOtherKeys2

-- | The kitty protocol, under any of its flags.
kittyExpect :: G.KeyOptions -> Stroke -> Expect
kittyExpect o s
  | s.action == G.KeyRelease && not f.reportEvents = Decodes []
  | otherwise = case s.key of
      TextKey _ u sh
        | asText u c -> pressOf (K.key (K.Char c))
        | m.shift && not (others m) && not (isAlpha u) && not shifted && not associated && not eventType ->
            Lost "shift alone as CSI u is xterm's, whose code is the character typed"
        | otherwise -> reported chord (Just c)
        where
          c = fromMaybe u (typedChar s)
          -- kitty reports the shifted key where shift changed it
          shifted = f.reportAlternates && m.shift && c /= u
          chord
            | shifted || (isLower u && s.mods.capsLock && not (others m)) = meant s u sh
            | otherwise = K.chord m (K.Char u)
      PadKey _ c
        | asText c c -> pressOf (K.key (K.Char c))
        -- the keypad's shifted key is reported even where shift does not
        -- change it
        | f.reportAlternates && m.shift -> reported (K.chord m {K.shift = False} (K.Char c)) (Just c)
        | otherwise -> reported (K.chord m (K.Char c)) (Just c)
      Named gk k
        | gk `elem` map fst mediaKeys || gk == G.KeyContextMenu -> Decodes []
        | gk `elem` map fst reportAllKeys && not f.reportAll -> Decodes []
        | gk `elem` [G.KeyEnter, G.KeyTab, G.KeyBackspace] && not f.reportAll ->
            -- their legacy bytes, and no release
            if s.action == G.KeyRelease
              then Decodes []
              else
                if m == K.noMods
                  then pressOf (K.key k)
                  else reported (K.chord m k) Nothing
        | otherwise -> reported (K.chord m k) Nothing
  where
    f = o.kittyFlags
    m = held s.mods
    -- A key that types is sent as its text, with no modifier but a shift
    -- that changed it, unless every key is an escape.
    asText u c = not f.reportAll && not (others m) && not (m.shift && c == u) && s.action /= G.KeyRelease
    associated = f.reportAssociated && s.action /= G.KeyRelease && not (others m)
    eventType = f.reportEvents && s.action /= G.KeyPress
    reported chord typed =
      Decodes
        [ I.Key
            (I.keyEvent chord)
              { I.action = if f.reportEvents then actionOf s.action else I.Press,
                I.text = if associated then maybe "" T.singleton typed else "",
                I.capsLock = s.mods.capsLock,
                I.numLock = s.mods.numLock
              }
        ]
    actionOf = \case
      G.KeyPress -> I.Press
      G.KeyRepeat -> I.Repeat
      G.KeyRelease -> I.Release

keysRoundtrip :: Property
keysRoundtrip =
  forAllShow ((,) <$> genKeyOptions <*> genStroke) show $ \(o, s) -> ioProperty $ do
    bytes <- oracle (G.withKeyEncoder o (\enc -> G.encodeKey enc (platform s)))
    let got = decodeAlone bytes
        shown = counterexample ("bytes: " <> show bytes)
    pure . checkCoverage . cover 60 (isDecodes (expected o s)) "carried" $ case expected o s of
      Decodes want -> tabulate "carried" [if null want then "nothing sent" else "an event"] . shown $ got === want
      Lost why ->
        tabulate "lost" [toString why] . shown . counterexample ("decoded: " <> show got) $
          clean got
  where
    isDecodes = \case
      Decodes _ -> True
      Lost _ -> False
    -- At most one key, and nothing unknown.
    clean = \case
      [] -> True
      [I.Key _] -> True
      _ -> False

genKeyOptions :: Gen G.KeyOptions
genKeyOptions = do
  flags <- frequency [(1, pure (KittyFlags False False False False False)), (2, KittyFlags <$> coin <*> coin <*> coin <*> coin <*> coin)]
  G.KeyOptions
    <$> coin
    <*> coin
    <*> coin
    <*> coin
    <*> coin
    <*> pure flags
    <*> pure G.OptionNotAlt
    <*> coin

genStroke :: Gen Stroke
genStroke = do
  k <- frequency [(6, elements textKeys), (2, elements padKeys), (1, elements padNamedKeys), (3, elements namedKeys), (1, elements kittyKeys)]
  let modifier = frequency [(2, pure Nothing), (1, Just <$> elements [G.LeftSide, G.RightSide])]
  sh <- modifier
  ct <- modifier
  al <- modifier
  su <- modifier
  caps <- frequency [(3, pure False), (1, pure True)]
  -- A keypad key types its character with Num Lock on, and is its
  -- logical key with it off.
  num <- case k of
    PadKey {} -> pure True
    Named gk _ | gk `elem` [pk | Named pk _ <- padNamedKeys] -> pure False
    _ -> frequency [(3, pure False), (1, pure True)]
  act <- elements [G.KeyPress, G.KeyRepeat, G.KeyRelease]
  pure Stroke {key = k, mods = G.Mods {shift = sh, ctrl = ct, alt = al, super = su, capsLock = caps, numLock = num}, action = act}

-- * Mice

-- | A mouse event and the modes it is reported under.
data MouseCase = MouseCase
  { options :: G.MouseOptions,
    event :: G.MouseEvent,
    -- | the position, in whole pixels
    pixel :: (Int, Int),
    -- | a cell's width and height in pixels
    cell :: (Int, Int)
  }
  deriving stock (Show)

mouseRoundtrip :: Property
mouseRoundtrip =
  forAllShow genMouseCase show $ \mc -> ioProperty $ do
    bytes <- oracle (G.withMouseEncoder mc.options (\enc -> G.encodeMouse enc mc.event))
    let pixels = mc.options.format == G.SgrPixelsFormat
        got = decodeAloneWith (I.setPixelMouse pixels I.decoder) bytes
    pure . checkCoverage . cover 50 (not (BS.null bytes)) "reported" . counterexample ("bytes: " <> show bytes) $
      if BS.null bytes then got === [] else got === [I.Mouse (expectedMouse mc)]

-- | What a reported mouse event decodes to: its action and button, the
-- modifiers a report carries (none under X10 tracking), and its cell, or
-- its pixel in SGR-pixel mode.
expectedMouse :: MouseCase -> I.MouseEvent
expectedMouse mc =
  I.MouseEvent
    { action = case mc.event.action of
        G.MousePress -> I.MousePress
        G.MouseRelease -> I.MouseRelease
        G.MouseMotion -> I.MouseMotion,
      button = mc.event.button >>= buttonOf,
      mods =
        if mc.options.tracking == G.X10Tracking
          then K.noMods
          else (held mc.event.mods) {K.super = False},
      position =
        if mc.options.format == G.SgrPixelsFormat
          then I.AtPixel x y
          else I.AtCell (x `div` cellWidth) (y `div` cellHeight)
    }
  where
    (x, y) = mc.pixel
    (cellWidth, cellHeight) = mc.cell
    buttonOf = \case
      G.MouseLeft -> Just I.LeftButton
      G.MouseMiddle -> Just I.MiddleButton
      G.MouseRight -> Just I.RightButton
      G.MouseFour -> Just I.WheelUp
      G.MouseFive -> Just I.WheelDown
      G.MouseSix -> Just I.WheelLeft
      G.MouseSeven -> Just I.WheelRight
      G.MouseEight -> Just I.Button8
      G.MouseNine -> Just I.Button9
      G.MouseTen -> Just I.Button10
      G.MouseEleven -> Just I.Button11
      G.MouseUnknownButton -> Nothing

genMouseCase :: Gen MouseCase
genMouseCase = do
  cols <- genExtent 300
  rows <- genExtent 100
  cw <- genExtent 32
  ch <- genExtent 64
  let geometry = G.gridGeometry GridSize {cols, rows} cw ch
      size = fromIntegral . fromExtent
  tracking <- frequency [(1, pure G.NoTracking), (1, pure G.X10Tracking), (2, pure G.NormalTracking), (2, pure G.ButtonTracking), (3, pure G.AnyTracking)]
  format <- elements [G.SgrFormat, G.SgrPixelsFormat]
  pressed <- coin
  act <- elements [minBound .. maxBound]
  -- Buttons 10 and 11, and one the platform cannot name, are not encoded
  -- by libghostty-vt 1.0; the unit tests decode 10 and 11.
  button <- oneof [pure Nothing, Just <$> elements [G.MouseLeft .. G.MouseNine]]
  let modifier = frequency [(2, pure Nothing), (1, pure (Just G.LeftSide))]
  sh <- modifier
  ct <- modifier
  al <- modifier
  su <- modifier
  x <- chooseInt (0, size cols * size cw - 1)
  y <- chooseInt (0, size rows * size ch - 1)
  -- A quarter of a pixel, exact in a float, is in the same pixel and the
  -- same cell: the encoder rounds a pixel and floors a cell.
  fx <- elements [0, 0.25]
  fy <- elements [0, 0.25]
  pure
    MouseCase
      { options = (G.mouseOptions geometry) {G.tracking = tracking, G.format = format, G.anyButtonPressed = pressed},
        event =
          G.MouseEvent
            { action = act,
              button,
              mods = G.noMods {G.shift = sh, G.ctrl = ct, G.alt = al, G.super = su},
              position = G.MousePosition {x = fromIntegral x + fx, y = fromIntegral y + fy}
            },
        pixel = (x, y),
        cell = (size cw, size ch)
      }
  where
    genExtent n = chooseInt (1, n) `suchThatMap` (extent . fromIntegral)

-- * Running

coin :: Gen Bool
coin = elements [False, True]

-- | Decode one event's bytes, then let the escape timeout pass.
decodeAlone :: ByteString -> [I.InputEvent]
decodeAlone = decodeAloneWith I.decoder

decodeAloneWith :: I.Decoder -> ByteString -> [I.InputEvent]
decodeAloneWith d0 b =
  let (evs, d) = I.decode b d0
   in evs <> fst (I.expire d)

oracle :: Eff '[Ghostty, IOE] a -> IO a
oracle act =
  runEff (runGhostty act) >>= \case
    Left e -> fail ("libghostty-vt refused: " <> show e)
    Right a -> pure a
