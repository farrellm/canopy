{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

-- | The host's input, decoded (DESIGN.md §5.7): a pure, incremental decoder
-- from the bytes the host terminal sends to events.
--
-- It reads every encoding a terminal may use for a key: the legacy one
-- (bytes, C0 controls, an ESC prefix for alt, xterm's PC-style function
-- keys with their modifier parameter, SS3 and the application keypad),
-- xterm's modifyOtherKeys, fixterms' and the kitty keyboard protocol's
-- @CSI u@ with every enhancement flag (events, alternate keys, all keys as
-- escape codes, associated text). It also reads bracketed paste, SGR and
-- SGR-pixel mouse reports, focus reports, in-band resize reports, and the
-- terminal's replies to the queries @Tendril.Term.Caps@ makes.
--
-- __Keys come out in the keymap's normal form__ (D36, "Tendril.Keymap"): a
-- legacy byte is its named key or its letter with ctrl (0x08 is C-h, never
-- C-Backspace; 0x7F is Backspace, never C-?), an ESC prefix is alt, and a
-- character key is the character the terminal reports it typed, with
-- shift cleared, wherever it reports only (a legacy byte, modifyOtherKeys,
-- kitty's shifted alternate key). Where it reports only the unshifted key,
-- the chord keeps shift (@C-S-/@).
--
-- __The escape timeout__ is the caller's. A lone ESC may be Escape, or the
-- start of a sequence whose rest has not arrived yet; so may @ESC [@ (alt
-- and @[@, or a CSI). While 'waiting' is true, the decoder holds such bytes,
-- and the driver calls 'expire' when nothing more has arrived within its
-- timeout: the held bytes are then read as keys (a lone ESC is Escape,
-- @ESC [@ is @M-[@). Under the kitty keyboard protocol Escape is sent as
-- @CSI 27 u@, so a timeout only ever ends a sequence the terminal cut short.
-- A bracketed paste waits for its end with no timeout.
--
-- __Ambiguities__ that bytes alone cannot settle are settled by what the
-- caller says it expects: @CSI 1 ; 5 R@ is Ctrl+F3 in xterm's encoding and a
-- cursor position report for row 1, column 5, so a report is read as one
-- when 'expectCursorPosition' says one is due, or when its row is not 1.
-- xterm sends Help as F15's @CSI 28 ~@ and the context menu key as F16's
-- @CSI 29 ~@; they are read as F15 and F16.
module Tendril.Term.Input
  ( -- * Decoding
    Decoder,
    decoder,
    decode,
    waiting,
    expire,
    setPixelMouse,
    expectCursorPosition,

    -- * Events
    InputEvent (..),
    KeyEvent (..),
    KeyAction (..),
    keyEvent,
    keyText,
    MouseEvent (..),
    MouseAction (..),
    MouseButton (..),
    MousePosition (..),
    Size (..),

    -- * Replies
    TerminalReply (..),
    ModeFamily (..),
    ModeSetting (..),
    ColorSlot (..),
    Rgb (..),
    ColorScheme (..),
  )
where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.Char (isLower, toLower, toUpper)
import Data.List (lookup)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Tendril.Keymap
  ( Key (..),
    KeyChord,
    Mods (..),
    noMods,
    typed,
    withAlt,
    withShift,
  )
import Tendril.Keymap qualified as Keymap

-- * Events

-- | One thing the host said.
data InputEvent
  = -- | A key
    Key KeyEvent
  | -- | A bracketed paste, without its brackets. Its bytes are read as
    -- UTF-8, an invalid byte as U+FFFD, and nothing else is changed: a line
    -- break is whatever the terminal sent (usually CR).
    Paste Text
  | -- | A mouse report
    Mouse MouseEvent
  | -- | Focus came (@True@) or went (@False@)
    Focus Bool
  | -- | The terminal's size changed (an in-band resize report, mode 2048)
    Resize Size
  | -- | A reply to a query
    Reply TerminalReply
  | -- | A sequence that is complete but means nothing the decoder knows,
    -- or bytes that are not UTF-8: these bytes, so that nothing is lost
    -- without a trace. The editor ignores it.
    Unknown ByteString
  deriving stock (Eq, Show)

-- | A key event.
data KeyEvent = KeyEvent
  { -- | the key and modifiers, in the keymap's normal form
    chord :: KeyChord,
    -- | pressed, repeating or released; only the kitty protocol reports
    -- anything but a press
    action :: KeyAction,
    -- | the text the key typed, where the kitty protocol reports it (its
    -- associated-text flag); empty otherwise
    text :: Text,
    -- | the key in the PC-101 US layout, where the kitty protocol reports
    -- it and it differs from the key (its alternate-keys flag): @\'q\'@ for
    -- a Cyrillic @й@
    base :: Maybe Char,
    -- | Hyper was held, which only the kitty protocol reports
    hyper :: Bool,
    -- | Meta, apart from Alt, was held, which only the kitty protocol
    -- reports
    meta :: Bool,
    -- | Caps Lock was on, which only the kitty protocol reports
    capsLock :: Bool,
    -- | Num Lock was on, which only the kitty protocol reports
    numLock :: Bool
  }
  deriving stock (Eq, Show)

-- | Whether a key went down, repeats or came up.
data KeyAction
  = -- | pressed
    Press
  | -- | held down, repeating
    Repeat
  | -- | released
    Release
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A press of a chord with nothing else reported: what every legacy
-- encoding sends.
--
-- >>> keyText (keyEvent (Keymap.chord noMods (Char 'x')))
-- Just "x"
keyEvent :: KeyChord -> KeyEvent
keyEvent c =
  KeyEvent
    { chord = c,
      action = Press,
      text = "",
      base = Nothing,
      hyper = False,
      meta = False,
      capsLock = False,
      numLock = False
    }

-- | The text a key event types when no keymap binds it and no prefix is
-- pending: nothing for a release, or with ctrl, alt, super, hyper or meta
-- held; otherwise the text the terminal reported with it, or else what its
-- chord types ('typed').
keyText :: KeyEvent -> Maybe Text
keyText e
  | e.action == Release || e.hyper || e.meta = Nothing
  | m.ctrl || m.alt || m.super = Nothing
  | not (T.null e.text) = Just e.text
  | otherwise = typed e.chord
  where
    m = e.chord.mods

-- | A mouse report.
data MouseEvent = MouseEvent
  { -- | pressed, released or moved
    action :: MouseAction,
    -- | the button; 'Nothing' for a motion with none held
    button :: Maybe MouseButton,
    -- | shift, alt and ctrl, which are all a report carries
    mods :: Mods,
    -- | where
    position :: MousePosition
  }
  deriving stock (Eq, Show)

-- | What the mouse did.
data MouseAction
  = -- | a button was pressed, or the wheel turned
    MousePress
  | -- | a button was released
    MouseRelease
  | -- | the mouse moved
    MouseMotion
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A mouse button, or a turn of the wheel.
data MouseButton
  = -- | the left button
    LeftButton
  | -- | the middle button
    MiddleButton
  | -- | the right button
    RightButton
  | -- | button 4: the wheel turned up
    WheelUp
  | -- | button 5: the wheel turned down
    WheelDown
  | -- | button 6: the wheel turned left
    WheelLeft
  | -- | button 7: the wheel turned right
    WheelRight
  | -- | button 8, often back
    Button8
  | -- | button 9, often forward
    Button9
  | -- | button 10
    Button10
  | -- | button 11
    Button11
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Where a mouse report says the mouse is, counted from 0 at the top left.
data MousePosition
  = -- | a column and a row, in an SGR report
    AtCell Int Int
  | -- | x and y in pixels, in an SGR-pixel report ('setPixelMouse')
    AtPixel Int Int
  deriving stock (Eq, Show)

-- | The terminal's size.
data Size = Size
  { -- | columns
    columns :: Int,
    -- | rows
    rows :: Int,
    -- | the text area's width in pixels, 0 if unknown
    width :: Int,
    -- | the text area's height in pixels, 0 if unknown
    height :: Int
  }
  deriving stock (Eq, Show)

-- * Replies

-- | A terminal's reply to a query.
data TerminalReply
  = -- | DA1 (@CSI ? … c@): the terminal's class and features
    PrimaryAttributes [Int]
  | -- | DA2 (@CSI > … c@): its type, version and more
    SecondaryAttributes [Int]
  | -- | DECRPM (@CSI ? mode ; setting $ y@, or without @?@ for an ANSI
    -- mode): a mode's setting
    ModeReport ModeFamily Int ModeSetting
  | -- | the kitty keyboard protocol's flags (@CSI ? flags u@)
    KittyKeyboardFlags Int
  | -- | a cursor position report (@CSI row ; column R@), from 0: the row,
    -- then the column
    CursorPosition Int Int
  | -- | a colour (OSC 4, 10, 11 or 12)
    ColorReport ColorSlot Rgb
  | -- | the clipboard (OSC 52): the selection it names (@c@ for the
    -- clipboard), and its contents, decoded from base64
    Clipboard Text ByteString
  | -- | XTVERSION (@DCS > | text ST@): the terminal's name and version
    TerminalVersion Text
  | -- | the colour scheme (@CSI ? 997 ; 1 n@ or @2 n@)
    ColorSchemeReport ColorScheme
  | -- | the text area's size in pixels (@CSI 4 ; height ; width t@): its
    -- width, then its height
    TextAreaPixels Int Int
  | -- | the text area's size in cells (@CSI 8 ; rows ; columns t@): its
    -- columns, then its rows
    TextAreaCells Int Int
  | -- | a cell's size in pixels (@CSI 6 ; height ; width t@): its width,
    -- then its height
    CellPixels Int Int
  deriving stock (Eq, Show)

-- | Which set of modes a mode number is in.
data ModeFamily
  = -- | DEC private modes (@?@)
    DecMode
  | -- | ANSI modes
    AnsiMode
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A mode's setting, as DECRPM reports it.
data ModeSetting
  = -- | the terminal does not know the mode (0)
    NotRecognized
  | -- | set (1)
    Set
  | -- | reset (2)
    Reset
  | -- | set, and cannot be changed (3)
    PermanentlySet
  | -- | reset, and cannot be changed (4)
    PermanentlyReset
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Which colour a colour report is of.
data ColorSlot
  = -- | the default foreground (OSC 10)
    ForegroundColor
  | -- | the default background (OSC 11)
    BackgroundColor
  | -- | the cursor's (OSC 12)
    CursorColor
  | -- | a palette entry (OSC 4)
    PaletteColor Int
  deriving stock (Eq, Ord, Show)

-- | A colour, 8 bits a channel; a report's wider channels are scaled.
data Rgb = Rgb
  { -- | red
    red :: Word8,
    -- | green
    green :: Word8,
    -- | blue
    blue :: Word8
  }
  deriving stock (Eq, Ord, Show)

-- | A colour scheme.
data ColorScheme
  = -- | dark
    Dark
  | -- | light
    Light
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- * Decoding

-- | The decoder's state: the bytes held while they wait for more, a paste
-- or string sequence in progress, and what the caller has said to expect.
data Decoder = Decoder
  { held :: ByteString,
    pasting :: Maybe Pasting,
    stringing :: Maybe Stringing,
    pixelMouse :: Bool,
    cursorReports :: Int
  }

-- | A paste so far: its bytes in reverse order, and the last few, which may
-- be the start of its end.
data Pasting = Pasting [ByteString] ByteString

-- | A string sequence held while it waits for its end: its kind (the byte
-- after ESC), its bytes so far in reverse order, how many there are, and
-- whether the last is an ESC that may start ST. It is kept in pieces and
-- scanned once, so a large reply is not copied and rescanned on each read.
data Stringing = Stringing Word8 [ByteString] Int Bool

-- | A decoder with nothing held, cell mouse reports and no reply expected.
decoder :: Decoder
decoder = Decoder {held = "", pasting = Nothing, stringing = Nothing, pixelMouse = False, cursorReports = 0}

-- | Whether mouse reports are in pixels (SGR-pixels, mode 1016) rather than
-- cells: the bytes are the same, so the caller says which mode it set.
setPixelMouse :: Bool -> Decoder -> Decoder
setPixelMouse p d = d {pixelMouse = p}

-- | Say that a cursor position report is due (the caller sent DSR 6), so
-- that the next @CSI row ; column R@ is read as one even where it could be
-- F3 with modifiers. A DA1 reply ends every report due: a terminal answers
-- in order, so the caller sends DA1 after its queries, and a DSR 6 not
-- answered before DA1's reply never will be.
expectCursorPosition :: Decoder -> Decoder
expectCursorPosition d = d {cursorReports = d.cursorReports + 1}

-- | Whether the decoder holds bytes that 'expire' would decode: the caller
-- should call it if nothing arrives within its escape timeout. False inside
-- a paste, which has no timeout.
waiting :: Decoder -> Bool
waiting d = isNothing d.pasting && (not (BS.null d.held) || isJust d.stringing)

-- | Decode the next bytes from the host: the events they complete, in
-- order. Bytes that may start a longer sequence are held for the next
-- call, or for 'expire'. Decoding bytes in pieces gives the events
-- decoding them at once does.
--
-- >>> [e.chord | Key e <- fst (decode "a\ESC[A" decoder)] == map (Keymap.chord noMods) [Char 'a', ArrowUp]
-- True
decode :: ByteString -> Decoder -> ([InputEvent], Decoder)
decode bytes d = case (d.pasting, d.stringing) of
  (Just p, _) -> pasteMore bytes p d
  (_, Just s) -> stringMore bytes s d
  _ -> run False (d.held <> bytes) d {held = ""}

-- | The escape timeout passed with bytes held: decode them as they are. A
-- lone ESC is Escape, and an ESC before an unfinished sequence is alt with
-- the sequence's first character (@ESC [@ is @M-[@), the rest decoded after
-- it.
--
-- >>> fst (expire (snd (decode "\ESC" decoder))) == [Key (keyEvent (Keymap.chord noMods Escape))]
-- True
expire :: Decoder -> ([InputEvent], Decoder)
expire d
  | isJust d.pasting = ([], d)
  | otherwise = run True (d.held <> maybe "" stringBytes d.stringing) d {held = "", stringing = Nothing}

-- | What one step of decoding found.
data Step
  = -- | the bytes may be the start of something longer
    Need
  | -- | these events, from this many bytes, and the decoder after them
    Took [InputEvent] Int Decoder

-- | Decode bytes until they end or wait for more; with @final@, nothing
-- waits.
run :: Bool -> ByteString -> Decoder -> ([InputEvent], Decoder)
run final = go []
  where
    go acc bs d
      | BS.null bs = (reverse acc, d)
      | otherwise = case step final d bs of
          Need -> (reverse acc, holding bs d)
          Took evs n d' ->
            let rest = BS.drop n bs
             in case d'.pasting of
                  Just p -> first ((reverse acc <> evs) <>) (pasteMore rest p d')
                  Nothing -> go (reverse evs <> acc) rest d'

-- | Hold bytes that wait for more: an unfinished string sequence in pieces
-- ('Stringing'), anything else as they are.
holding :: ByteString -> Decoder -> Decoder
holding bs d = case BS.unpack (BS.take 2 bs) of
  [0x1B, k]
    | k `elem` stringKinds,
      StringMore n esc <- scanString k 2 False (BS.drop 2 bs) ->
        d {stringing = Just (Stringing k [bs] n esc)}
  _ -> d {held = bs}

-- | A held string sequence's bytes.
stringBytes :: Stringing -> ByteString
stringBytes (Stringing _ chunks _ _) = BS.concat (reverse chunks)

-- | Bytes after an unfinished string sequence: scan only them while the
-- sequence goes on, and once it ends (or is not one) decode it whole.
stringMore :: ByteString -> Stringing -> Decoder -> ([InputEvent], Decoder)
stringMore bytes s@(Stringing k chunks n esc) d
  | BS.null bytes = ([], d)
  | otherwise = case scanString k n esc bytes of
      StringMore n' esc' -> ([], d {stringing = Just (Stringing k (bytes : chunks) n' esc')})
      _ -> run False (stringBytes s <> bytes) d {stringing = Nothing}

-- | The end of a bracketed paste.
pasteEnd :: ByteString
pasteEnd = "\ESC[201~"

-- | Bytes inside a paste: find its end, which may straddle two calls.
pasteMore :: ByteString -> Pasting -> Decoder -> ([InputEvent], Decoder)
pasteMore bytes (Pasting chunks carry) d = case BS.breakSubstring pasteEnd s of
  (before, after)
    | not (BS.null after) ->
        let pasted = decodeUtf8Lenient (BS.concat (reverse (before : chunks)))
         in first (Paste pasted :) (run False (BS.drop (BS.length pasteEnd) after) d {pasting = Nothing})
  _ ->
    let (keep, carry') = BS.splitAt (BS.length s - (BS.length pasteEnd - 1)) s
     in ([], d {pasting = Just (Pasting (if BS.null keep then chunks else keep : chunks) carry')})
  where
    s = carry <> bytes

-- | Decode one thing at the start of non-empty bytes.
step :: Bool -> Decoder -> ByteString -> Step
step final d bs = case BS.uncons bs of
  Nothing -> Took [] 0 d
  Just (0x1B, _) -> escape final d bs
  Just _ -> case utf8 bs of
    Decoded c n -> Took [press (Keymap.chord noMods (Char c))] n d
    Short | final -> Took [Unknown bs] (BS.length bs) d
    Short -> Need
    Invalid n -> Took [Unknown (BS.take n bs)] n d

-- | A press of a chord.
press :: KeyChord -> InputEvent
press = Key . keyEvent

-- | Bytes that start with ESC.
escape :: Bool -> Decoder -> ByteString -> Step
escape final d bs = case BS.indexMaybe bs 1 of
  Nothing
    | final -> Took [press (Keymap.chord noMods Escape)] 1 d
    | otherwise -> Need
  Just 0x1B -> escEsc
  Just b
    | b == 0x5B -> sequenceOr (csi d bs)
    | b == 0x4F -> sequenceOr (ss3 d bs)
    | b `elem` stringKinds -> sequenceOr (stringSequence d b bs)
    | otherwise -> altChar
  where
    -- A sequence, or else ESC is alt on the character after it.
    sequenceOr = \case
      Parsed evs n d' -> Took evs n d'
      Partial | not final -> Need
      _ -> altChar
    altChar = case utf8 (BS.drop 1 bs) of
      Decoded c n -> Took [press (withAlt (Keymap.chord noMods (Char c)))] (n + 1) d
      Short | not final -> Need
      _ -> Took [press (Keymap.chord noMods Escape)] 1 d
    -- ESC ESC: alt on a key sequence after it, Escape before any other
    -- sequence (a reply that arrived just after Escape), or else
    -- Alt+Escape.
    escEsc = case BS.indexMaybe bs 2 of
      Nothing
        | final -> Took [press altEscape] 2 d
        | otherwise -> Need
      Just b
        | Just parse <- innerSequence b ->
            case parse (BS.drop 1 bs) of
              Parsed [Key e] n d' -> Took [Key e {chord = withAlt e.chord}] (n + 1) d'
              Parsed {} -> Took [press (Keymap.chord noMods Escape)] 1 d
              Partial | not final -> Need
              _ -> Took [press altEscape] 2 d
      Just _ -> Took [press altEscape] 2 d
    altEscape = withAlt (Keymap.chord noMods Escape)
    innerSequence b
      | b == 0x5B = Just (csi d)
      | b == 0x4F = Just (ss3 d)
      | b `elem` stringKinds = Just (stringSequence d b)
      | otherwise = Nothing

-- | What parsing a sequence found.
data Parse
  = -- | events, from this many bytes, and the decoder after them
    Parsed [InputEvent] Int Decoder
  | -- | the bytes end before the sequence does
    Partial
  | -- | the bytes are not this kind of sequence
    Malformed

-- | The longest sequence the decoder holds while it waits for its end:
-- longer, and it is not one.
maxSequence :: Int
maxSequence = 4096

-- | The longest string sequence (OSC, DCS): an OSC 52 reply carries the
-- clipboard.
maxStringSequence :: Int
maxStringSequence = 16 * 1024 * 1024

-- ** CSI

-- | A control sequence: its private marker, its parameters with their
-- sub-parameters (empty ones 'Nothing'), its intermediate bytes and its
-- final byte.
data Csi = Csi
  { marker :: Maybe Word8,
    params :: [[Maybe Int]],
    intermediates :: ByteString,
    final :: Word8
  }

-- | A sequence that starts with @ESC [@.
csi :: Decoder -> ByteString -> Parse
csi d bs = case scan 2 of
  Nothing -> Partial
  Just (Left ()) -> Malformed
  Just (Right n) ->
    let body = BS.take (n - 2) (BS.drop 2 bs)
        (paramBytes, rest) = BS.span (\b -> b >= 0x30 && b <= 0x3F) body
        raw = BS.take n bs
     in case (BS.unsnoc rest, parseParams paramBytes) of
          (Just (inter, fin), Just (mk, ps)) -> interpretCsi d raw n Csi {marker = mk, params = ps, intermediates = inter, final = fin}
          _ -> Parsed [Unknown raw] n d
  where
    -- The length of the sequence, or Left at a byte that cannot be in one.
    -- A sequence longer than 'maxSequence' is not one, complete or not, so
    -- that bytes decoded in pieces and whole agree.
    scan i = go i False
      where
        go j inInter = case BS.indexMaybe bs j of
          _ | j >= maxSequence -> Just (Left ())
          Nothing -> Nothing
          Just b
            | b >= 0x30 && b <= 0x3F && not inInter -> go (j + 1) False
            | b >= 0x20 && b <= 0x2F -> go (j + 1) True
            | b >= 0x40 && b <= 0x7E -> Just (Right (j + 1))
            | otherwise -> Just (Left ())

-- | Parameter bytes: a private marker, then numbers separated by @;@, each
-- with sub-parameters separated by @:@. 'Nothing' for anything else, or a
-- number too long to be one.
parseParams :: ByteString -> Maybe (Maybe Word8, [[Maybe Int]])
parseParams bytes = do
  let (mk, rest) = case BS.uncons bytes of
        Just (b, r) | b >= 0x3C && b <= 0x3F -> (Just b, r)
        _ -> (Nothing, bytes)
  ps <-
    if BS.null rest
      then Just []
      else traverse (traverse decimal . BS.split 0x3A) (BS.split 0x3B rest)
  pure (mk, ps)

-- | A parameter's digits: 'Nothing' inside for none, and 'Nothing' for
-- anything but digits, or a number too long to be one.
decimal :: ByteString -> Maybe (Maybe Int)
decimal b
  | BS.null b = Just Nothing
  | BS.length b <= 9 && BS.all (\x -> x >= 0x30 && x <= 0x39) b =
      Just (Just (BS.foldl' (\a x -> a * 10 + fromIntegral (x - 0x30)) 0 b))
  | otherwise = Nothing

-- | A parameter's sub-parameter, by index.
sub :: Int -> Int -> Csi -> Maybe Int
sub i j c = join ((c.params !!? i) >>= (!!? j))

-- | A parameter's first sub-parameter.
param :: Int -> Csi -> Maybe Int
param i = sub i 0

-- | The parameters, if each is one number.
plain :: Csi -> Maybe [Int]
plain c = traverse number1 c.params
  where
    number1 [Just n] = Just n
    number1 _ = Nothing

-- | What a complete control sequence means.
interpretCsi :: Decoder -> ByteString -> Int -> Csi -> Parse
interpretCsi d raw n c = case (c.marker, c.intermediates, c.final) of
  (Nothing, "", 0x7E) -> tilde
  (Nothing, "", 0x75) -> only (kittyKey c)
  (Nothing, "", 0x52) -> cursorOrF3
  (Nothing, "", f)
    | Just k <- lookup f letterKeys -> only (legacyKey k c)
    | f == 0x5A -> only (legacyKey Tab c >>= addShift)
    | f == 0x49, null c.params -> done [Focus True]
    | f == 0x4F, null c.params -> done [Focus False]
    | f == 0x74 -> only (windowReport c)
  (Nothing, "$", 0x79) -> only (modeReport AnsiMode c)
  (Just 0x3F, "", 0x63) -> case plain c of
    -- DA1 is the sentinel after the caller's queries: no report is due
    -- after it.
    Just ps -> Parsed [Reply (PrimaryAttributes ps)] n d {cursorReports = 0}
    Nothing -> unknown
  (Just 0x3F, "", 0x75) -> only (Reply . KittyKeyboardFlags <$> (plain c >>= single))
  (Just 0x3F, "$", 0x79) -> only (modeReport DecMode c)
  (Just 0x3F, "", 0x6E) -> only (colorScheme c)
  (Just 0x3E, "", 0x63) -> only (Reply . SecondaryAttributes <$> plain c)
  (Just 0x3C, "", f) | f == 0x4D || f == 0x6D -> only (sgrMouse d.pixelMouse c)
  _ -> unknown
  where
    done evs = Parsed evs n d
    unknown = done [Unknown raw]
    only = maybe unknown (done . pure)
    single [x] = Just x
    single _ = Nothing
    addShift = \case
      Key e -> Just (Key e {chord = withShift e.chord})
      _ -> Nothing
    tilde = case param 0 c of
      Just 200 | length c.params == 1 -> Parsed [] n d {pasting = Just (Pasting [] "")}
      Just 27 -> only (modifyOtherKeys c)
      Just k | Just key' <- lookup k tildeKeys -> only (legacyKey key' c)
      _ -> unknown
    cursorOrF3 = case plain c of
      Just [row, col]
        | d.cursorReports > 0 || row /= 1 ->
            Parsed [Reply (CursorPosition (row - 1) (col - 1))] n d {cursorReports = max 0 (d.cursorReports - 1)}
      _ -> only (legacyKey (F 3) c)

-- | Final bytes of xterm's PC-style keys: @CSI A@, or @CSI 1 ; mods A@.
letterKeys :: [(Word8, Key)]
letterKeys =
  [ (0x41, ArrowUp),
    (0x42, ArrowDown),
    (0x43, ArrowRight),
    (0x44, ArrowLeft),
    (0x45, Begin),
    (0x46, End),
    (0x48, Home),
    (0x50, F 1),
    (0x51, F 2),
    (0x53, F 4)
  ]

-- | The numbers of xterm's @CSI n ~@ keys.
tildeKeys :: [(Int, Key)]
tildeKeys =
  [ (1, Home),
    (2, InsertKey),
    (3, DeleteKey),
    (4, End),
    (5, PageUp),
    (6, PageDown),
    (7, Home),
    (8, End),
    (57427, Begin)
  ]
    <> zip [11 .. 15] (map F [1 .. 5])
    <> zip [17 .. 21] (map F [6 .. 10])
    <> zip [23 .. 26] (map F [11 .. 14])
    <> zip [28, 29] (map F [15, 16])
    <> zip [31 .. 34] (map F [17 .. 20])
    <> zip [42 .. 46] (map F [21 .. 25])

-- | The modifiers and lock states a @mods@ parameter carries: 1 plus a bit
-- each for shift, alt, ctrl, super, hyper, meta, Caps Lock and Num Lock.
data Modifiers = Modifiers
  { mods :: Mods,
    hyperHeld :: Bool,
    metaHeld :: Bool,
    capsOn :: Bool,
    numOn :: Bool
  }

-- | A @mods@ parameter.
modifiers :: Int -> Modifiers
modifiers p =
  Modifiers
    { mods = Mods {shift = bit 0, alt = bit 1, ctrl = bit 2, super = bit 3},
      hyperHeld = bit 4,
      metaHeld = bit 5,
      capsOn = bit 6,
      numOn = bit 7
    }
  where
    b = max 0 (p - 1)
    bit = testBit b

-- | A key event from its key, modifiers and action.
eventOf :: KeyAction -> Modifiers -> KeyChord -> KeyEvent
eventOf act m c =
  (keyEvent c)
    { action = act,
      hyper = m.hyperHeld,
      meta = m.metaHeld,
      capsLock = m.capsOn,
      numLock = m.numOn
    }

-- | The kitty protocol's event type: 1 press, 2 repeat, 3 release.
actionOf :: Maybe Int -> Maybe KeyAction
actionOf = \case
  Nothing -> Just Press
  Just 1 -> Just Press
  Just 2 -> Just Repeat
  Just 3 -> Just Release
  Just _ -> Nothing

-- | A key whose sequence carries only its modifiers and action, as the
-- second parameter (@CSI 1 ; mods:event A@, @CSI 3 ; mods ~@).
legacyKey :: Key -> Csi -> Maybe InputEvent
legacyKey k c
  | length c.params > 2 || any ((> 2) . length) c.params = Nothing
  | otherwise = do
      act <- actionOf (sub 1 1 c)
      let m = modifiers (fromMaybe 1 (param 1 c))
      pure (Key (eventOf act m (Keymap.chord m.mods k)))

-- | xterm's modifyOtherKeys: @CSI 27 ; mods ; code ~@, where the code is
-- the character the key typed, so shift has been applied to it.
modifyOtherKeys :: Csi -> Maybe InputEvent
modifyOtherKeys c = do
  [27, p, code] <- plain c
  k <- codeKey code
  let m = modifiers p
      ms = case k of
        Char _ -> m.mods {shift = False}
        _ -> m.mods
  pure (Key (eventOf Press m (Keymap.chord ms (applied m k))))
  where
    -- A letter the terminal sent small with shift is its capital.
    applied m = \case
      Char ch | m.mods.shift -> Char (toUpper ch)
      k -> k

-- | The kitty protocol's (and fixterms') @CSI code:shifted:base ; mods:event
-- ; text u@.
kittyKey :: Csi -> Maybe InputEvent
kittyKey c = do
  guard (length c.params <= 3)
  code <- param 0 c
  act <- actionOf (sub 1 1 c)
  k <- codeKey code
  let m = modifiers (fromMaybe 1 (param 1 c))
      shifted = sub 0 1 c >>= scalar
      -- The base layout key, unless it is the key: kitty reports the
      -- keypad's 5 as its own code with base key 5, which is 'Char' '5'
      -- here.
      baseKey = mfilter ((/= k) . Char) (sub 0 2 c >>= scalar)
      txt = T.pack (mapMaybe (>>= scalar) (fromMaybe [] (c.params !!? 2)))
      others = m.mods.ctrl || m.mods.alt || m.mods.super || m.hyperHeld || m.metaHeld
      -- xterm's CSI u is a character's own code and no event type: an event
      -- type, or one of kitty's functional codes (the keypad's), is kitty's,
      -- whose code is the unshifted key.
      xtermForm = code < 57344 && isNothing (sub 1 1 c)
      cased x = toUpper x /= x || toLower x /= x
      ch = case (k, shifted) of
        (Char x, _)
          | m.capsOn && not others && cased x ->
              -- Caps Lock with no other modifier: the key types the other
              -- case, as shift would, and shift with it the case it
              -- would have without both (the shifted key is the capital
              -- whatever the lock).
              let up = isLower x /= m.mods.shift
               in Keymap.chord m.mods {shift = False} (Char (if up then toUpper x else toLower x))
        (Char _, Just s) | m.mods.shift -> Keymap.chord m.mods {shift = False} (Char s)
        (Char x, Nothing)
          | m.mods.shift && not others && not (cased x) && T.null txt && xtermForm ->
              -- Shift alone, with neither the shifted key nor text: xterm's
              -- CSI u (formatOtherKeys), whose code is the character shift
              -- typed, as modifyOtherKeys' is. Kitty sends this only with
              -- every key as an escape and neither alternate keys nor
              -- text, which the decoder's caller does not ask for.
              Keymap.chord m.mods {shift = False} k
        _ -> Keymap.chord m.mods k
  pure (Key (eventOf act m ch) {text = txt, base = baseKey})

-- | A code point, if it is a Unicode scalar value.
scalar :: Int -> Maybe Char
scalar n
  | n < 0 || n > 0x10FFFF || (n >= 0xD800 && n <= 0xDFFF) = Nothing
  | otherwise = Just (chr n)

-- | The key for a code in a @CSI u@ or modifyOtherKeys sequence: a
-- character, or one of the kitty protocol's functional keys, which are in
-- the Private Use Area from 57344.
codeKey :: Int -> Maybe Key
codeKey n = case n of
  9 -> Just Tab
  13 -> Just Enter
  27 -> Just Escape
  127 -> Just Backspace
  _
    | n >= 57344 && n <= 63743 -> functionalKey n
    | otherwise -> Char <$> scalar n

-- | The kitty protocol's functional keys.
functionalKey :: Int -> Maybe Key
functionalKey n
  | n >= 57376 && n <= 57398 = Just (F (fromIntegral (n - 57376 + 13)))
  | n >= 57399 && n <= 57408 = Just (Char (chr (fromEnum '0' + n - 57399)))
  | n >= 57428 && n <= 57440 = Media <$> safeToEnum (n - 57428)
  | n >= 57441 && n <= 57454 = Modifier <$> safeToEnum (n - 57441)
  | otherwise = lookup n table
  where
    table =
      [ (57358, CapsLock),
        (57359, ScrollLock),
        (57360, NumLock),
        (57361, PrintScreen),
        (57362, Pause),
        (57363, ContextMenu),
        (57409, Char '.'),
        (57410, Char '/'),
        (57411, Char '*'),
        (57412, Char '-'),
        (57413, Char '+'),
        (57414, Enter),
        (57415, Char '='),
        (57416, Char ','),
        (57417, ArrowLeft),
        (57418, ArrowRight),
        (57419, ArrowUp),
        (57420, ArrowDown),
        (57421, PageUp),
        (57422, PageDown),
        (57423, Home),
        (57424, End),
        (57425, InsertKey),
        (57426, DeleteKey),
        (57427, Begin)
      ]

-- | The value of a bounded enumeration at an index, if there is one.
safeToEnum :: (Enum a, Bounded a) => Int -> Maybe a
safeToEnum i = find ((== i) . fromEnum) [minBound .. maxBound]

-- | An SGR mouse report: @CSI < button ; x ; y M@, or @m@ for a release.
sgrMouse :: Bool -> Csi -> Maybe InputEvent
sgrMouse pixels c = do
  [b, x, y] <- plain c
  let low = b .&. 3
      number = low .|. (if testBit b 6 then 4 else 0) .|. (if testBit b 7 then 8 else 0)
      button
        | low == 3 && not (testBit b 6) && not (testBit b 7) = Nothing
        | otherwise = toEnumMaybe number
      act
        | c.final == 0x6D = MouseRelease
        | testBit b 5 = MouseMotion
        | otherwise = MousePress
      position = if pixels then AtPixel x y else AtCell (x - 1) (y - 1)
      mods = noMods {shift = testBit b 2, alt = testBit b 3, ctrl = testBit b 4}
  -- Bits above 7 mean nothing; a button number past 11 is not one, and
  -- only a plain 3 is no button.
  guard (b < 256 && (isJust button || number == 3))
  pure (Mouse MouseEvent {action = act, button, mods, position})
  where
    toEnumMaybe :: Int -> Maybe MouseButton
    toEnumMaybe i = find ((== i) . buttonNumber) [minBound .. maxBound]
    buttonNumber = \case
      LeftButton -> 0
      MiddleButton -> 1
      RightButton -> 2
      WheelUp -> 4
      WheelDown -> 5
      WheelLeft -> 6
      WheelRight -> 7
      Button8 -> 8
      Button9 -> 9
      Button10 -> 10
      Button11 -> 11

-- | A reply to XTWINOPS, or an in-band resize report (@CSI 48 ; rows ;
-- columns ; height ; width t@).
windowReport :: Csi -> Maybe InputEvent
windowReport c =
  plain c >>= \case
    [48, r, cols, h, w] -> Just (Resize Size {columns = cols, rows = r, width = w, height = h})
    [4, h, w] -> Just (Reply (TextAreaPixels w h))
    [6, h, w] -> Just (Reply (CellPixels w h))
    [8, r, cols] -> Just (Reply (TextAreaCells cols r))
    _ -> Nothing

-- | DECRPM: @CSI ? mode ; setting $ y@.
modeReport :: ModeFamily -> Csi -> Maybe InputEvent
modeReport family c = do
  [mode, s] <- plain c
  setting <- safeToEnum s
  pure (Reply (ModeReport family mode setting))

-- | The colour scheme: @CSI ? 997 ; 1 n@ (dark) or @2 n@ (light).
colorScheme :: Csi -> Maybe InputEvent
colorScheme c =
  plain c >>= \case
    [997, 1] -> Just (Reply (ColorSchemeReport Dark))
    [997, 2] -> Just (Reply (ColorSchemeReport Light))
    _ -> Nothing

-- ** SS3

-- | A sequence that starts with @ESC O@: F1–F4, the cursor keys in
-- application mode, and the application keypad, with an optional
-- modifier parameter.
ss3 :: Decoder -> ByteString -> Parse
ss3 d bs = go 2
  where
    go j = case BS.indexMaybe bs j of
      _ | j >= maxSequence -> Malformed
      Nothing -> Partial
      Just b
        | (b >= 0x30 && b <= 0x39) || b == 0x3B -> go (j + 1)
        | b >= 0x40 && b <= 0x7E ->
            let ps = BS.split 0x3B (BS.take (j - 2) (BS.drop 2 bs))
                mods = case reverse ps of
                  p : _ | Just (Just v) <- decimal p -> v
                  _ -> 1
                m = modifiers mods
             in -- A final byte that names no key is not SS3: alt on O, and
                -- the rest typed after it (@M-O@ then @a@).
                maybe Malformed (\k -> Parsed [Key (eventOf Press m (Keymap.chord m.mods k))] (j + 1) d) (ss3Key b)
        | otherwise -> Malformed

-- | The key an SS3 sequence's final byte names.
ss3Key :: Word8 -> Maybe Key
ss3Key b = case lookup b letterKeys of
  Just k -> Just k
  Nothing
    | b == 0x52 -> Just (F 3)
    | b == 0x4D -> Just Enter
    | b == 0x49 -> Just Tab
    | b == 0x58 -> Just (Char '=')
    | b >= 0x70 && b <= 0x79 -> Just (Char (chr (fromIntegral b - 0x70 + fromEnum '0')))
    | otherwise -> lookup b [(0x6A, Char '*'), (0x6B, Char '+'), (0x6C, Char ','), (0x6D, Char '-'), (0x6E, Char '.'), (0x6F, Char '/')]

-- ** String sequences

-- | A sequence that starts with @ESC ]@ (OSC), @ESC P@ (DCS), @ESC _@
-- (APC), @ESC ^@ (PM) or @ESC X@ (SOS), and ends with ST (@ESC \\@), or
-- for an OSC also BEL. No reply is empty or holds another C0 control, so
-- such bytes are not one: they are alt on the character after ESC, as the
-- user typed them (@M-]@ then @C-g@ is not an empty OSC).
stringSequence :: Decoder -> Word8 -> ByteString -> Parse
stringSequence d kind bs = case scanString kind 2 False (BS.drop 2 bs) of
  StringEnds len n ->
    let body = BS.take len (BS.drop 2 bs)
        ev = case kind of
          0x5D -> osc body
          0x50 -> dcs body
          _ -> Nothing
     in Parsed [fromMaybe (Unknown (BS.take n bs)) ev] n d
  StringBad -> Malformed
  StringMore {} -> Partial

-- | The bytes after ESC that start a string sequence: OSC, DCS, APC, PM and
-- SOS.
stringKinds :: [Word8]
stringKinds = [0x5D, 0x50, 0x5F, 0x5E, 0x58]

-- | Where a scan of a string sequence got to.
data StringScan
  = -- | it ends: its body's length, and the sequence's
    StringEnds Int Int
  | -- | it is not one
    StringBad
  | -- | the bytes end first: how many the sequence has so far, and whether
    -- the last is an ESC that may start ST
    StringMore Int Bool

-- | Scan bytes of a string sequence of a kind, the first of which is its
-- byte at an index, after an ESC there or not. Counted from the sequence's
-- start, so that a scan resumed on the next read agrees with one of the
-- whole.
scanString :: Word8 -> Int -> Bool -> ByteString -> StringScan
scanString kind start = go 0
  where
    go i esc bytes =
      let j = start + i
       in case BS.indexMaybe bytes i of
            _ | j >= maxStringSequence -> StringBad
            Nothing -> StringMore j esc
            Just b
              | esc -> if b == 0x5C then StringEnds (j - 3) (j + 1) else StringBad
              | b == 0x07, kind == 0x5D, j > 2 -> StringEnds (j - 2) (j + 1)
              | b == 0x1B, j > 2 -> go (i + 1) True bytes
              | b < 0x20 -> StringBad
              | otherwise -> go (i + 1) False bytes

-- | An OSC's body: colour and clipboard replies.
osc :: ByteString -> Maybe InputEvent
osc body = case BS.split 0x3B body of
  ["10", spec] -> color ForegroundColor spec
  ["11", spec] -> color BackgroundColor spec
  ["12", spec] -> color CursorColor spec
  ["4", i, spec] -> do
    (mk, ps) <- parseParams i
    guard (isNothing mk)
    [[Just idx]] <- Just ps
    color (PaletteColor idx) spec
  ["52", sel, payload] -> Reply . Clipboard (decodeUtf8Lenient sel) <$> base64 payload
  _ -> Nothing
  where
    color slot spec = Reply . ColorReport slot <$> rgbSpec spec

-- | A DCS's body: XTVERSION's reply.
dcs :: ByteString -> Maybe InputEvent
dcs body = do
  rest <- BS.stripPrefix ">|" body
  pure (Reply (TerminalVersion (decodeUtf8Lenient rest)))

-- | An X colour, @rgb:r/g/b@ with one to four hex digits a channel.
rgbSpec :: ByteString -> Maybe Rgb
rgbSpec spec = do
  rest <- BS.stripPrefix "rgb:" spec
  [r, g, b] <- traverse channel (BS.split 0x2F rest)
  pure Rgb {red = r, green = g, blue = b}
  where
    channel h = do
      guard (BS.length h >= 1 && BS.length h <= 4)
      v <- BS.foldl' (\acc x -> (\a y -> a * 16 + y) <$> acc <*> hexDigit x) (Just 0) h
      let top = (1 `shiftL` (4 * BS.length h)) - 1 :: Int
      pure (fromIntegral ((v * 255 * 2 + top) `div` (2 * top)))
    hexDigit x
      | x >= 0x30 && x <= 0x39 = Just (fromIntegral x - 0x30)
      | x >= 0x61 && x <= 0x66 = Just (fromIntegral x - 0x61 + 10)
      | x >= 0x41 && x <= 0x46 = Just (fromIntegral x - 0x41 + 10)
      | otherwise = Nothing

-- | Standard base64, padded or not.
base64 :: ByteString -> Maybe ByteString
base64 input = do
  let body = BS.dropWhileEnd (== 0x3D) input
  guard (BS.length body `mod` 4 /= 1 && BS.length input - BS.length body <= 2)
  sextets <- traverse value (BS.unpack body)
  pure (BS.pack (bytes sextets))
  where
    value x
      | x >= 0x41 && x <= 0x5A = Just (x - 0x41)
      | x >= 0x61 && x <= 0x7A = Just (x - 0x61 + 26)
      | x >= 0x30 && x <= 0x39 = Just (x - 0x30 + 52)
      | x == 0x2B = Just 62
      | x == 0x2F = Just 63
      | otherwise = Nothing
    bytes :: [Word8] -> [Word8]
    bytes (a : b : c : e : rest) = [a `shiftL` 2 .|. b `shiftR` 4, b `shiftL` 4 .|. c `shiftR` 2, c `shiftL` 6 .|. e] <> bytes rest
    bytes [a, b, c] = [a `shiftL` 2 .|. b `shiftR` 4, b `shiftL` 4 .|. c `shiftR` 2]
    bytes [a, b] = [a `shiftL` 2 .|. b `shiftR` 4]
    bytes _ = []

-- ** UTF-8

-- | What the bytes start with.
data Utf8
  = -- | a character, from this many bytes
    Decoded Char Int
  | -- | the start of a character, cut short
    Short
  | -- | this many bytes that are not UTF-8
    Invalid Int

-- | The character at the start of non-empty bytes.
utf8 :: ByteString -> Utf8
utf8 bs = case BS.uncons bs of
  Nothing -> Short
  Just (b0, _)
    | b0 < 0x80 -> Decoded (chr (fromIntegral b0)) 1
    | b0 >= 0xC2 && b0 <= 0xDF -> multi 2 (fromIntegral b0 .&. 0x1F) 0x80
    | b0 >= 0xE0 && b0 <= 0xEF -> multi 3 (fromIntegral b0 .&. 0x0F) 0x800
    | b0 >= 0xF0 && b0 <= 0xF4 -> multi 4 (fromIntegral b0 .&. 0x07) 0x10000
    | otherwise -> Invalid 1
  where
    multi :: Int -> Int -> Int -> Utf8
    multi len lead least = go 1 lead
      where
        go i acc
          | i == len =
              if acc < least || acc > 0x10FFFF || (acc >= 0xD800 && acc <= 0xDFFF)
                then Invalid len
                else Decoded (chr acc) len
          | otherwise = case BS.indexMaybe bs i of
              Nothing -> Short
              Just b
                | b .&. 0xC0 == 0x80 -> go (i + 1) (acc `shiftL` 6 .|. fromIntegral (b .&. 0x3F))
                | otherwise -> Invalid i
