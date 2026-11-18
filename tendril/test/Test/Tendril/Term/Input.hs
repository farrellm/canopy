-- | The host-input decoder (DESIGN.md §5.7, M2.7): one test per encoding,
-- and a property that decoding bytes in pieces gives the events decoding
-- them at once does.
module Test.Tendril.Term.Input (tests) where

import Data.ByteString qualified as BS
import Tendril.Keymap (Key (..), KeyChord, MediaKey (..), ModifierKey (..), Mods (..), key, noMods, withAlt, withCtrl, withShift, withSuper)
import Tendril.Term.Input
import Test.QuickCheck (Gen, chooseInt, elements, forAll, listOf, vectorOf, (===))
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Tendril.Term.Input"
    [ testGroup "legacy" legacy,
      testGroup "escape prefix and timeout" escapes,
      testGroup "PC-style keys" pcStyle,
      testGroup "modifyOtherKeys" otherKeys,
      testGroup "kitty" kitty,
      testGroup "paste" pastes,
      testGroup "mouse" mice,
      testGroup "focus and resize" focusResize,
      testGroup "replies" replies,
      testGroup "unknown" unknowns,
      testProperty "pieces decode as the whole" $
        forAll genStream $ \parts ->
          forAll (genCuts (BS.length (mconcat parts))) $ \cuts ->
            decodeAll [mconcat parts] === decodeAll (cutAt cuts (mconcat parts))
    ]

-- | Decode chunks in turn, then let the timeout pass.
decodeAll :: [ByteString] -> [InputEvent]
decodeAll chunks =
  let (evs, d) = foldl' (\(acc, dd) c -> first (acc <>) (decode c dd)) ([], decoder) chunks
   in evs <> fst (expire d)

-- | The events bytes decode to, with the timeout after them.
decodes :: ByteString -> [InputEvent] -> Assertion
decodes b expected = decodeAll [b] @?= expected

-- | The chords of key presses bytes decode to.
chords :: ByteString -> [KeyChord] -> Assertion
chords b cs = decodes b (map (Key . keyEvent) cs)

cK, aK, sK :: Char -> KeyChord
cK c = withCtrl (key (Char c))
aK c = withAlt (key (Char c))
sK c = withShift (key (Char c))

legacy :: [TestTree]
legacy =
  [ testCase "printable ASCII and UTF-8" $ chords (encodeUtf8 ("aZ é→😀" :: Text)) (map (key . Char) "aZ é→😀"),
    testCase "C0 bytes are their named keys or ctrl letters" $
      chords "\x01\x1a\x00\x1f\x1c\x0a" [cK 'a', cK 'z', cK ' ', cK '_', cK '\\', cK 'j'],
    testCase "Tab, Enter, Backspace" $ chords "\t\r\DEL" [key Tab, key Enter, key Backspace],
    -- D36: a legacy 0x08 is C-h, never C-Backspace.
    testCase "0x08 is C-h" $ chords "\x08" [cK 'h'],
    testCase "0x7F is Backspace, never C-?" $ chords "\DEL" [key Backspace],
    testCase "keyText types a plain key's character" $
      map keyText [keyEvent (key (Char 'a')), keyEvent (cK 'a'), keyEvent (key Enter)] @?= [Just "a", Nothing, Nothing]
  ]

escapes :: [TestTree]
escapes =
  [ testCase "ESC before a character is alt" $ chords "\ESCa\ESCA\ESC\DEL\ESC\r\ESC\x08" [aK 'a', aK 'A', withAlt (key Backspace), withAlt (key Enter), withAlt (cK 'h')],
    testCase "ESC before a UTF-8 character" $ chords (encodeUtf8 ("\ESCé" :: Text)) [aK 'é'],
    testCase "a lone ESC waits, and is Escape at the timeout" $ do
      let (evs, d) = decode "\ESC" decoder
      evs @?= []
      waiting d @?= True
      fst (expire d) @?= [Key (keyEvent (key Escape))],
    testCase "ESC, then a character in the next read, is alt" $ do
      let (_, d) = decode "\ESC" decoder
      fst (decode "x" d) @?= [Key (keyEvent (aK 'x'))],
    testCase "ESC [ at the timeout is M-[" $ chords "\ESC[" [aK '['],
    testCase "ESC O at the timeout is M-O" $ chords "\ESCO" [aK 'O'],
    testCase "ESC ] at the timeout is M-], and the rest is read as keys" $ chords "\ESC]1" [aK ']', key (Char '1')],
    -- A control byte cannot be in a reply, so M-] then C-g is not an empty OSC.
    testCase "regression 32: ESC ] before a control byte is M-], at once" $
      fst (decode "\ESC]\a\ESCPx\r" decoder) @?= map (Key . keyEvent) [aK ']', cK 'g', aK 'P', key (Char 'x'), key Enter],
    testCase "regression 33: a sequence past the longest is not one, whole or in pieces" $ do
      let long = "\ESC[" <> BS.replicate 5000 0x31 <> "~"
      decodeAll [long] @?= decodeAll [BS.take 4100 long, BS.drop 4100 long]
      take 1 (decodeAll [long]) @?= [Key (keyEvent (aK '['))],
    -- An SS3 final byte that names no key is not SS3, so M-O then a is not lost.
    testCase "regression 36: ESC O before a byte that names no key is M-O" $
      chords "\ESCOa\ESCO5z" [aK 'O', key (Char 'a'), aK 'O', key (Char '5'), key (Char 'z')],
    testCase "ESC ESC is M-ESC" $ chords "\ESC\ESC" [withAlt (key Escape)],
    testCase "ESC before a key sequence is alt on it" $ chords "\ESC\ESC[A\ESC\ESCOP" [withAlt (key ArrowUp), withAlt (key (F 1))],
    testCase "ESC before a non-key sequence is Escape" $ decodes "\ESC\ESC[I" [Key (keyEvent (key Escape)), Focus True],
    testCase "an incomplete UTF-8 character waits" $ do
      let (evs, d) = decode "\xc3" decoder
      evs @?= []
      fst (decode "\xa9" d) @?= [Key (keyEvent (key (Char 'é')))]
  ]

pcStyle :: [TestTree]
pcStyle =
  [ testCase "cursor keys, normal and application" $
      chords "\ESC[A\ESC[B\ESC[C\ESC[D\ESCOA\ESC[H\ESC[F\ESCOH\ESC[E" [key ArrowUp, key ArrowDown, key ArrowRight, key ArrowLeft, key ArrowUp, key Home, key End, key Home, key Begin],
    testCase "cursor keys with modifiers" $
      chords "\ESC[1;5A\ESC[1;2D\ESC[1;3H\ESC[1;9C" [withCtrl (key ArrowUp), withShift (key ArrowLeft), withAlt (key Home), withSuper (key ArrowRight)],
    testCase "tilde keys" $
      chords "\ESC[2~\ESC[3~\ESC[5~\ESC[6~\ESC[1~\ESC[4~\ESC[3;5~" [key InsertKey, key DeleteKey, key PageUp, key PageDown, key Home, key End, withCtrl (key DeleteKey)],
    testCase "function keys" $
      chords
        "\ESCOP\ESCOQ\ESCOR\ESCOS\ESC[15~\ESC[17~\ESC[24~\ESC[25~\ESC[34~\ESC[46~\ESC[1;5P\ESC[13;2~"
        [key (F 1), key (F 2), key (F 3), key (F 4), key (F 5), key (F 6), key (F 12), key (F 13), key (F 20), key (F 25), withCtrl (key (F 1)), withShift (key (F 3))],
    testCase "Ctrl+F3 is CSI 1;5R when no cursor report is due" $ chords "\ESC[1;5R" [withCtrl (key (F 3))],
    testCase "Shift+Tab" $ chords "\ESC[Z" [withShift (key Tab)],
    testCase "application keypad" $
      chords "\ESCOM\ESCOp\ESCOy\ESCOk\ESCOX\ESCOn" [key Enter, key (Char '0'), key (Char '9'), key (Char '+'), key (Char '='), key (Char '.')]
  ]

otherKeys :: [TestTree]
otherKeys =
  [ testCase "Ctrl+Tab, Shift+Enter, Ctrl+Escape" $
      chords "\ESC[27;5;9~\ESC[27;2;13~\ESC[27;5;27~" [withCtrl (key Tab), withShift (key Enter), withCtrl (key Escape)],
    -- The code is the character typed, so shift has been applied to it.
    testCase "a character's code has shift applied" $
      chords "\ESC[27;6;63~\ESC[27;5;97~\ESC[27;6;65~\ESC[27;2;32~" [cK '?', cK 'a', cK 'A', key (Char ' ')]
  ]

-- | A kitty key event, with its fields.
kittyEvent :: KeyChord -> KeyEvent
kittyEvent = keyEvent

kitty :: [TestTree]
kitty =
  [ testCase "ctrl, alt and super letters" $ chords "\ESC[97;5u\ESC[97;3u\ESC[97;9u\ESC[97;7u" [cK 'a', aK 'a', withSuper (key (Char 'a')), withAlt (cK 'a')],
    testCase "Escape, Enter, Tab and Backspace as CSI u" $
      chords "\ESC[27u\ESC[13u\ESC[9;2u\ESC[127;5u" [key Escape, key Enter, withShift (key Tab), withCtrl (key Backspace)],
    testCase "Ctrl+Shift+a is C-A" $ chords "\ESC[97;6u" [cK 'A'],
    -- D36: where only the unshifted key is reported, the chord keeps shift.
    testCase "without alternate keys, Ctrl+Shift+/ is C-S-/" $ chords "\ESC[47;6u" [withShift (cK '/')],
    testCase "with the shifted key, Ctrl+Shift+/ is C-?" $ chords "\ESC[47:63;6u" [cK '?'],
    testCase "a shifted key without shift held is not applied" $ chords "\ESC[47:63;5u" [cK '/'],
    testCase "the base layout key" $
      decodes "\ESC[1081::113;5u" [Key (kittyEvent (cK 'й')) {base = Just 'q'}],
    testCase "repeat and release" $
      decodes
        "\ESC[97;1:2u\ESC[97;1:3u\ESC[1;1:3A\ESC[3;5:3~"
        [ Key (acting Repeat (kittyEvent (key (Char 'a')))),
          Key (acting Release (kittyEvent (key (Char 'a')))),
          Key (acting Release (kittyEvent (key ArrowUp))),
          Key (acting Release (kittyEvent (withCtrl (key DeleteKey))))
        ],
    testCase "associated text" $
      decodes "\ESC[97;;97u\ESC[47;2;63u" [Key (kittyEvent (key (Char 'a'))) {text = "a"}, Key (kittyEvent (sK '/')) {text = "?"}],
    testCase "keyText prefers the associated text, and a release types nothing" $
      map keyText [(keyEvent (sK '/')) {text = "?"}, acting Release (keyEvent (key (Char 'a')))] @?= [Just "?", Nothing],
    testCase "hyper, meta and the locks" $
      decodes
        "\ESC[97;17u\ESC[97;33u\ESC[97;129u"
        [ Key (kittyEvent (key (Char 'a'))) {hyper = True},
          Key (kittyEvent (key (Char 'a'))) {meta = True},
          Key (kittyEvent (key (Char 'a'))) {numLock = True}
        ],
    -- The shifted key is the capital whatever the lock, so Caps Lock comes first.
    testCase "regression 38: Caps Lock and shift with the shifted key give the small letter" $
      decodes
        "\ESC[97:65;66u\ESC[97:65;65u"
        [Key (kittyEvent (key (Char 'a'))) {capsLock = True}, Key (kittyEvent (key (Char 'A'))) {capsLock = True}],
    -- xterm's formatOtherKeys CSI u sends the character shift typed, as modifyOtherKeys does.
    testCase "regression 39: shift alone on a character with no shifted key is applied" $ do
      decodes "\ESC[33;2u\ESC[49;6u" [Key (kittyEvent (key (Char '!'))), Key (kittyEvent (withShift (cK '1')))]
      [keyText e | Key e <- decodeAll ["\ESC[33;2u"]] @?= [Just "!"],
    -- xterm's CSI u has no event type and no functional code: with either it
    -- is kitty's, whose code is the unshifted key (found by M2.8's roundtrip).
    testCase "regression 40: shift alone with an event type or a keypad code is kept" $
      decodes
        "\ESC[32;2:3u\ESC[49;2:2u\ESC[57399;2u\ESC[57399;2:3u"
        [ Key (acting Release (kittyEvent (withShift (key (Char ' '))))),
          Key (acting Repeat (kittyEvent (withShift (key (Char '1'))))),
          Key (kittyEvent (withShift (key (Char '0')))),
          Key (acting Release (kittyEvent (withShift (key (Char '0')))))
        ],
    -- The keypad's 5 is the key '5' here, so its base layout key says nothing.
    testCase "regression 41: a base layout key that is the key is not reported" $
      decodes "\ESC[57404::53u\ESC[1081::113u" [Key (kittyEvent (key (Char '5'))), Key (kittyEvent (key (Char 'й'))) {base = Just 'q'}],
    testCase "Caps Lock applies to a letter with no other modifier" $
      decodes
        "\ESC[97;65u\ESC[97;66u\ESC[97;69u"
        [ Key (kittyEvent (key (Char 'A'))) {capsLock = True},
          Key (kittyEvent (key (Char 'a'))) {capsLock = True},
          Key (kittyEvent (cK 'a')) {capsLock = True}
        ],
    testCase "functional keys" $
      chords
        "\ESC[57376u\ESC[57398u\ESC[57399u\ESC[57414u\ESC[57417u\ESC[57427u\ESC[57358u\ESC[57363u\ESC[57428u\ESC[57440u"
        [key (F 13), key (F 35), key (Char '0'), key Enter, key ArrowLeft, key Begin, key CapsLock, key ContextMenu, key (Media MediaPlay), key (Media VolumeMute)],
    testCase "modifier keys" $
      chords "\ESC[57441;2u\ESC[57448;5u\ESC[57453u" [withShift (key (Modifier LeftShift)), withCtrl (key (Modifier RightControl)), key (Modifier IsoLevel3Shift)],
    testCase "an unassigned functional code is unknown" $ decodes "\ESC[57364u" [Unknown "\ESC[57364u"]
  ]

pastes :: [TestTree]
pastes =
  [ testCase "a paste is its text, escapes and all" $
      decodes "\ESC[200~a\ESC[Ab\r\ESC[201~x" [Paste "a\ESC[Ab\r", Key (keyEvent (key (Char 'x')))],
    testCase "a paste waits with no timeout" $ do
      let (evs, d) = decode "\ESC[200~ab\ESC[20" decoder
      evs @?= []
      waiting d @?= False
      fst (expire d) @?= []
      fst (decode "1~" d) @?= [Paste "ab"],
    testCase "a paste in many pieces" $
      decodeAll (map BS.singleton (BS.unpack "\ESC[200~h\195\169llo\ESC[201~")) @?= [Paste "héllo"],
    testCase "an invalid byte in a paste is U+FFFD" $ decodes "\ESC[200~a\xffz\ESC[201~" [Paste "a\xfffdz"]
  ]

mice :: [TestTree]
mice =
  [ testCase "press and release" $
      decodes
        "\ESC[<0;10;5M\ESC[<0;10;5m"
        [ Mouse MouseEvent {action = MousePress, button = Just LeftButton, mods = noMods, position = AtCell 9 4},
          Mouse MouseEvent {action = MouseRelease, button = Just LeftButton, mods = noMods, position = AtCell 9 4}
        ],
    testCase "motion with and without a button, and modifiers" $
      decodes
        "\ESC[<35;1;1M\ESC[<54;3;2M"
        [ Mouse MouseEvent {action = MouseMotion, button = Nothing, mods = noMods, position = AtCell 0 0},
          Mouse MouseEvent {action = MouseMotion, button = Just RightButton, mods = noMods {shift = True, ctrl = True}, position = AtCell 2 1}
        ],
    testCase "wheel and extra buttons" $
      decodes
        "\ESC[<64;1;1M\ESC[<67;1;1M\ESC[<129;1;1M\ESC[<9;1;1M"
        [ Mouse MouseEvent {action = MousePress, button = Just WheelUp, mods = noMods, position = AtCell 0 0},
          Mouse MouseEvent {action = MousePress, button = Just WheelRight, mods = noMods, position = AtCell 0 0},
          Mouse MouseEvent {action = MousePress, button = Just Button9, mods = noMods, position = AtCell 0 0},
          Mouse MouseEvent {action = MousePress, button = Just MiddleButton, mods = noMods {alt = True}, position = AtCell 0 0}
        ],
    testCase "regression 34: button bits that name no button are unknown" $
      decodes "\ESC[<195;1;1M" [Unknown "\ESC[<195;1;1M"],
    testCase "SGR pixels" $
      fst (decode "\ESC[<0;0;37M" (setPixelMouse True decoder))
        @?= [Mouse MouseEvent {action = MousePress, button = Just LeftButton, mods = noMods, position = AtPixel 0 37}]
  ]

focusResize :: [TestTree]
focusResize =
  [ testCase "focus" $ decodes "\ESC[I\ESC[O" [Focus True, Focus False],
    testCase "in-band resize" $ decodes "\ESC[48;24;80;480;800t" [Resize Size {columns = 80, rows = 24, width = 800, height = 480}]
  ]

replies :: [TestTree]
replies =
  [ testCase "DA1 and DA2" $ decodes "\ESC[?62;22c\ESC[>1;10;0c" [Reply (PrimaryAttributes [62, 22]), Reply (SecondaryAttributes [1, 10, 0])],
    testCase "DECRPM" $
      decodes "\ESC[?2026;2$y\ESC[4;0$y" [Reply (ModeReport DecMode 2026 Reset), Reply (ModeReport AnsiMode 4 NotRecognized)],
    testCase "kitty keyboard flags" $ decodes "\ESC[?31u" [Reply (KittyKeyboardFlags 31)],
    testCase "a cursor position report not on row 1" $ decodes "\ESC[12;40R" [Reply (CursorPosition 11 39)],
    testCase "a cursor position report on row 1, when one is due" $ do
      let d = expectCursorPosition decoder
          (evs, d') = decode "\ESC[1;5R\ESC[1;5R" d
      evs @?= [Reply (CursorPosition 0 4), Key (keyEvent (withCtrl (key (F 3))))]
      waiting d' @?= False,
    -- A terminal answers in order, so DA1's reply ends a report a terminal never sent.
    testCase "regression 37: DA1's reply ends the cursor reports due" $
      fst (decode "\ESC[?62c\ESC[1;5R" (expectCursorPosition decoder))
        @?= [Reply (PrimaryAttributes [62]), Key (keyEvent (withCtrl (key (F 3))))],
    -- A held reply is scanned once, not copied and rescanned on each read.
    localOption (mkTimeout 20000000) $
      testCase "regression 35: a large reply in small reads takes linear time" $ do
        let payload = BS.replicate (4 * 1024 * 1024) 0x41
            reply = "\ESC]52;c;" <> payload <> "\a"
            chunks = cutAt [512, 1024 .. BS.length reply] reply
            (evs, d) = foldl' (\(acc, dd) c -> first (acc <>) (decode c dd)) ([], decoder) chunks
        map (\case Reply (Clipboard sel bytes) -> Just (sel, BS.length bytes); _ -> Nothing) evs @?= [Just ("c", 3 * 1024 * 1024)]
        waiting d @?= False,
    testCase "colours" $
      decodes
        "\ESC]11;rgb:ffff/8000/0000\a\ESC]10;rgb:f/80/000\ESC\\\ESC]4;3;rgb:12/34/56\a"
        [ Reply (ColorReport BackgroundColor (Rgb 255 128 0)),
          Reply (ColorReport ForegroundColor (Rgb 255 128 0)),
          Reply (ColorReport (PaletteColor 3) (Rgb 0x12 0x34 0x56))
        ],
    testCase "OSC 52" $ decodes "\ESC]52;c;aGVsbG8=\a\ESC]52;c;aGk\a" [Reply (Clipboard "c" "hello"), Reply (Clipboard "c" "hi")],
    testCase "XTVERSION" $ decodes "\ESCP>|ghostty 1.0.0\ESC\\" [Reply (TerminalVersion "ghostty 1.0.0")],
    testCase "colour scheme" $ decodes "\ESC[?997;1n\ESC[?997;2n" [Reply (ColorSchemeReport Dark), Reply (ColorSchemeReport Light)],
    testCase "window reports" $
      decodes "\ESC[4;480;800t\ESC[8;24;80t\ESC[6;20;10t" [Reply (TextAreaPixels 800 480), Reply (TextAreaCells 80 24), Reply (CellPixels 10 20)]
  ]

unknowns :: [TestTree]
unknowns =
  [ testCase "an unknown sequence is kept" $ decodes "\ESC[99xa" [Unknown "\ESC[99x", Key (keyEvent (key (Char 'a')))],
    testCase "an invalid byte is kept" $ decodes "\xff\&a" [Unknown "\xff", Key (keyEvent (key (Char 'a')))],
    testCase "an unknown OSC is kept" $ decodes "\ESC]99;x\a" [Unknown "\ESC]99;x\a"],
    testCase "a stray paste end is unknown" $ decodes "\ESC[201~" [Unknown "\ESC[201~"]
  ]

-- | Complete pieces of input, none of which is cut by the timeout: a piece
-- that starts with ESC is a whole sequence, so no piece leaves an ESC that
-- a later one could complete differently.
genStream :: Gen [ByteString]
genStream = listOf (elements corpus)
  where
    corpus =
      [ "a",
        encodeUtf8 ("é" :: Text),
        encodeUtf8 ("😀" :: Text),
        "\x01",
        "\r",
        "\DEL",
        "\ESCa",
        "\ESC\DEL",
        "\ESC[A",
        "\ESC[1;5C",
        "\ESCOP",
        "\ESC[15;2~",
        "\ESC[27;5;9~",
        "\ESC[97;6u",
        "\ESC[47:63;6u",
        "\ESC[97;1:3u",
        "\ESC[97;;97u",
        "\ESC[200~x\ESC[Ay\ESC[201~",
        "\ESC[<0;3;4M",
        "\ESC[I",
        "\ESC[?62;22c",
        "\ESC]11;rgb:ffff/0000/0000\a",
        "\ESC]52;c;aGk=\ESC\\",
        "\ESC]52;c;" <> BS.replicate 300 0x41 <> "\a",
        "\ESCOa",
        "\ESCP>|t 1\ESC\\",
        "\ESC\ESC[B",
        "\ESC[99x",
        "\xff"
      ]

-- | Where to cut bytes of a length.
genCuts :: Int -> Gen [Int]
genCuts n = do
  k <- chooseInt (0, 6)
  sort <$> vectorOf k (chooseInt (0, n))

-- | Bytes cut at offsets.
cutAt :: [Int] -> ByteString -> [ByteString]
cutAt cuts b = go 0 cuts
  where
    go from = \case
      [] -> [BS.drop from b]
      c : cs -> BS.take (c - from) (BS.drop from b) : go c cs

-- | A key event with another action. The update names a field only
-- 'KeyEvent' has, since @action@ alone is 'MouseEvent''s too.
acting :: KeyAction -> KeyEvent -> KeyEvent
acting a e = e {action = a, hyper = e.hyper}
