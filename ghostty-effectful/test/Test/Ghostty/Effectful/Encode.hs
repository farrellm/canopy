{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The encoders (PLAN.md M1.9, DESIGN.md §4.8): keys and mice under their
-- options and under a terminal's modes, the reports, and paste with and
-- without a terminal. Each expected sequence is the one the protocol
-- specifies. These are tendril's oracles (§11.2), so the closed types are
-- held against the header too.
module Test.Ghostty.Effectful.Encode (tests) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Effectful.Exception qualified as E
import Ghostty.Effectful.Encode
import Ghostty.Effectful.Event (TerminalEvent (..))
import Ghostty.Effectful.Internal.Encode (keyCode, modsBits)
import Ghostty.Effectful.Internal.Size (CellPixels (..), Extent (..))
import Ghostty.Effectful.Mode (ModeKey (..))
import Ghostty.Effectful.Terminal (ColorScheme (..), KittyFlags (..), feed, noKittyFlags)
import Ghostty.FFI.Key.Event (GhosttyKey (..), ghosttyKeyEnum, pattern GhosttyModsCtrl, pattern GhosttyModsCtrlSide, pattern GhosttyModsShift)
import Ghostty.FFI.Layout (CEnum (..))
import Test.Ghostty.Effectful.Support (ghostty, grid, rowName, squash, withFresh)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Encode"
    [ testGroup
        "keys"
        [ testCase "every key is a header value, each once, and the header has no other" $
            sort (map (\k -> let GhosttyKey n = keyCode k in toInteger n) universe)
              @?= sort (map snd ghosttyKeyEnum.values),
          testCase "every key is its header row's name" $
            filter
              (\k -> fmap squash (rowName ghosttyKeyEnum (let GhosttyKey n = keyCode k in toInteger n)) /= Just (squash ("GHOSTTY_" <> show k)))
              universe
              @?= [],
          testCase "modifiers: a side bit only with its modifier" $ do
            modsBits noMods {ctrl = Just RightSide} @?= GhosttyModsCtrl + GhosttyModsCtrlSide
            modsBits noMods {ctrl = Just LeftSide, shift = Just LeftSide} @?= GhosttyModsCtrl + GhosttyModsShift
            modsBits noMods @?= 0,
          testCase "legacy: text, Ctrl-C, an arrow, a bare modifier" $ do
            r <- ghostty $ withKeyEncoder defaultKeyOptions $ \enc ->
              traverse
                (encodeKey enc)
                [ typed KeyA "a" noMods,
                  typed KeyC "c" noMods {ctrl = Just LeftSide},
                  keyPress KeyArrowUp,
                  keyPress KeyShiftLeft
                ]
            r @?= ["a", "\ETX", "\ESC[A", ""],
          testCase "control characters in a key's text are dropped: the key is encoded from its code" $ do
            r <- ghostty $ withKeyEncoder defaultKeyOptions $ \enc ->
              traverse (encodeKey enc) [keyPress KeyEnter `withText` "\r", keyPress KeyEnter]
            r @?= ["\r", "\r"],
          testCase "options from a terminal: cursor-key application mode (DECCKM)" $ do
            r <- withFresh $ \t -> withKeyEncoder defaultKeyOptions $ \enc -> do
              before <- encodeKey enc (keyPress KeyArrowUp)
              _ <- feed t "\ESC[?1h"
              keyOptionsFrom enc t
              after <- encodeKey enc (keyPress KeyArrowUp)
              pure (before, after)
            r @?= ("\ESC[A", "\ESCOA"),
          testCase "options set directly: the Kitty protocol, and set back" $ do
            r <- ghostty $ withKeyEncoder defaultKeyOptions {kittyFlags = noKittyFlags {disambiguate = True}} $ \enc -> do
              kitty <- encodeKey enc (keyPress KeyEscape)
              setKeyEncoder enc defaultKeyOptions
              legacy <- encodeKey enc (keyPress KeyEscape)
              pure (kitty, legacy)
            r @?= ("\ESC[27u", "\ESC"),
          testCase "the Kitty protocol finds a text key by its unshifted character" $ do
            r <- ghostty $ withKeyEncoder defaultKeyOptions {kittyFlags = noKittyFlags {disambiguate = True, reportEvents = True}} $ \enc ->
              traverse (encodeKey enc) [typed KeyA "a" noMods `withAction` KeyRelease, (typed KeyA "a" noMods `withAction` KeyRelease) `withUnshifted` Nothing]
            r @?= ["\ESC[97;1:3u", ""],
          testCase "the Kitty protocol reports a release only when asked" $ do
            let release = typed KeyA "a" noMods `withAction` KeyRelease
            r <- ghostty $ do
              plain <- withKeyEncoder defaultKeyOptions {kittyFlags = noKittyFlags {disambiguate = True}} (`encodeKey` release)
              events <- withKeyEncoder defaultKeyOptions {kittyFlags = noKittyFlags {disambiguate = True, reportEvents = True}} (`encodeKey` release)
              pure (plain, events)
            r @?= ("", "\ESC[97;1:3u"),
          testCase "Backspace is DEL, or BS in backarrow key mode (DECBKM)" $ do
            r <- ghostty $ do
              del <- withKeyEncoder defaultKeyOptions (`encodeKey` keyPress KeyBackspace)
              bs <- withKeyEncoder defaultKeyOptions {backarrowKeyMode = True} (`encodeKey` keyPress KeyBackspace)
              pure (del, bs)
            r @?= ("\DEL", "\BS")
        ],
      testGroup
        "mice"
        [ testCase "SGR: a left press, in cells counted from 1" $ do
            r <- ghostty $ withMouseEncoder sgr $ \enc -> encodeMouse enc (press 25 45)
            r @?= "\ESC[<0;3;3M",
          testCase "SGR: a release, with Ctrl held" $ do
            r <- ghostty $ withMouseEncoder sgr $ \enc ->
              encodeMouse enc MouseEvent {action = MouseRelease, button = Just MouseLeft, mods = noMods {ctrl = Just LeftSide}, position = MousePosition 25 45}
            r @?= "\ESC[<16;3;3m",
          testCase "options from a terminal: nothing until the program asks" $ do
            r <- withFresh $ \t -> withMouseEncoder (mouseOptions surface) $ \enc -> do
              mouseOptionsFrom enc t
              before <- encodeMouse enc (press 5 5)
              _ <- feed t "\ESC[?1000h\ESC[?1006h"
              mouseOptionsFrom enc t
              after <- encodeMouse enc (press 5 5)
              pure (before, after)
            r @?= ("", "\ESC[<0;1;1M"),
          testCase "motion within the last cell is dropped, until it is forgotten" $ do
            let move x = MouseEvent {action = MouseMotion, button = Nothing, mods = noMods, position = MousePosition x 5}
            r <- ghostty $ withMouseEncoder sgr {tracking = AnyTracking, trackLastCell = True} $ \enc -> do
              a <- encodeMouse enc (move 1)
              b <- encodeMouse enc (move 8)
              c <- encodeMouse enc (move 12)
              forgetLastCell enc
              d <- encodeMouse enc (move 12)
              pure [a, b, c, d]
            r @?= ["\ESC[<35;1;1M", "", "\ESC[<35;2;1M", "\ESC[<35;2;1M"],
          testCase "a grid's geometry is its cells, with no margins" $
            gridGeometry (grid 80 24) (Extent 10) (Extent 20)
              @?= MouseGeometry {screenWidth = 800, screenHeight = 480, cellWidth = Extent 10, cellHeight = Extent 20, padding = noPadding}
        ],
      testGroup
        "reports"
        [ testCase "focus gained and lost" $ do
            r <- ghostty (traverse encodeFocus [FocusGained, FocusLost])
            r @?= ["\ESC[I", "\ESC[O"],
          testCase "size reports in every style" $ do
            r <- ghostty (traverse (\s -> encodeSizeReport s (grid 80 24) (CellPixels 9 18)) universe)
            r @?= ["\ESC[48;24;80;432;720t", "\ESC[4;432;720t", "\ESC[6;18;9t", "\ESC[8;24;80t", "\ESC[9;24;80t"],
          testCase "mode reports: DEC private, ANSI, and a mode the library does not know" $ do
            r <- ghostty $ do
              dec <- encodeModeReport (knownMode ModeCursorKeys) ModeIsSet
              ansi <- traverse (`encodeModeReport` ModeIsReset) (modeNumber AnsiMode 4)
              other <- traverse (`encodeModeReport` ModeNotRecognized) (modeNumber DecPrivateMode 9999)
              pure (dec, ansi, other)
            r @?= ("\ESC[?1;1$y", Just "\ESC[4;2$y", Just "\ESC[?9999;0$y"),
          testCase "a mode number past 15 bits is refused" $
            modeNumber DecPrivateMode 0x8000 @?= Nothing,
          testCase "colour-scheme reports" $ do
            r <- ghostty (traverse encodeColorSchemeReport [Dark, Light])
            r @?= ["\ESC[?997;1n", "\ESC[?997;2n"]
        ],
      testGroup
        "paste"
        [ testCase "the safety check refuses line breaks and the bracket end" $ do
            r <- ghostty (traverse isPasteSafe ["plain text", "two\nlines", "cr\r", "x\ESC[201~y"])
            r @?= [True, False, False, False],
          testCase "encoding without a terminal: newlines, brackets, control bytes" $ do
            r <- ghostty (traverse (uncurry encodePaste) [(Unbracketed, "a\nb"), (Bracketed, "a\nb"), (Unbracketed, "a\ESCb")])
            r @?= ["a\rb", "\ESC[200~a\nb\ESC[201~", "a b"],
          testCase "an encoding longer than the first buffer is whole" $ do
            let long = BS.replicate 300 0x61
            r <- ghostty (encodePaste Bracketed long)
            r @?= "\ESC[200~" <> long <> "\ESC[201~",
          testCase "encoding leaves its input unchanged" $ do
            let input = "a\ESCb" :: ByteString
            _ <- ghostty (encodePaste Unbracketed input)
            input @?= "a\ESCb",
          testCase "into a terminal: rejected, then allowed, as a WritePty" $ do
            r <- withFresh $ \t -> do
              refused <- paste t RefuseUnsafe "hi\nthere"
              allowed <- paste t AllowUnsafe "hi\nthere"
              pure (refused, allowed)
            r @?= (Left Rejected, Right [WritePty "hi\rthere"]),
          testCase "into a terminal in bracketed paste mode (2004): line breaks are safe" $ do
            r <- withFresh $ \t -> do
              _ <- feed t "\ESC[?2004h"
              paste t RefuseUnsafe "hi\nthere"
            r @?= Right [WritePty "\ESC[200~hi\nthere\ESC[201~"],
          testCase "an empty paste writes nothing" $ do
            r <- withFresh $ \t -> paste t RefuseUnsafe ""
            r @?= Right []
        ],
      testGroup
        "handles"
        [ testCase "encoders are open inside their scopes and freed after" $ do
            r <- ghostty $ do
              inside <- withKeyEncoder defaultKeyOptions $ \_ -> withMouseEncoder sgr (const openEncoders)
              after <- openEncoders
              pure (inside, after)
            r @?= (2, 0),
          testCase "an encoder is freed when its scope throws" $ do
            r <- ghostty $ do
              e <- E.try @ErrorCall (withKeyEncoder defaultKeyOptions $ \_ -> liftIO (throwIO (ErrorCall "boom")))
              n <- openEncoders
              pure (either (\(ErrorCall m) -> m) (const "no exception") e, n)
            r @?= ("boom", 0)
        ]
    ]

-- | Normal tracking in SGR, on an 80×24 grid of 10×20-pixel cells.
sgr :: MouseOptions
sgr = (mouseOptions surface) {tracking = NormalTracking, format = SgrFormat}

-- | 80×24 cells of 10×20 pixels.
surface :: MouseGeometry
surface = gridGeometry (grid 80 24) (Extent 10) (Extent 20)

-- | A left press at a pixel.
press :: Float -> Float -> MouseEvent
press x y = MouseEvent {action = MousePress, button = Just MouseLeft, mods = noMods, position = MousePosition x y}

-- | A press of a key that types text, as a platform reports one: its
-- unshifted character is the text's.
typed :: Key -> Text -> Mods -> KeyEvent
typed k t m = KeyEvent {action = KeyPress, key = k, mods = m, consumedMods = noMods, composing = False, text = t, unshifted = fst <$> T.uncons t}

-- | The same event, with another action.
withAction :: KeyEvent -> KeyAction -> KeyEvent
withAction e a = KeyEvent {action = a, key = e.key, mods = e.mods, consumedMods = e.consumedMods, composing = e.composing, text = e.text, unshifted = e.unshifted}

-- | The same event, with another unshifted character.
withUnshifted :: KeyEvent -> Maybe Char -> KeyEvent
withUnshifted e u = KeyEvent {action = e.action, key = e.key, mods = e.mods, consumedMods = e.consumedMods, composing = e.composing, text = e.text, unshifted = u}

-- | The same event, with other text.
withText :: KeyEvent -> Text -> KeyEvent
withText e t = KeyEvent {action = e.action, key = e.key, mods = e.mods, consumedMods = e.consumedMods, composing = e.composing, text = t, unshifted = e.unshifted}
