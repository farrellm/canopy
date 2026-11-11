-- | The encoders against the linked library: keys and mice (with their
-- event handles and the position shims), focus, size and mode reports,
-- colour-scheme reports, and paste, with and without a terminal.
--
-- These are smoke tests of the raw binding, each checked against what the
-- sequence is specified to be. The encoders are tendril's input-decoding
-- oracle (DESIGN.md §11.2), tested through @ghostty-effectful@.
module Test.Ghostty.FFI.Encode (tests) where

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CBool)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castFunPtrToPtr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Ghostty.FFI.ColorScheme
import Ghostty.FFI.Device
import Ghostty.FFI.Focus
import Ghostty.FFI.Io
import Ghostty.FFI.Key.Encoder
import Ghostty.FFI.Key.Event
import Ghostty.FFI.Layout (CStruct (..))
import Ghostty.FFI.Modes
import Ghostty.FFI.Mouse.Encoder
import Ghostty.FFI.Mouse.Event
import Ghostty.FFI.Paste
import Ghostty.FFI.SizeReport
import Ghostty.FFI.Terminal
import Ghostty.FFI.Terminal.Callbacks
import Ghostty.FFI.Types
import Test.Ghostty.FFI.Support
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | The C MIME reader of @test/cbits/gvtffi_test.c@: writes its userdata, a
-- NUL-terminated string.
foreign import ccall "&gvtffi_test_mime_reader"
  testMimeReader :: GhosttyMimeReaderFn

tests :: TestTree
tests =
  testGroup
    "Encode"
    [ testGroup
        "keys"
        [ testCase "a key event keeps what is set on it" $
            withKeyEvent $ \e -> do
              ghosttyKeyEventSetAction e GhosttyKeyActionRepeat >>= (@?= GhosttySuccess)
              ghosttyKeyEventSetKey e GhosttyKeyF5 >>= (@?= GhosttySuccess)
              ghosttyKeyEventSetMods e (GhosttyModsCtrl + GhosttyModsCtrlSide) >>= (@?= GhosttySuccess)
              ghosttyKeyEventSetConsumedMods e GhosttyModsShift >>= (@?= GhosttySuccess)
              ghosttyKeyEventSetComposing e 1 >>= (@?= GhosttySuccess)
              ghosttyKeyEventSetUnshiftedCodepoint e 0x61 >>= (@?= GhosttySuccess)
              ghosttyKeyEventGetAction e >>= (@?= GhosttyKeyActionRepeat)
              ghosttyKeyEventGetKey e >>= (@?= GhosttyKeyF5)
              ghosttyKeyEventGetMods e >>= (@?= GhosttyModsCtrl + GhosttyModsCtrlSide)
              ghosttyKeyEventGetConsumedMods e >>= (@?= GhosttyModsShift)
              ghosttyKeyEventGetComposing e >>= (@?= 1)
              ghosttyKeyEventGetUnshiftedCodepoint e >>= (@?= 0x61)
              BS.useAsCStringLen "\xc3\xa9" $ \(s, n) -> do
                with (GhosttyString (castPtr s) (fromIntegral n)) (ghosttyKeyEventSetUtf8 e) >>= (@?= GhosttySuccess)
                utf8 e >>= (@?= "\xc3\xa9")
                -- A string of length zero clears the text; its pointer is not read.
                with (GhosttyString nullPtr 0) (ghosttyKeyEventSetUtf8 e) >>= (@?= GhosttySuccess)
                utf8 e >>= (@?= ""),
          testCase "a key event refuses a key it does not know, unchanged" $
            withKeyEvent $ \e -> do
              ghosttyKeyEventSetKey e GhosttyKeyF5 >>= (@?= GhosttySuccess)
              ghosttyKeyEventSetKey e (GhosttyKey 0x7fff) >>= (@?= GhosttyInvalidValue)
              ghosttyKeyEventGetKey e >>= (@?= GhosttyKeyF5),
          testCase "legacy: text, Ctrl-C, an arrow" $
            withKeyEncoder $ \enc -> do
              keyBytes enc GhosttyKeyA 0 (Just "a") >>= (@?= "a")
              keyBytes enc GhosttyKeyC GhosttyModsCtrl (Just "c") >>= (@?= "\x03")
              keyBytes enc GhosttyKeyArrowUp 0 Nothing >>= (@?= "\ESC[A"),
          testCase "options from a terminal: cursor-key application mode (DECCKM)" $
            withTerminal 80 24 $ \t -> withKeyEncoder $ \enc -> do
              feed t "\ESC[?1h"
              ghosttyKeyEncoderSetFromTerminal enc t >>= (@?= GhosttySuccess)
              keyBytes enc GhosttyKeyArrowUp 0 Nothing >>= (@?= "\ESCOA"),
          testCase "an option set directly: the Kitty protocol's disambiguate flag" $
            withKeyEncoder $ \enc -> do
              with GhosttyKittyKeyDisambiguate $ \v ->
                ghosttyKeyEncoderSet enc GhosttyKeyEncoderOptKittyFlags (castPtr v) >>= (@?= GhosttySuccess)
              keyBytes enc GhosttyKeyEscape 0 Nothing >>= (@?= "\ESC[27u")
              -- A null value restores the default: no Kitty flags.
              ghosttyKeyEncoderSet enc GhosttyKeyEncoderOptKittyFlags nullPtr >>= (@?= GhosttySuccess)
              keyBytes enc GhosttyKeyEscape 0 Nothing >>= (@?= "\ESC")
        ],
      testGroup
        "mice"
        [ testCase "the position shims set and get a position" $
            withMouseEvent $ \e -> do
              with (GhosttyMousePosition 12.5 3.25) (ghosttyMouseEventSetPosition e) >>= (@?= GhosttySuccess)
              alloca $ \pp -> do
                ghosttyMouseEventGetPosition e pp
                peek pp >>= (@?= GhosttyMousePosition 12.5 3.25),
          testCase "a mouse event keeps its button, or none" $
            withMouseEvent $ \e -> do
              ghosttyMouseEventSetButton e GhosttyMouseButtonRight >>= (@?= GhosttySuccess)
              alloca $ \bp -> do
                ghosttyMouseEventGetButton e bp >>= (@?= GhosttySuccess)
                peek bp >>= (@?= GhosttyMouseButtonRight)
                ghosttyMouseEventClearButton e >>= (@?= GhosttySuccess)
                ghosttyMouseEventGetButton e bp >>= (@?= GhosttyNoValue)
              ghosttyMouseEventSetMods e GhosttyModsAlt >>= (@?= GhosttySuccess)
              ghosttyMouseEventGetMods e >>= (@?= GhosttyModsAlt),
          testCase "SGR: a left press, in cells counted from 1" $
            withMouseEncoder $ \enc -> withMouseEvent $ \e -> do
              setMouseOpt enc GhosttyMouseEncoderOptEvent GhosttyMouseTrackingNormal
              setMouseOpt enc GhosttyMouseEncoderOptFormat GhosttyMouseFormatSgr
              setMouseOpt enc GhosttyMouseEncoderOptSize (GhosttyMouseEncoderSize 0x28 800 480 10 20 0 0 0 0)
              ghosttyMouseEventSetAction e GhosttyMouseActionPress >>= (@?= GhosttySuccess)
              ghosttyMouseEventSetButton e GhosttyMouseButtonLeft >>= (@?= GhosttySuccess)
              with (GhosttyMousePosition 25 45) (ghosttyMouseEventSetPosition e) >>= (@?= GhosttySuccess)
              bytesVia (ghosttyMouseEncoderEncode enc e) >>= (@?= "\ESC[<0;3;3M"),
          testCase "options from a terminal: no tracking until the program asks" $
            withTerminal 80 24 $ \t -> withMouseEncoder $ \enc -> withMouseEvent $ \e -> do
              setMouseOpt enc GhosttyMouseEncoderOptSize (GhosttyMouseEncoderSize 0x28 800 480 10 20 0 0 0 0)
              ghosttyMouseEventSetAction e GhosttyMouseActionPress >>= (@?= GhosttySuccess)
              ghosttyMouseEventSetButton e GhosttyMouseButtonLeft >>= (@?= GhosttySuccess)
              ghosttyMouseEncoderSetFromTerminal enc t >>= (@?= GhosttySuccess)
              bytesVia (ghosttyMouseEncoderEncode enc e) >>= (@?= "")
              feed t "\ESC[?1000h\ESC[?1006h"
              ghosttyMouseEncoderSetFromTerminal enc t >>= (@?= GhosttySuccess)
              ghosttyMouseEncoderReset enc >>= (@?= GhosttySuccess)
              bytesVia (ghosttyMouseEncoderEncode enc e) >>= (@?= "\ESC[<0;1;1M")
        ],
      testGroup
        "reports"
        [ testCase "focus gained and lost" $ do
            bytesVia (ghosttyFocusEncode GhosttyFocusGained) >>= (@?= "\ESC[I")
            bytesVia (ghosttyFocusEncode GhosttyFocusLost) >>= (@?= "\ESC[O"),
          testCase "a size report through its shim (CSI 18 t)" $
            with (GhosttySizeReportSize 24 80 9 18) $ \sp ->
              bytesVia (ghosttySizeReportEncode GhosttySizeReportCsi18T sp) >>= (@?= "\ESC[8;24;80t"),
          testCase "mode reports, DEC private and ANSI" $ do
            bytesVia (ghosttyModeReportEncode GhosttyModeCursorKeys GhosttyModeReportSet) >>= (@?= "\ESC[?1;1$y")
            bytesVia (ghosttyModeReportEncode GhosttyModeInsert GhosttyModeReportReset) >>= (@?= "\ESC[4;2$y"),
          testCase "a colour-scheme report" $
            bytesVia (ghosttyColorSchemeReportEncode GhosttyColorSchemeDark) >>= (@?= "\ESC[?997;1n")
        ],
      testGroup
        "paste"
        [ testCase "the safety check refuses line breaks and the bracket end" $ do
            isSafe "plain text" >>= (@?= 1)
            isSafe "two\nlines" >>= (@?= 0)
            isSafe "cr\r" >>= (@?= 0)
            isSafe "x\ESC[201~y" >>= (@?= 0),
          testCase "encoding without a terminal: newlines, brackets, control bytes" $ do
            pasteEncode "a\nb" 0 >>= (@?= "a\rb")
            pasteEncode "a\nb" 1 >>= (@?= "\ESC[200~a\nb\ESC[201~")
            pasteEncode "a\ESCb" 0 >>= (@?= "a b"),
          testCase "into a terminal: rejected, then allowed, through WRITE_PTY (safe)" $
            withPasteTerminal $ \t out -> do
              paste t "hi\nthere" 0 >>= (@?= (GhosttyRejected, 0))
              readIORef out >>= (@?= "")
              paste t "hi\nthere" 1 >>= (@?= (GhosttySuccess, 1))
              readIORef out >>= (@?= "hi\rthere"),
          testCase "into a terminal in bracketed paste mode (2004)" $
            withPasteTerminal $ \t out -> do
              feed t "\ESC[?2004h"
              paste t "hi\nthere" 0 >>= (@?= (GhosttySuccess, 1))
              readIORef out >>= (@?= "\ESC[200~hi\nthere\ESC[201~")
        ]
    ]

-- * Keys

withKeyEvent :: (GhosttyKeyEvent -> IO a) -> IO a
withKeyEvent = bracket (newHandle ghosttyKeyEventNew) ghosttyKeyEventFree

withKeyEncoder :: (GhosttyKeyEncoder -> IO a) -> IO a
withKeyEncoder = bracket (newHandle ghosttyKeyEncoderNew) ghosttyKeyEncoderFree

-- | A key press, with its text if it has any.
keyBytes :: GhosttyKeyEncoder -> GhosttyKey -> GhosttyMods -> Maybe ByteString -> IO ByteString
keyBytes enc key mods text = withKeyEvent $ \e -> do
  ghosttyKeyEventSetAction e GhosttyKeyActionPress >>= (@?= GhosttySuccess)
  ghosttyKeyEventSetKey e key >>= (@?= GhosttySuccess)
  ghosttyKeyEventSetMods e mods >>= (@?= GhosttySuccess)
  let encode = bytesVia (ghosttyKeyEncoderEncode enc e)
  case text of
    Nothing -> encode
    Just b -> BS.useAsCStringLen b $ \(p, n) -> do
      with (GhosttyString (castPtr p) (fromIntegral n)) (ghosttyKeyEventSetUtf8 e) >>= (@?= GhosttySuccess)
      encode

-- | A key event's text, copied out.
utf8 :: GhosttyKeyEvent -> IO ByteString
utf8 e = alloca $ \sp -> ghosttyKeyEventGetUtf8 e sp >> peek sp >>= ghosttyStringBytes

-- * Mice

withMouseEvent :: (GhosttyMouseEvent -> IO a) -> IO a
withMouseEvent = bracket (newHandle ghosttyMouseEventNew) ghosttyMouseEventFree

withMouseEncoder :: (GhosttyMouseEncoder -> IO a) -> IO a
withMouseEncoder = bracket (newHandle ghosttyMouseEncoderNew) ghosttyMouseEncoderFree

setMouseOpt :: (Storable a) => GhosttyMouseEncoder -> GhosttyMouseEncoderOption -> a -> IO ()
setMouseOpt enc o v = withValue v (ghosttyMouseEncoderSet enc o . castPtr)

-- * Paste

isSafe :: ByteString -> IO CBool
isSafe b = BS.useAsCStringLen b $ \(p, n) -> ghosttyPasteIsSafe p (fromIntegral n)

-- | Encode a copy of the data, which the call modifies in place.
pasteEncode :: ByteString -> CBool -> IO ByteString
pasteEncode b bracketed = BS.useAsCStringLen b $ \(p, n) ->
  bytesVia (ghosttyPasteEncode p (fromIntegral n) bracketed)

-- | A terminal whose @WRITE_PTY@ output collects in the 'IORef'.
withPasteTerminal :: (GhosttyTerminal -> IORef ByteString -> IO ()) -> IO ()
withPasteTerminal act = withTerminal 80 24 $ \t -> do
  out <- newIORef BS.empty
  let onWrite _ _ p n = BS.packCStringLen (castPtr p, fromIntegral n) >>= \b -> modifyIORef' out (<> b)
  withCallback (mkGhosttyTerminalWritePtyFn onWrite) $ \fn -> do
    ghosttyTerminalSet t GhosttyTerminalOptWritePty (castFunPtrToPtr fn) >>= (@?= GhosttySuccess)
    act t out

-- | Paste one @text/plain@ representation, read by the C reader, and whether
-- anything was written.
paste :: GhosttyTerminal -> String -> CBool -> IO (GhosttyResult, CBool)
paste t text allowUnsafe =
  withCString text $ \txt -> BS.useAsCStringLen "text/plain" $ \(m, mn) ->
    with (GhosttyString (castPtr m) (fromIntegral mn)) $ \mime ->
      alloca $ \wp -> do
        -- Size, location, source, MIME types and their count, reader, and
        -- whether to write text that could inject commands.
        let request =
              GhosttyPaste
                (fromIntegral ghosttyPasteLayout.size)
                GhosttyClipboardLocationStandard
                GhosttyPasteSourceClipboard
                mime
                1
                (GhosttyMimeReader testMimeReader (castPtr (txt :: CString)))
                allowUnsafe
        r <- with request $ \pp -> ghosttyTerminalPaste t pp wp
        (r,) <$> peek wp
