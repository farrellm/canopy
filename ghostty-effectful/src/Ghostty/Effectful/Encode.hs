-- | Encoding input for a terminal program (DESIGN.md §4.8, §8.6): keys and
-- mouse events, focus changes, size, mode and colour-scheme reports, and
-- pastes, each as the bytes the program reads.
--
-- Key and mouse encoders are handles, scoped like terminals (§4.2). Each is
-- configured by its options, which can be set from what a terminal's
-- program asked for (its modes and keyboard flags), and keeps them until
-- they are set again. A mouse encoder also remembers the last cell it
-- reported, when asked to drop motions within one cell. These are also
-- tendril's input-decoding oracle (§11.2).
--
-- > withKeyEncoder defaultKeyOptions $ \enc -> do
-- >   keyOptionsFrom enc t
-- >   encodeKey enc (keyPress KeyArrowUp)   -- "\ESC[A", or "\ESCOA" under DECCKM
module Ghostty.Effectful.Encode
  ( -- * Keys
    KeyEncoder,
    withKeyEncoder,
    setKeyEncoder,
    keyOptionsFrom,
    encodeKey,
    KeyOptions (..),
    defaultKeyOptions,
    OptionAsAlt (..),

    -- * Mice
    MouseEncoder,
    withMouseEncoder,
    setMouseEncoder,
    mouseOptionsFrom,
    forgetLastCell,
    encodeMouse,
    MouseOptions (..),
    mouseOptions,
    MouseTracking (..),
    MouseFormat (..),
    MouseGeometry (..),
    Padding (..),
    noPadding,
    gridGeometry,

    -- * Reports
    Focus (..),
    encodeFocus,
    SizeReportStyle (..),
    encodeSizeReport,
    ModeNumber,
    modeNumber,
    knownMode,
    ModeReport (..),
    encodeModeReport,
    encodeColorSchemeReport,

    -- * Paste
    paste,
    Rejected (..),
    PasteSafety (..),
    isPasteSafe,
    encodePaste,

    -- * Handles
    openEncoders,

    -- * Events
    module Ghostty.Effectful.Input,
  )
where

import Control.Exception (bracket)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Static (unsafeEff_, unsafeSeqUnliftIO)
import Ghostty.Effectful.Event (TerminalEvent)
import Ghostty.Effectful.Input
import Ghostty.Effectful.Internal.Effect (Ghostty, Handles (..), owned)
import Ghostty.Effectful.Internal.Encode
  ( Focus (..),
    KeyEncoder,
    KeyOptions (..),
    ModeNumber,
    ModeReport (..),
    MouseEncoder,
    MouseFormat (..),
    MouseGeometry (..),
    MouseOptions (..),
    MouseTracking (..),
    OptionAsAlt (..),
    Padding (..),
    PasteSafety (..),
    Rejected (..),
    SizeReportStyle (..),
    closeKeyEncoder,
    closeMouseEncoder,
    defaultKeyOptions,
    encodeColorSchemeReportOf,
    encodeFocusEvent,
    encodeKeyEvent,
    encodeModeReportOf,
    encodeMouseEvent,
    encodePasteBytes,
    encodeSizeReportOf,
    gridGeometry,
    keyOptionsFromTerminal,
    knownMode,
    modeNumber,
    mouseOptions,
    mouseOptionsFromTerminal,
    noPadding,
    openKeyEncoder,
    openMouseEncoder,
    pasteInto,
    pasteSafe,
    resetMouse,
    setKeyOptions,
    setMouseOptions,
  )
import Ghostty.Effectful.Internal.Size (CellPixels, GridSize)
import Ghostty.Effectful.Internal.Terminal (Terminal)
import Ghostty.Effectful.Terminal.Keys (ColorScheme)

-- * Keys

-- | Open a key encoder with the given options for the duration of the
-- action, and free it when the action ends, by return or by exception. Its
-- type variable is rank-2, so it cannot leave the action (§4.2).
withKeyEncoder :: (Ghostty :> es) => KeyOptions -> (forall k. KeyEncoder k -> Eff es a) -> Eff es a
withKeyEncoder o k = do
  hs <- owned "withKeyEncoder"
  unsafeSeqUnliftIO $ \unlift ->
    bracket
      (openKeyEncoder o <* modifyIORef' hs.encoderCount (+ 1))
      (\e -> closeKeyEncoder e <* modifyIORef' hs.encoderCount (subtract 1))
      (unlift . k)

-- | Replace every option.
setKeyEncoder :: (Ghostty :> es) => KeyEncoder k -> KeyOptions -> Eff es ()
setKeyEncoder enc o = do
  _ <- owned "setKeyEncoder"
  unsafeEff_ (setKeyOptions enc o)

-- | Set the options from what the terminal's program asked for now: the
-- cursor-key and keypad modes, Alt's ESC prefix, modifyOtherKeys and the
-- Kitty flags. 'macosOptionAsAlt' is not the terminal's, and becomes
-- 'OptionNotAlt'. A later change of the terminal's modes needs another call.
keyOptionsFrom :: (Ghostty :> es) => KeyEncoder k -> Terminal s -> Eff es ()
keyOptionsFrom enc t = do
  _ <- owned "keyOptionsFrom"
  unsafeEff_ (keyOptionsFromTerminal enc t)

-- | The bytes a key event sends; empty for one that sends none (a modifier
-- pressed alone, or a release outside the Kitty protocol).
--
-- >>> import Effectful (runEff)
-- >>> import Ghostty.Effectful (runGhostty)
-- >>> runEff (runGhostty (withKeyEncoder defaultKeyOptions (\e -> encodeKey e (keyPress KeyArrowUp))))
-- Right "\ESC[A"
encodeKey :: (Ghostty :> es) => KeyEncoder k -> KeyEvent -> Eff es ByteString
encodeKey enc e = do
  _ <- owned "encodeKey"
  unsafeEff_ (encodeKeyEvent enc e)

-- * Mice

-- | Open a mouse encoder with the given options for the duration of the
-- action, and free it when the action ends, by return or by exception.
withMouseEncoder :: (Ghostty :> es) => MouseOptions -> (forall m. MouseEncoder m -> Eff es a) -> Eff es a
withMouseEncoder o k = do
  hs <- owned "withMouseEncoder"
  unsafeSeqUnliftIO $ \unlift ->
    bracket
      (openMouseEncoder o <* modifyIORef' hs.encoderCount (+ 1))
      (\e -> closeMouseEncoder e <* modifyIORef' hs.encoderCount (subtract 1))
      (unlift . k)

-- | Replace every option, forgetting the last cell reported.
setMouseEncoder :: (Ghostty :> es) => MouseEncoder m -> MouseOptions -> Eff es ()
setMouseEncoder enc o = do
  _ <- owned "setMouseEncoder"
  unsafeEff_ (setMouseOptions enc o)

-- | Set the tracking and format from what the terminal's program asked for
-- now, forgetting the last cell reported. The geometry and the held button
-- stay as they were.
mouseOptionsFrom :: (Ghostty :> es) => MouseEncoder m -> Terminal s -> Eff es ()
mouseOptionsFrom enc t = do
  _ <- owned "mouseOptionsFrom"
  unsafeEff_ (mouseOptionsFromTerminal enc t)

-- | Forget the last cell reported, so the next motion is reported wherever
-- it is ('trackLastCell').
forgetLastCell :: (Ghostty :> es) => MouseEncoder m -> Eff es ()
forgetLastCell enc = do
  _ <- owned "forgetLastCell"
  unsafeEff_ (resetMouse enc)

-- | The bytes a mouse event sends; empty for one the options do not report.
encodeMouse :: (Ghostty :> es) => MouseEncoder m -> MouseEvent -> Eff es ByteString
encodeMouse enc e = do
  _ <- owned "encodeMouse"
  unsafeEff_ (encodeMouseEvent enc e)

-- * Reports

-- | A focus report (mode 1004).
--
-- >>> import Effectful (runEff)
-- >>> import Ghostty.Effectful (runGhostty)
-- >>> runEff (runGhostty (encodeFocus FocusGained))
-- Right "\ESC[I"
encodeFocus :: (Ghostty :> es) => Focus -> Eff es ByteString
encodeFocus f = do
  _ <- owned "encodeFocus"
  unsafeEff_ (encodeFocusEvent f)

-- | A size report of a grid and its cell size, in the given style.
encodeSizeReport :: (Ghostty :> es) => SizeReportStyle -> GridSize -> CellPixels -> Eff es ByteString
encodeSizeReport style g px = do
  _ <- owned "encodeSizeReport"
  unsafeEff_ (encodeSizeReportOf style g px)

-- | A DECRPM reply: what a mode is, as the answer to DECRQM.
encodeModeReport :: (Ghostty :> es) => ModeNumber -> ModeReport -> Eff es ByteString
encodeModeReport m s = do
  _ <- owned "encodeModeReport"
  unsafeEff_ (encodeModeReportOf m s)

-- | A colour-scheme report (@CSI ? 997 ; 1 n@ for dark, @2@ for light).
encodeColorSchemeReport :: (Ghostty :> es) => ColorScheme -> Eff es ByteString
encodeColorSchemeReport s = do
  _ <- owned "encodeColorSchemeReport"
  unsafeEff_ (encodeColorSchemeReportOf s)

-- * Paste

-- | Paste text into the terminal's program, framed as its modes ask:
-- bracketed (mode 2004), or with its line feeds made carriage returns, and
-- with unsafe control bytes made spaces. The bytes come back as a
-- 'Ghostty.Effectful.Event.WritePty' event, for the caller to write to the
-- PTY, as 'Ghostty.Effectful.Terminal.feed''s replies do; empty text writes
-- nothing. Text that could inject commands under 'RefuseUnsafe' is
-- 'Rejected', and nothing is written: ask, then paste with 'AllowUnsafe'
-- (§8.5).
paste :: (Ghostty :> es) => Terminal s -> PasteSafety -> ByteString -> Eff es (Either Rejected [TerminalEvent])
paste t safety text = do
  _ <- owned "paste"
  unsafeEff_ (pasteInto t safety text)

-- | Whether text holds nothing that could inject commands in any mode: no
-- line break, no bracketed paste terminator. Stricter than 'paste', which
-- allows line breaks inside a bracketed paste.
isPasteSafe :: (Ghostty :> es) => ByteString -> Eff es Bool
isPasteSafe b = do
  _ <- owned "isPasteSafe"
  unsafeEff_ (pasteSafe b)

-- | Text as a paste writes it, without a terminal: bracketed ('True'), or
-- with its line feeds made carriage returns, and with unsafe control bytes
-- made spaces in either case. No safety check is made.
--
-- >>> import Effectful (runEff)
-- >>> import Ghostty.Effectful (runGhostty)
-- >>> runEff (runGhostty (encodePaste False "a\nb"))
-- Right "a\rb"
encodePaste :: (Ghostty :> es) => Bool -> ByteString -> Eff es ByteString
encodePaste bracketed b = do
  _ <- owned "encodePaste"
  unsafeEff_ (encodePasteBytes bracketed b)

-- * Handles

-- | How many key and mouse encoders are open in this
-- 'Ghostty.Effectful.runGhostty'.
openEncoders :: (Ghostty :> es) => Eff es Int
openEncoders = do
  hs <- owned "openEncoders"
  unsafeEff_ (readIORef hs.encoderCount)
