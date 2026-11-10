{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The callbacks the binding installs on every terminal (DESIGN.md §4.5),
-- and the state they answer from.
--
-- A callback runs inside a VT write, so each does one thing: append to the
-- terminal's buffer, or answer a query from the terminal's 'Answers'. Each is
-- 'sheltered': an exception inside one is recorded, not let into C, and thrown
-- by whoever drains the buffer. What needs the terminal (the title after a
-- title change, the cursor after a prompt mark) is a 'Pending' entry,
-- resolved ('resolveOnto') between writes, because a callback must not call back into its
-- terminal. A C enum value the binding does not name is recorded the same
-- way, and becomes 'UnknownEnum' then.
--
-- @ENQUIRY@ and @XTVERSION@ return a struct by value, which no Haskell
-- function can, so they are C (@cbits/gveff_terminal.c@), answering from the
-- terminal's @USERDATA@ (a 'GveffUserdata'), whose two strings this module
-- owns. @RENDER_HOLD@ is C too: when a hold begins it captures the frame into
-- every render state attached to the terminal, which is a C call the header
-- asks for inside the callback (DESIGN.md §4.3), and then calls the Haskell
-- callback that appends the event.
module Ghostty.Effectful.Internal.Callbacks
  ( -- * Pending events
    Pending (..),
    drain,
    resolveOnto,
    joinWrites,

    -- * Answers
    Answers (..),
    Installed,
    installCallbacks,
    registerFunPtr,
    releaseCallbacks,
    setEnquiryAnswer,
    setXtversionAnswer,

    -- * Frame capture
    captureAttach,
    captureDetach,
    captureHeld,
    captureRelease,
    captureFraming,
    Framing (..),
  )
where

import Control.Exception (catch, mask_, onException, throwIO)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Foreign.C.Types (CBool (..), CInt (..), CSize)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (copyBytes, fromBool, toBool, with)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castFunPtrToPtr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), ResultCode (..))
import Ghostty.Effectful.Event
import Ghostty.Effectful.Internal.Convert (colorSchemeCode, emptyString, oscTerminator, stringText, unknownEnum)
import Ghostty.Effectful.Internal.Error (expectSuccess, resultCode, resultError)
import Ghostty.Effectful.Internal.Size (CellPixels (..), GridSize (..), extent, fromExtent)
import Ghostty.Effectful.Point (Point (..))
import Ghostty.Effectful.Terminal.Keys (ColorScheme (..), DeviceAttributes (..), TerminalData (..))
import Ghostty.FFI.Device
import Ghostty.FFI.Layout (CEnum)
import Ghostty.FFI.Osc (ghosttyOscTerminatorEnum)
import Ghostty.FFI.Render (GhosttyRenderState (..))
import Ghostty.FFI.SizeReport (GhosttySizeReportSize (..))
import Ghostty.FFI.Terminal
  ( GhosttyTerminal,
    GhosttyTerminalOption,
    ghosttyTerminalSet,
    pattern GhosttyTerminalOptBell,
    pattern GhosttyTerminalOptClipboardWrite,
    pattern GhosttyTerminalOptColorScheme,
    pattern GhosttyTerminalOptDesktopNotification,
    pattern GhosttyTerminalOptDeviceAttributes,
    pattern GhosttyTerminalOptEnquiry,
    pattern GhosttyTerminalOptProgramStatus,
    pattern GhosttyTerminalOptProgressReport,
    pattern GhosttyTerminalOptPwdChanged,
    pattern GhosttyTerminalOptRenderHold,
    pattern GhosttyTerminalOptReset,
    pattern GhosttyTerminalOptResizeWindow,
    pattern GhosttyTerminalOptSemanticPrompt,
    pattern GhosttyTerminalOptSize,
    pattern GhosttyTerminalOptTitleChanged,
    pattern GhosttyTerminalOptUnknownSequence,
    pattern GhosttyTerminalOptUserdata,
    pattern GhosttyTerminalOptWritePty,
    pattern GhosttyTerminalOptXtversion,
  )
import Ghostty.FFI.Terminal.Callbacks
import Ghostty.FFI.Types (GhosttyResult (..), GhosttyString (..), ghosttyStringBytes)

-- | What a callback appended: an event, or what will make one after the
-- write.
data Pending
  = -- | an event as it is
    Ready TerminalEvent
  | -- | the title changed; read it after the write
    TitlePending
  | -- | the working directory changed; read it after the write
    PwdPending
  | -- | a prompt mark; read the cursor after the write
    PromptPending PromptStep
  | -- | a value of the described enum that the binding does not name, from
    -- the named callback
    Unnamed CallSite CEnum CInt
  | -- | an exception inside a callback, which C must not see
    Failed SomeException

-- | Take what the buffer holds, oldest first, leaving it empty.
drain :: IORef [Pending] -> IO [Pending]
drain buf = do
  ps <- readIORef buf
  writeIORef buf []
  pure (reverse ps)

-- | Turn drained entries into events, newest first onto the given ones,
-- reading what they need from the terminal (through the given read), which
-- is between writes now. Throws what a callback recorded.
resolveOnto :: (forall a. TerminalData a -> IO a) -> [TerminalEvent] -> [Pending] -> IO [TerminalEvent]
resolveOnto getData = foldlM (\acc p -> (: acc) <$> event p)
  where
    event = \case
      Ready e -> pure e
      TitlePending -> TitleChanged <$> getData Title
      PwdPending -> PwdChanged <$> getData Pwd
      PromptPending step -> do
        x <- getData CursorX
        y <- getData CursorY
        pure (SemanticPrompt PromptMark {step, cursor = Point {col = x, row = fromIntegral y}})
      Unnamed site e n -> unknownEnum site e n
      Failed e -> throwIO e

-- | Join each run of adjacent 'WritePty's into one, so a reply goes to the
-- PTY in one write.
joinWrites :: [TerminalEvent] -> [TerminalEvent]
joinWrites = \case
  WritePty a : rest@(WritePty _ : _) ->
    let (more, rest') = writes rest
     in WritePty (BS.concat (a : more)) : joinWrites rest'
  e : rest -> e : joinWrites rest
  [] -> []
  where
    writes = \case
      WritePty b : rest -> first (b :) (writes rest)
      rest -> ([], rest)

-- | What a terminal's query callbacks answer with, and the grid they report.
data Answers = Answers
  { -- | CSI ? 996 n; 'Nothing' ignores it
    scheme :: Maybe ColorScheme,
    -- | DA1, DA2, DA3
    attributes :: DeviceAttributes,
    -- | whether size queries are answered
    sizeQueries :: Bool,
    -- | whether clipboard writes are accepted
    clipboard :: Bool,
    -- | the grid, kept with the terminal's by every resize
    grid :: GridSize,
    -- | the cell size, likewise
    pixels :: CellPixels
  }

-- | What installing made, which 'releaseCallbacks' frees once the terminal
-- is.
data Installed = Installed
  { -- | the Haskell callbacks' function pointers
    funPtrs :: [FunPtr ()],
    -- | the @USERDATA@ the C callbacks answer from
    userdata :: Ptr GveffUserdata
  }

-- | A terminal's @USERDATA@, @GveffUserdata@ in @include/gveff_terminal.h@:
-- ENQ's reply and XTVERSION's name, first, as an array of two strings; then
-- the render states that a hold captures into.
data GveffUserdata

-- | Install every callback but @CLIPBOARD_READ@ on a terminal, appending to
-- the buffer and answering from the answers, with ENQ and XTVERSION
-- answering the given bytes. If one cannot be installed, what was made is
-- freed and the error thrown.
installCallbacks :: GhosttyTerminal -> IORef [Pending] -> IORef Answers -> ByteString -> ByteString -> IO Installed
installCallbacks h buf answers enq xtv = do
  made <- newIORef []
  let register :: IO (FunPtr a) -> IO (FunPtr a)
      register = registerFunPtr made
      wrapped :: IO (FunPtr a) -> GhosttyTerminalOption -> IO ()
      wrapped mk opt = setPtr opt . castFunPtrToPtr =<< register mk
      setPtr opt p = expectSuccess (CallSite "ghostty_terminal_set") =<< ghosttyTerminalSet h opt p
  ud <- gveffUserdataNew
  when (ud == nullPtr) $ throwIO (OutOfMemory (CallSite "gveff_userdata_new"))
  let install = do
        replaceAnswer ud enquiryIndex enq
        replaceAnswer ud xtversionIndex xtv
        setPtr GhosttyTerminalOptUserdata (castPtr ud)
        setPtr GhosttyTerminalOptEnquiry (castFunPtrToPtr gveffEnquiry)
        setPtr GhosttyTerminalOptXtversion (castFunPtrToPtr gveffXtversion)
        wrapped (mkGhosttyTerminalWritePtyFn (onWritePty buf)) GhosttyTerminalOptWritePty
        wrapped (mkGhosttyTerminalBellFn (\_ _ -> push buf (Ready Bell))) GhosttyTerminalOptBell
        wrapped (mkGhosttyTerminalTitleChangedFn (\_ _ -> push buf TitlePending)) GhosttyTerminalOptTitleChanged
        wrapped (mkGhosttyTerminalPwdChangedFn (\_ _ -> push buf PwdPending)) GhosttyTerminalOptPwdChanged
        wrapped (mkGhosttyTerminalSizeFn (onSizeAnswer buf answers)) GhosttyTerminalOptSize
        wrapped (mkGhosttyTerminalColorSchemeFn (onSchemeAnswer buf answers)) GhosttyTerminalOptColorScheme
        wrapped (mkGhosttyTerminalDeviceAttributesFn (onAttributesAnswer buf answers)) GhosttyTerminalOptDeviceAttributes
        wrapped (mkGhosttyTerminalClipboardWriteFn (onClipboardWrite buf answers)) GhosttyTerminalOptClipboardWrite
        wrapped (mkGhosttyTerminalDesktopNotificationFn (onNotification buf)) GhosttyTerminalOptDesktopNotification
        wrapped (mkGhosttyTerminalProgressReportFn (onProgress buf)) GhosttyTerminalOptProgressReport
        wrapped (mkGhosttyTerminalProgramStatusFn (onProgramStatus buf)) GhosttyTerminalOptProgramStatus
        wrapped (mkGhosttyTerminalSemanticPromptFn (onSemanticPrompt buf)) GhosttyTerminalOptSemanticPrompt
        wrapped (mkGhosttyTerminalResetFn (\_ _ -> push buf (Ready Reset))) GhosttyTerminalOptReset
        gveffSetHoldNotify ud =<< register (mkGhosttyTerminalRenderHoldFn (onRenderHold buf ud))
        setPtr GhosttyTerminalOptRenderHold (castFunPtrToPtr gveffRenderHold)
        wrapped (mkGhosttyTerminalUnknownSequenceFn (onUnknownSequence buf)) GhosttyTerminalOptUnknownSequence
        wrapped (mkGhosttyTerminalResizeWindowFn (onResizeWindow buf)) GhosttyTerminalOptResizeWindow
  let installed = (\funPtrs -> Installed {funPtrs, userdata = ud}) <$> readIORef made
  install `onException` (releaseCallbacks =<< installed)
  installed

-- | Make a callback's function pointer and record it to be freed, masked so
-- that no asynchronous exception falls between the two and leaks it. Every
-- pointer 'installCallbacks' makes goes through it (regression 15).
registerFunPtr :: IORef [FunPtr ()] -> IO (FunPtr a) -> IO (FunPtr a)
registerFunPtr made mk = mask_ $ do
  fp <- mk
  fp <$ modifyIORef' made (castFunPtr fp :)

-- | Free the callbacks, the answer strings and the @USERDATA@. Only once the
-- terminal that calls them is freed.
releaseCallbacks :: Installed -> IO ()
releaseCallbacks i = do
  traverse_ freeHaskellFunPtr i.funPtrs
  traverse_ (peekElemOff (answersOf i.userdata) >=> freeString) [0 .. answerCount - 1]
  gveffUserdataFree i.userdata

-- | Set ENQ's reply; empty sends none.
setEnquiryAnswer :: Installed -> ByteString -> IO ()
setEnquiryAnswer i = replaceAnswer i.userdata enquiryIndex

-- | Set XTVERSION's name; empty reports the library's own.
setXtversionAnswer :: Installed -> ByteString -> IO ()
setXtversionAnswer i = replaceAnswer i.userdata xtversionIndex

-- | Replace one of the @USERDATA@ strings with a copy of the bytes, and free
-- the old one. No write is running, so no C callback is reading it.
replaceAnswer :: Ptr GveffUserdata -> Int -> ByteString -> IO ()
replaceAnswer ud k bs = mask_ $ do
  new <-
    if BS.null bs
      then pure emptyString
      else BSU.unsafeUseAsCStringLen bs $ \(src, n) -> do
        dst <- mallocBytes n
        copyBytes dst (castPtr src) n
        pure GhosttyString {ptr = dst, len = fromIntegral n}
  old <- peekElemOff (answersOf ud) k
  pokeElemOff (answersOf ud) k new
  freeString old

-- | The answer strings: @answers@, the struct's first field (C asserts it).
answersOf :: Ptr GveffUserdata -> Ptr GhosttyString
answersOf = castPtr

-- | Where ENQ's reply and XTVERSION's name sit in the @USERDATA@'s
-- @answers@, and its length: @GVEFF_ANSWER_ENQUIRY@, @GVEFF_ANSWER_XTVERSION@
-- and @GVEFF_ANSWER_COUNT@ in @include/gveff_terminal.h@.
enquiryIndex, xtversionIndex, answerCount :: Int
enquiryIndex = 0
xtversionIndex = 1
answerCount = 2

-- | Free a string 'replaceAnswer' made.
freeString :: GhosttyString -> IO ()
freeString s = when (s.ptr /= nullPtr) (free s.ptr)

-- | ENQ's answer, in C (@cbits/gveff_terminal.c@).
foreign import capi "gveff_terminal.h &gveff_enquiry"
  gveffEnquiry :: GhosttyTerminalEnquiryFn

-- | XTVERSION's answer, in C.
foreign import capi "gveff_terminal.h &gveff_xtversion"
  gveffXtversion :: GhosttyTerminalXtversionFn

-- | The render-hold callback, in C: it captures, then calls the notifier.
foreign import capi "gveff_terminal.h &gveff_render_hold"
  gveffRenderHold :: GhosttyTerminalRenderHoldFn

-- | A zeroed @USERDATA@, or null. @unsafe@, as is every call below: none
-- calls back into Haskell, and each is a few loads and stores.
foreign import capi unsafe "gveff_terminal.h gveff_userdata_new"
  gveffUserdataNew :: IO (Ptr GveffUserdata)

-- | Free a @USERDATA@ and its capture list, not the answers' bytes.
foreign import capi unsafe "gveff_terminal.h gveff_userdata_free"
  gveffUserdataFree :: Ptr GveffUserdata -> IO ()

-- | Set the Haskell callback a hold reports to.
foreign import capi unsafe "gveff_terminal.h gveff_set_hold_notify"
  gveffSetHoldNotify :: Ptr GveffUserdata -> GhosttyTerminalRenderHoldFn -> IO ()

-- | Capture into a render state from now on; false if the list cannot grow.
foreign import capi unsafe "gveff_terminal.h gveff_capture_attach"
  gveffCaptureAttach :: Ptr GveffUserdata -> GhosttyRenderState -> IO CBool

-- | Stop capturing into a render state.
foreign import capi unsafe "gveff_terminal.h gveff_capture_detach"
  gveffCaptureDetach :: Ptr GveffUserdata -> GhosttyRenderState -> IO ()

-- | Whether a render state holds a frame captured when a hold began.
foreign import capi unsafe "gveff_terminal.h gveff_capture_held"
  gveffCaptureHeld :: Ptr GveffUserdata -> GhosttyRenderState -> IO CBool

-- | Drop every render state's captured frame.
foreign import capi unsafe "gveff_terminal.h gveff_capture_release"
  gveffCaptureRelease :: Ptr GveffUserdata -> IO ()

-- | Mark a frame of a render state open or closed.
foreign import capi unsafe "gveff_terminal.h gveff_capture_framing"
  gveffCaptureFraming :: Ptr GveffUserdata -> GhosttyRenderState -> CBool -> IO ()

-- | The first failed capture since the last call, or success; and reset it.
foreign import capi unsafe "gveff_terminal.h gveff_capture_failure"
  gveffCaptureFailure :: Ptr GveffUserdata -> IO GhosttyResult

-- * Frame capture

-- | Capture a terminal's frames into a render state when a hold begins, from
-- now on (DESIGN.md §4.3). Out of memory if the list cannot grow.
captureAttach :: Installed -> GhosttyRenderState -> IO ()
captureAttach i rs = do
  ok <- gveffCaptureAttach i.userdata rs
  when (ok == 0) $ throwIO (OutOfMemory (CallSite "gveff_capture_attach"))

-- | Stop capturing into a render state, before it is freed.
captureDetach :: Installed -> GhosttyRenderState -> IO ()
captureDetach i = gveffCaptureDetach i.userdata

-- | Whether a render state holds the frame captured when a hold began, the
-- hold not having ended since.
captureHeld :: Installed -> GhosttyRenderState -> IO Bool
captureHeld i rs = toBool <$> gveffCaptureHeld i.userdata rs

-- | Drop every render state's captured frame: the terminal's owner ended
-- the hold by turning the mode off, which calls no callback.
captureRelease :: Installed -> IO ()
captureRelease i = gveffCaptureRelease i.userdata

-- | Whether a render state has a frame open.
data Framing = FrameOpen | FrameClosed
  deriving stock (Eq, Show)

-- | Mark a frame of a render state open or closed. A hold that begins
-- while one is open does not capture into it, since its rows are being
-- read and an update would invalidate them.
captureFraming :: Installed -> GhosttyRenderState -> Framing -> IO ()
captureFraming i rs = gveffCaptureFraming i.userdata rs . fromBool . (== FrameOpen)

-- * The callbacks

-- | Append to the buffer.
push :: IORef [Pending] -> Pending -> IO ()
push buf p = modifyIORef' buf (p :)

-- | Run a callback's body, recording any exception rather than letting it
-- into C, and returning the given value then.
sheltered :: IORef [Pending] -> a -> IO a -> IO a
sheltered buf dflt act = act `catch` \e -> dflt <$ push buf (Failed e)

-- | A percentage the header gives as -1 when absent.
percentOf :: Int8 -> Maybe Word8
percentOf n = fromIntegral n <$ guard (n >= 0)

-- | @RENDER_HOLD@'s Haskell half, called by the C callback after it
-- captured: a capture that failed, then the event.
onRenderHold :: IORef [Pending] -> Ptr GveffUserdata -> GhosttyTerminal -> Ptr () -> CBool -> IO ()
onRenderHold buf ud _ _ held = sheltered buf () $ do
  failure <- gveffCaptureFailure ud
  case resultCode failure of
    ResultSuccess -> pass
    other -> push buf (Failed (toException (resultError (CallSite "ghostty_render_state_update") other)))
  push buf (Ready (RenderHold (toBool held)))

-- | @WRITE_PTY@: copy the reply out.
onWritePty :: IORef [Pending] -> GhosttyTerminal -> Ptr () -> Ptr Word8 -> CSize -> IO ()
onWritePty buf _ _ p n = sheltered buf () $ do
  b <- BS.packCStringLen (castPtr p, fromIntegral n)
  push buf (Ready (WritePty b))

-- | @SIZE@: the grid and cell size, if size queries are answered.
onSizeAnswer :: IORef [Pending] -> IORef Answers -> GhosttyTerminal -> Ptr () -> Ptr GhosttySizeReportSize -> IO CBool
onSizeAnswer buf answers _ _ out = sheltered buf 0 $ do
  a <- readIORef answers
  if not a.sizeQueries
    then pure 0
    else do
      poke
        out
        GhosttySizeReportSize
          { rows = fromExtent a.grid.rows,
            columns = fromExtent a.grid.cols,
            cellWidth = a.pixels.width,
            cellHeight = a.pixels.height
          }
      pure 1

-- | @COLOR_SCHEME@: the configured scheme, if any.
onSchemeAnswer :: IORef [Pending] -> IORef Answers -> GhosttyTerminal -> Ptr () -> Ptr GhosttyColorScheme -> IO CBool
onSchemeAnswer buf answers _ _ out = sheltered buf 0 $ do
  a <- readIORef answers
  case a.scheme of
    Nothing -> pure 0
    Just scheme -> 1 <$ poke out (colorSchemeCode scheme)

-- | @DEVICE_ATTRIBUTES@: the configured attributes, always.
onAttributesAnswer :: IORef [Pending] -> IORef Answers -> GhosttyTerminal -> Ptr () -> Ptr GhosttyDeviceAttributes -> IO CBool
onAttributesAnswer buf answers _ _ out = sheltered buf 0 $ do
  a <- readIORef answers
  let d = a.attributes
      fs = take 64 d.features
  poke
    out
    GhosttyDeviceAttributes
      { primary = GhosttyDeviceAttributesPrimary {conformanceLevel = d.conformanceLevel, features = fs, numFeatures = fromIntegral (length fs)},
        secondary = GhosttyDeviceAttributesSecondary {deviceType = d.deviceType, firmwareVersion = d.firmwareVersion, romCartridge = d.romCartridge},
        tertiary = GhosttyDeviceAttributesTertiary {unitId = d.unitId}
      }
  pure 1

-- | @CLIPBOARD_WRITE@: accepted, recorded and answered with success;
-- refused, answered with a denial. The reply is the one call a callback
-- makes, because the header requires it before the callback returns. The
-- request is a frozen struct (it has no @size@, and never grows), so it is
-- read whole; the reply is sized.
onClipboardWrite :: IORef [Pending] -> IORef Answers -> GhosttyTerminal -> Ptr () -> Ptr GhosttyClipboardWrite -> IO ()
onClipboardWrite buf answers _ _ wp = sheltered buf () $ do
  a <- readIORef answers
  w <- peek wp
  location <- case w.location of
    GhosttyClipboardLocationStandard -> pure (Just StandardClipboard)
    GhosttyClipboardLocationSelection -> pure (Just SelectionClipboard)
    GhosttyClipboardLocationPrimary -> pure (Just PrimaryClipboard)
    GhosttyClipboardLocationUnknown n -> Nothing <$ push buf (Unnamed clipboardSite ghosttyClipboardLocationEnum n)
  ok <- case location of
    Just loc | a.clipboard -> do
      cs <- peekArray (fromIntegral w.contentsLen) w.contents
      contents <- forM cs $ \c -> ClipboardContent <$> stringText c.mime <*> ghosttyStringBytes c.data_
      True <$ push buf (Ready (ClipboardWrite loc contents))
    _ -> pure False
  let reply =
        GhosttyClipboardWriteReply
          { size = fromIntegral (sizeOf reply),
            result = if ok then GhosttyClipboardWriteResultSuccess else GhosttyClipboardWriteResultDenied,
            remember = 0
          }
  with reply (callGhosttyClipboardWriteReplyFn w.reply wp)

-- | Where a clipboard write's errors come from.
clipboardSite :: CallSite
clipboardSite = CallSite "GhosttyTerminalClipboardWriteFn"

-- | @DESKTOP_NOTIFICATION@.
onNotification :: IORef [Pending] -> GhosttyTerminal -> Ptr () -> Ptr GhosttyTerminalDesktopNotification -> IO ()
onNotification buf _ _ np = sheltered buf () $ do
  n <- peek np
  e <- Notification <$> stringText n.title <*> stringText n.body
  push buf (Ready e)

-- | @PROGRESS_REPORT@.
onProgress :: IORef [Pending] -> GhosttyTerminal -> Ptr () -> Ptr GhosttyTerminalProgressReport -> IO ()
onProgress buf _ _ rp = sheltered buf () $ do
  r <- peek rp
  let ready st = push buf (Ready (Progress ProgressReport {state = st, percent = percentOf r.progress}))
  case r.state of
    GhosttyTerminalProgressStateRemove -> ready ProgressRemove
    GhosttyTerminalProgressStateSet -> ready ProgressSet
    GhosttyTerminalProgressStateError -> ready ProgressError
    GhosttyTerminalProgressStateIndeterminate -> ready ProgressIndeterminate
    GhosttyTerminalProgressStatePause -> ready ProgressPause
    GhosttyTerminalProgressStateUnknown n -> push buf (Unnamed (CallSite "GhosttyTerminalProgressReportFn") ghosttyTerminalProgressStateEnum n)

-- | @PROGRAM_STATUS@.
onProgramStatus :: IORef [Pending] -> GhosttyTerminal -> Ptr () -> Ptr GhosttyTerminalProgramStatus -> IO ()
onProgramStatus buf _ _ rp = sheltered buf () $ do
  r <- peek rp
  let state = case r.state of
        GhosttyProgramStatusStateIdle -> Right StatusIdle
        GhosttyProgramStatusStateWorking -> Right StatusWorking
        GhosttyProgramStatusStateDone -> Right StatusDone
        GhosttyProgramStatusStateBlocked -> Right StatusBlocked
        GhosttyProgramStatusStateError -> Right StatusError
        GhosttyProgramStatusStateClear -> Right StatusClear
        GhosttyProgramStatusStateUnknown n -> Left (ghosttyProgramStatusStateEnum, n)
      need = case r.kind of
        GhosttyProgramStatusKindNone -> Right Nothing
        GhosttyProgramStatusKindPermission -> Right (Just NeedPermission)
        GhosttyProgramStatusKindQuestion -> Right (Just NeedQuestion)
        GhosttyProgramStatusKindAuth -> Right (Just NeedAuth)
        GhosttyProgramStatusKindUnknown n -> Left (ghosttyProgramStatusKindEnum, n)
  case (,) <$> state <*> need of
    Left (e, n) -> push buf (Unnamed (CallSite "GhosttyTerminalProgramStatusFn") e n)
    Right (st, nd) -> do
      report <-
        ProgramReport st nd (percentOf r.progress)
          <$> stringText r.id
          <*> stringText r.app
          <*> stringText r.title
          <*> stringText r.message
      push buf (Ready (ProgramStatus report))

-- | @SEMANTIC_PROMPT@: the mark now, the cursor after the write.
onSemanticPrompt :: IORef [Pending] -> GhosttyTerminal -> Ptr () -> Ptr GhosttyTerminalSemanticPrompt -> IO ()
onSemanticPrompt buf _ _ ep = sheltered buf () $ do
  e <- peek ep
  case e.kind of
    GhosttySemanticPromptPromptStart -> case e.promptKind of
      GhosttySemanticPromptPromptPrimary -> mark (PromptStart PrimaryPrompt)
      GhosttySemanticPromptPromptRight -> mark (PromptStart RightPrompt)
      GhosttySemanticPromptPromptContinuation -> mark (PromptStart ContinuationPrompt)
      GhosttySemanticPromptPromptSecondary -> mark (PromptStart SecondaryPrompt)
      GhosttySemanticPromptPromptKindUnknown n -> push buf (Unnamed site ghosttySemanticPromptPromptKindEnum n)
    GhosttySemanticPromptInputStart -> mark InputStart
    GhosttySemanticPromptOutputStart -> mark . OutputStart =<< stringText e.command
    GhosttySemanticPromptCommandEnd -> do
      err <- stringText e.error
      mark (CommandEnd (e.exitCode <$ guard (toBool e.hasExitCode)) err)
    -- The header says INVALID is never reported, so it is no more named
    -- than an unknown value.
    GhosttySemanticPromptInvalid -> push buf (Unnamed site ghosttySemanticPromptKindEnum 0)
    GhosttySemanticPromptKindUnknown n -> push buf (Unnamed site ghosttySemanticPromptKindEnum n)
  where
    site = CallSite "GhosttyTerminalSemanticPromptFn"
    mark = push buf . PromptPending

-- | @UNKNOWN_SEQUENCE@.
onUnknownSequence :: IORef [Pending] -> GhosttyTerminal -> Ptr () -> Ptr GhosttyTerminalUnknownSequence -> IO ()
onUnknownSequence buf _ _ sp = sheltered buf () $ do
  peek sp >>= \case
    UnknownSequenceApc s -> do
      b <- ghosttyStringBytes s.content
      ready Apc (toBool s.truncated) b
    UnknownSequenceOsc s -> case oscTerminator s.terminator of
      Right term -> osc term s
      Left n -> push buf (Unnamed site ghosttyOscTerminatorEnum n)
    UnknownSequenceOther (GhosttyTerminalUnknownSequenceTag n) -> push buf (Unnamed site ghosttyTerminalUnknownSequenceTagEnum n)
  where
    site = CallSite "GhosttyTerminalUnknownSequenceFn"
    ready k t b = push buf (Ready (UnknownSequence UnsupportedSequence {kind = k, truncated = t, content = b}))
    osc term s = ghosttyStringBytes s.content >>= ready (Osc term) (toBool s.truncated)

-- | @RESIZE_WINDOW@.
onResizeWindow :: IORef [Pending] -> GhosttyTerminal -> Ptr () -> Ptr GhosttyTerminalResizeWindow -> IO ()
onResizeWindow buf _ _ rp = sheltered buf () $ do
  peek rp >>= \case
    ResizeWindowGrid g -> push buf (Ready (ResizeRequest WindowSize {cols = extent g.columns, rows = extent g.rows}))
    ResizeWindowOther (GhosttyTerminalResizeWindowTag n) ->
      push buf (Unnamed (CallSite "GhosttyTerminalResizeWindowFn") ghosttyTerminalResizeWindowTagEnum n)
