{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The callbacks the binding installs on every terminal (DESIGN.md §4.5),
-- and the state they answer from.
--
-- A callback runs inside a VT write, so each does one thing: append to the
-- terminal's buffer, or answer a query from the terminal's 'Answers'. Each is
-- 'sheltered': an exception inside one is recorded, not let into C, and thrown
-- by whoever drains the buffer. What needs the terminal (the title after a
-- title change, the working directory after its change, a prompt mark's
-- cursor) is read inside the callback, as the header asks; only writing to
-- the terminal from one is forbidden (DESIGN.md §4.5, D20). A C enum value
-- the binding does not name is recorded as an 'Unnamed' entry, and becomes
-- 'UnknownEnum' when drained. Every append goes through 'push', which also counts
-- it, so a prompt mark C copies knows its place.
--
-- @ENQUIRY@ and @XTVERSION@ return a struct by value, which no Haskell
-- function can, so they are C (@cbits/gveff_terminal.c@), answering from the
-- terminal's @USERDATA@ (a 'GveffUserdata'), whose two strings this module
-- owns. @RENDER_HOLD@ is C too: when a hold begins it captures the frame into
-- every render state attached to the terminal, which is a C call the header
-- asks for inside the callback (DESIGN.md §4.3), and then calls the Haskell
-- callback that appends the event. @SEMANTIC_PROMPT@ is C as well, so that a
-- mark runs no Haskell (DESIGN.md §4.5, D18): it copies the mark into the
-- @USERDATA@, and 'takeEvents' takes the copies after the write.
module Ghostty.Effectful.Internal.Callbacks
  ( -- * Pending events
    Pending (..),
    takeEvents,
    pushPending,
    markFailed,

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

import Control.Exception (catch, handle, mask_, onException, throwIO)
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Vector qualified as V
import Foreign.C.Types (CBool (..), CInt (..), CSize (..))
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (copyBytes, fromBool, toBool, with)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castFunPtrToPtr, castPtr, freeHaskellFunPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..))
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), ResultCode (..))
import Ghostty.Effectful.Event
import Ghostty.Effectful.Internal.Convert (colorSchemeCode, emptyString, getSite, oscTerminator, stringText, unknownEnumError)
import Ghostty.Effectful.Internal.Error (expectSuccess, optionalVia, orInvariant, resultCode, resultError)
import Ghostty.Effectful.Internal.Size (CellPixels (..), GridSize (..), extent, fromExtent)
import Ghostty.Effectful.Point (Point (..))
import Ghostty.Effectful.Terminal.Keys (ColorScheme (..), DeviceAttributes (..))
import Ghostty.FFI.Device
import Ghostty.FFI.Layout (CEnum)
import Ghostty.FFI.Osc (ghosttyOscTerminatorEnum)
import Ghostty.FFI.Render (GhosttyRenderState (..))
import Ghostty.FFI.SizeReport (GhosttySizeReportSize (..))
import Ghostty.FFI.Terminal
  ( GhosttyTerminal (..),
    GhosttyTerminalData (..),
    GhosttyTerminalOption,
    ghosttyTerminalGet,
    ghosttyTerminalSet,
    pattern GhosttyTerminalDataInvalid,
    pattern GhosttyTerminalDataPwd,
    pattern GhosttyTerminalDataTitle,
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

-- | What a callback appended: an event, or a failure to throw when drained.
data Pending
  = -- | an event as it is
    Ready TerminalEvent
  | -- | a value of the described enum that the binding does not name, from
    -- the named callback
    Unnamed CallSite CEnum CInt
  | -- | an exception inside a callback, which C must not see
    Failed SomeException

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
    userdata :: Ptr GveffUserdata,
    -- | where the callbacks append, which 'takeEvents' takes and empties
    sink :: Sink
  }

-- | A terminal's @USERDATA@, @GveffUserdata@ in @include/gveff_terminal.h@:
-- ENQ's reply and XTVERSION's name, first, as an array of two strings; then
-- the render states that a hold captures into, and the prompt marks.
data GveffUserdata

-- | A prompt mark C copied, @GveffMark@ in @include/gveff_terminal.h@: the
-- event, how many Haskell entries came before it, and its cursor.
data GveffMark

-- | Install every callback but @CLIPBOARD_READ@ on a terminal, appending to
-- a new buffer and answering from the answers, with ENQ and XTVERSION
-- answering the given bytes. If one cannot be installed, what was made is
-- freed and the error thrown.
installCallbacks :: GhosttyTerminal -> IORef Answers -> ByteString -> ByteString -> IO Installed
installCallbacks h answers enq xtv = do
  events <- newIORef []
  made <- newIORef []
  let register :: IO (FunPtr a) -> IO (FunPtr a)
      register = registerFunPtr made
      wrapped :: IO (FunPtr a) -> GhosttyTerminalOption -> IO ()
      wrapped mk opt = setPtr opt . castFunPtrToPtr =<< register mk
      setPtr opt p = expectSuccess (CallSite "ghostty_terminal_set") =<< ghosttyTerminalSet h opt p
  ud <- gveffUserdataNew
  when (ud == nullPtr) $ throwIO (OutOfMemory (CallSite "gveff_userdata_new"))
  pushed <- gveffPushed ud
  let buf = Sink {entries = events, count = pushed}
  let install = do
        replaceAnswer ud enquiryIndex enq
        replaceAnswer ud xtversionIndex xtv
        setPtr GhosttyTerminalOptUserdata (castPtr ud)
        setPtr GhosttyTerminalOptEnquiry (castFunPtrToPtr gveffEnquiry)
        setPtr GhosttyTerminalOptXtversion (castFunPtrToPtr gveffXtversion)
        wrapped (mkGhosttyTerminalWritePtyFn (onWritePty buf)) GhosttyTerminalOptWritePty
        wrapped (mkGhosttyTerminalBellFn (\_ _ -> push buf (Ready Bell))) GhosttyTerminalOptBell
        wrapped (mkGhosttyTerminalTitleChangedFn (onText buf TitleChanged GhosttyTerminalDataTitle)) GhosttyTerminalOptTitleChanged
        wrapped (mkGhosttyTerminalPwdChangedFn (onText buf PwdChanged GhosttyTerminalDataPwd)) GhosttyTerminalOptPwdChanged
        wrapped (mkGhosttyTerminalSizeFn (onSizeAnswer buf answers)) GhosttyTerminalOptSize
        wrapped (mkGhosttyTerminalColorSchemeFn (onSchemeAnswer buf answers)) GhosttyTerminalOptColorScheme
        wrapped (mkGhosttyTerminalDeviceAttributesFn (onAttributesAnswer buf answers)) GhosttyTerminalOptDeviceAttributes
        wrapped (mkGhosttyTerminalClipboardWriteFn (onClipboardWrite buf answers)) GhosttyTerminalOptClipboardWrite
        wrapped (mkGhosttyTerminalDesktopNotificationFn (onNotification buf)) GhosttyTerminalOptDesktopNotification
        wrapped (mkGhosttyTerminalProgressReportFn (onProgress buf)) GhosttyTerminalOptProgressReport
        wrapped (mkGhosttyTerminalProgramStatusFn (onProgramStatus buf)) GhosttyTerminalOptProgramStatus
        setPtr GhosttyTerminalOptSemanticPrompt (castFunPtrToPtr gveffSemanticPrompt)
        wrapped (mkGhosttyTerminalResetFn (\_ _ -> push buf (Ready Reset))) GhosttyTerminalOptReset
        gveffSetHoldNotify ud =<< register (mkGhosttyTerminalRenderHoldFn (onRenderHold buf ud))
        setPtr GhosttyTerminalOptRenderHold (castFunPtrToPtr gveffRenderHold)
        wrapped (mkGhosttyTerminalUnknownSequenceFn (onUnknownSequence buf)) GhosttyTerminalOptUnknownSequence
        wrapped (mkGhosttyTerminalResizeWindowFn (onResizeWindow buf)) GhosttyTerminalOptResizeWindow
  let installed = (\funPtrs -> Installed {funPtrs, userdata = ud, sink = buf}) <$> readIORef made
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

-- | The prompt-mark callback, in C: it copies the mark onto the
-- @USERDATA@'s marks, which 'takeEvents' takes.
foreign import capi "gveff_terminal.h &gveff_semantic_prompt"
  gveffSemanticPrompt :: GhosttyTerminalSemanticPromptFn

-- | Whether there are marks, or a mark's failure, to take.
foreign import capi unsafe "gveff_terminal.h gveff_marks_pending"
  gveffMarksPending :: Ptr GveffUserdata -> IO CBool

-- | The marks since the last clear.
foreign import capi unsafe "gveff_terminal.h gveff_marks"
  gveffMarks :: Ptr GveffUserdata -> IO (Ptr GveffMark)

-- | How many marks there are.
foreign import capi unsafe "gveff_terminal.h gveff_mark_count"
  gveffMarkCount :: Ptr GveffUserdata -> IO CSize

-- | @sizeof(GveffMark)@.
foreign import capi "gveff_terminal.h value GVEFF_MARK_SIZE"
  gveffMarkSize :: CSize

-- | @GveffMark@'s @before@'s offset.
foreign import capi "gveff_terminal.h value GVEFF_MARK_BEFORE"
  gveffMarkBefore :: CSize

-- | @GveffMark@'s @cursor@'s offset.
foreign import capi "gveff_terminal.h value GVEFF_MARK_CURSOR"
  gveffMarkCursor :: CSize

-- | The @USERDATA@'s count that 'push' adds one to.
foreign import capi unsafe "gveff_terminal.h gveff_pushed"
  gveffPushed :: Ptr GveffUserdata -> IO (Ptr CSize)

-- | Free the marks' strings, empty the list, and forget the failure.
foreign import capi unsafe "gveff_terminal.h gveff_marks_clear"
  gveffMarksClear :: Ptr GveffUserdata -> IO ()

-- | The first mark since the clear that could not be copied, or whose
-- cursor could not be read, or success. No mark is taken after it.
foreign import capi unsafe "gveff_terminal.h gveff_marks_failure"
  gveffMarksFailure :: Ptr GveffUserdata -> IO GhosttyResult

-- | The key whose read failed, or @INVALID@ for a copy.
foreign import capi unsafe "gveff_terminal.h gveff_marks_failure_key"
  gveffMarksFailureKey :: Ptr GveffUserdata -> IO GhosttyTerminalData

-- | How many Haskell entries came before the failure.
foreign import capi unsafe "gveff_terminal.h gveff_marks_failure_before"
  gveffMarksFailureBefore :: Ptr GveffUserdata -> IO CSize

-- | Record a mark's failure here in the stream, unless one is recorded.
foreign import capi unsafe "gveff_terminal.h gveff_mark_failed"
  gveffMarkFailed :: Ptr GveffUserdata -> GhosttyResult -> GhosttyTerminalData -> IO ()

-- | A zeroed @USERDATA@, or null. @unsafe@, as is every call below: none
-- calls back into Haskell, and each is a few loads and stores.
foreign import capi unsafe "gveff_terminal.h gveff_userdata_new"
  gveffUserdataNew :: IO (Ptr GveffUserdata)

-- | Free a @USERDATA@, its capture list and its marks, not the answers'
-- bytes.
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

-- | Append to the buffer, and count it, so a prompt mark C copies after
-- this knows its place (DESIGN.md §4.5, D20).
push :: Sink -> Pending -> IO ()
push sink p = do
  modifyIORef' sink.entries (p :)
  n <- peek sink.count
  poke sink.count (n + 1)

-- | Append to the buffer from outside a callback, counted as 'push' counts
-- (for tests, which stand in for a callback).
pushPending :: Installed -> Pending -> IO ()
pushPending i = push i.sink

-- | Record a prompt mark's failure at this point in the stream, as the C
-- callback does when a copy or a cursor read fails (for tests, from inside
-- a callback, since neither can be made to fail).
markFailed :: Installed -> GhosttyResult -> GhosttyTerminalData -> IO ()
markFailed i = gveffMarkFailed i.userdata

-- | Where the callbacks append: the terminal's buffer, and the
-- @USERDATA@'s count of what is in it, which C reads for each prompt mark.
data Sink = Sink
  { -- | what the callbacks appended since the last take, newest first
    entries :: !(IORef [Pending]),
    -- | the @USERDATA@'s @pushed@, the number of entries, which 'push' adds
    -- to and 'takeEvents' resets
    count :: !(Ptr CSize)
  }

-- | @TITLE_CHANGED@ and @PWD_CHANGED@: the new value, read inside the
-- callback, as the header asks (DESIGN.md §4.5, D20).
onText :: Sink -> (Text -> TerminalEvent) -> GhosttyTerminalData -> GhosttyTerminal -> Ptr () -> IO ()
onText buf event key h _ = sheltered buf () $ do
  -- A failed read throws, and 'sheltered' records it. The value is there
  -- inside its own change's callback, so its absence is an 'Invariant', as
  -- for every other required read ('Ghostty.Effectful.Internal.Keys').
  let site = getSite key
  t <- stringText =<< orInvariant site ResultNoValue =<< optionalVia ResultNoValue site (ghosttyTerminalGet h key . castPtr)
  push buf (Ready (event t))

-- | Run a callback's body, recording any exception rather than letting it
-- into C, and returning the given value then.
sheltered :: Sink -> a -> IO a -> IO a
sheltered buf dflt act = act `catch` \e -> dflt <$ push buf (Failed e)

-- | A percentage the header gives as -1 when absent.
percentOf :: Int8 -> Maybe Word8
percentOf n = fromIntegral n <$ guard (n >= 0)

-- | @RENDER_HOLD@'s Haskell half, called by the C callback after it
-- captured: a capture that failed, then the event.
onRenderHold :: Sink -> Ptr GveffUserdata -> GhosttyTerminal -> Ptr () -> CBool -> IO ()
onRenderHold buf ud _ _ held = sheltered buf () $ do
  failure <- gveffCaptureFailure ud
  case resultCode failure of
    ResultSuccess -> pass
    other -> push buf (Failed (toException (resultError (CallSite "ghostty_render_state_update") other)))
  push buf (Ready (RenderHold (toBool held)))

-- | @WRITE_PTY@: copy the reply out.
onWritePty :: Sink -> GhosttyTerminal -> Ptr () -> Ptr Word8 -> CSize -> IO ()
onWritePty buf _ _ p n = sheltered buf () $ do
  b <- BS.packCStringLen (castPtr p, fromIntegral n)
  push buf (Ready (WritePty b))

-- | @SIZE@: the grid and cell size, if size queries are answered.
onSizeAnswer :: Sink -> IORef Answers -> GhosttyTerminal -> Ptr () -> Ptr GhosttySizeReportSize -> IO CBool
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
onSchemeAnswer :: Sink -> IORef Answers -> GhosttyTerminal -> Ptr () -> Ptr GhosttyColorScheme -> IO CBool
onSchemeAnswer buf answers _ _ out = sheltered buf 0 $ do
  a <- readIORef answers
  case a.scheme of
    Nothing -> pure 0
    Just scheme -> 1 <$ poke out (colorSchemeCode scheme)

-- | @DEVICE_ATTRIBUTES@: the configured attributes, always.
onAttributesAnswer :: Sink -> IORef Answers -> GhosttyTerminal -> Ptr () -> Ptr GhosttyDeviceAttributes -> IO CBool
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
onClipboardWrite :: Sink -> IORef Answers -> GhosttyTerminal -> Ptr () -> Ptr GhosttyClipboardWrite -> IO ()
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
onNotification :: Sink -> GhosttyTerminal -> Ptr () -> Ptr GhosttyTerminalDesktopNotification -> IO ()
onNotification buf _ _ np = sheltered buf () $ do
  n <- peek np
  e <- Notification <$> stringText n.title <*> stringText n.body
  push buf (Ready e)

-- | @PROGRESS_REPORT@.
onProgress :: Sink -> GhosttyTerminal -> Ptr () -> Ptr GhosttyTerminalProgressReport -> IO ()
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
onProgramStatus :: Sink -> GhosttyTerminal -> Ptr () -> Ptr GhosttyTerminalProgramStatus -> IO ()
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

-- | What a walk found: its first failure, and its events, oldest first.
data Resolved = Resolved !(Maybe SomeException) ![TerminalEvent]

-- | Take what the callbacks appended since the last take, leaving the
-- buffer and the marks empty: the Haskell callbacks' entries, with the
-- prompt marks C copied put among them where the stream had them, by the
-- count C recorded with each (DESIGN.md §4.5, D20). Each mark carries the
-- cursor C read inside its callback. The events come out oldest first,
-- each run of adjacent 'WritePty's joined into one, so a reply goes to the
-- PTY in one write. One pass, newest first as the buffer is kept, building
-- the one list returned (§12.3).
--
-- A failure (an exception in a callback, an unnamed enum value, a mark
-- that could not be copied or a cursor that could not be read) drops the
-- take's events, and the first in the stream is thrown, after the take has
-- emptied what it took. Nothing appended allocates nothing.
takeEvents :: Installed -> IO [TerminalEvent]
takeEvents i = do
  -- A read then a write, not atomicModifyIORef': only the owner thread
  -- touches the buffer (DESIGN.md §4.6), and the callbacks that append to
  -- it run on that thread inside a C call, never between the two.
  hs <- readIORef buf
  -- 'push' counted every entry, so this is the buffer's length, and the
  -- count the marks' are against.
  j <- fromIntegral <$> peek i.sink.count
  -- One call, allocating nothing, when there are no marks.
  pending <- toBool <$> gveffMarksPending i.userdata
  if not pending
    then case hs of
      [] -> pure []
      _ -> do
        r <- walk nullPtr (-1) j hs [] Nothing []
        emptied
        finish r
    else do
      n <- fromIntegral <$> gveffMarkCount i.userdata
      p <- gveffMarks i.userdata
      failure <- gveffMarksFailure i.userdata
      -- No 'finally' (a 'catch#' and its closures on every take): the walk
      -- throws nothing synchronously, since 'markOnto' catches what its
      -- string reads throw. The marks are cleared before the buffer is
      -- emptied, so an asynchronous exception before the clear leaves both
      -- for the next take, their counts agreeing; one between the two loses
      -- only the marks. Freeing the USERDATA frees what is never taken.
      r <- case resultCode failure of
        ResultSuccess -> walk p (n - 1) j hs [] Nothing []
        c -> lateFailure p n j hs c =<< gveffMarksFailureBefore i.userdata
      gveffMarksClear i.userdata
      emptied
      finish r
  where
    buf = i.sink.entries
    emptied = writeIORef buf [] >> poke i.sink.count 0
    -- A failure drops the events.
    finish = \case
      Resolved Nothing es -> pure es
      Resolved (Just x) _ -> throwIO x
    -- C recorded a mark's failure after @at@ entries, and took no mark
    -- after it: the first failure is an earlier one among the entries and
    -- marks before it, if any, or it. Only those are walked; a failure
    -- drops the events anyway.
    lateFailure p n j hs c at = do
      key <- gveffMarksFailureKey i.userdata
      let k = fromIntegral at
          site
            | key == GhosttyTerminalDataInvalid = CallSite "gveff_semantic_prompt"
            | otherwise = getSite key
          late = toException (resultError site c)
      Resolved own _ <- walk p (n - 1) k (drop (j - k) hs) [] Nothing []
      pure $! Resolved (own <|> Just late) []

-- | Place the entries @hs@ (newest first, @j@ of them) and marks @k@ down
-- to 0 in front of the events after them, @out@, oldest first. Going
-- backwards through the stream, a mark is the newest thing left if C
-- counted @j@ entries or more before it. @run@ is the bytes
-- of the 'WritePty's just placed, oldest first, not yet joined; @own@ the
-- oldest failure met so far.
walk :: Ptr GveffMark -> Int -> Int -> [Pending] -> [ByteString] -> Maybe SomeException -> [TerminalEvent] -> IO Resolved
walk p !k !j hs !run !own !out
  | k >= 0 = do
      let m = p `plusPtr` (k * markSize)
      before <- fromIntegral <$> peekByteOff @CSize m markBeforeAt
      if before >= j
        then do
          c <- peekByteOff @Word32 m markCursorAt
          e <- peek @GhosttyTerminalSemanticPrompt (castPtr m)
          let !after = joined run out
          markOnto c e own after >>= \(Resolved own' out') -> walk p (k - 1) j hs [] own' out'
        else entry
  | otherwise = entry
  where
    entry = case hs of
      [] -> pure $! Resolved own (joined run out)
      h : rest -> case h of
        Ready (WritePty b) -> walk p k (j - 1) rest (b : run) own out
        Ready e -> let !after = joined run out in walk p k (j - 1) rest [] own (e : after)
        Unnamed site en v -> walk p k (j - 1) rest run (Just (toException (unknownEnumError site en v))) out
        Failed x -> walk p k (j - 1) rest run (Just x) out

-- | @GveffMark@'s size, and its fields' offsets, from the header; the event
-- is at 0.
markSize, markBeforeAt, markCursorAt :: Int
markSize = fromIntegral gveffMarkSize
markBeforeAt = fromIntegral gveffMarkBefore
markCursorAt = fromIntegral gveffMarkCursor
{-# NOINLINE markSize #-}
{-# NOINLINE markBeforeAt #-}
{-# NOINLINE markCursorAt #-}

-- | A run of 'WritePty's' bytes, oldest first, as one in front of the
-- events after it.
joined :: [ByteString] -> [TerminalEvent] -> [TerminalEvent]
joined run out = case run of
  [] -> out
  [b] -> WritePty b : out
  _ -> (WritePty $! BS.concat run) : out
{-# INLINE joined #-}

-- | @SEMANTIC_PROMPT@'s event, copied by the C callback, with the cursor C
-- read inside it (the column in the low 16 bits, the row in the high),
-- in front of the events after it, or its failure kept: the take walks
-- backwards, so a failure met later is earlier in the stream. Only reading a string can throw,
-- so an empty one, as a mark's strings nearly always are, is not read, and
-- the mark is built in the loop with nothing else allocated; one with text
-- is 'markRead''s.
markOnto :: Word32 -> GhosttyTerminalSemanticPrompt -> Maybe SomeException -> [TerminalEvent] -> IO Resolved
markOnto cursor e own acc = case e.kind of
  GhosttySemanticPromptPromptStart -> case e.promptKind of
    GhosttySemanticPromptPromptPrimary -> mark (PromptStart PrimaryPrompt)
    GhosttySemanticPromptPromptRight -> mark (PromptStart RightPrompt)
    GhosttySemanticPromptPromptContinuation -> mark (PromptStart ContinuationPrompt)
    GhosttySemanticPromptPromptSecondary -> mark (PromptStart SecondaryPrompt)
    GhosttySemanticPromptPromptKindUnknown n -> unnamed ghosttySemanticPromptPromptKindEnum n
  GhosttySemanticPromptInputStart -> mark InputStart
  GhosttySemanticPromptOutputStart
    | e.command.len == 0 -> mark (OutputStart mempty)
  GhosttySemanticPromptCommandEnd
    | e.error.len == 0 -> mark (commandEnd e)
  GhosttySemanticPromptOutputStart -> markRead cursor e own acc
  GhosttySemanticPromptCommandEnd -> markRead cursor e own acc
  -- The header says INVALID is never reported, so it is no more named
  -- than an unknown value.
  GhosttySemanticPromptInvalid -> unnamed ghosttySemanticPromptKindEnum 0
  GhosttySemanticPromptKindUnknown n -> unnamed ghosttySemanticPromptKindEnum n
  where
    -- The event is built now, not left a thunk: the benchmark's 'length'
    -- would never force one, but every caller does.
    mark step = let !ev = SemanticPrompt (promptMark cursor step) in pure $! Resolved own (ev : acc)
    unnamed en v = pure $! Resolved (Just (toException (unknownEnumError (CallSite "GhosttyTerminalSemanticPromptFn") en v))) acc
{-# INLINE markOnto #-}

-- | A mark whose string is not empty: read it, catching what the read
-- throws.
markRead :: Word32 -> GhosttyTerminalSemanticPrompt -> Maybe SomeException -> [TerminalEvent] -> IO Resolved
markRead cursor e own acc =
  handle (\x -> pure $! Resolved (Just x) acc) $ do
    step <- case e.kind of
      GhosttySemanticPromptCommandEnd -> CommandEnd (exitCodeOf e) <$> stringText e.error
      _ -> OutputStart <$> stringText e.command
    pure $! Resolved own (SemanticPrompt (promptMark cursor step) : acc)
{-# NOINLINE markRead #-}

-- | A mark at the cursor C packed: the column in the low 16 bits, the row
-- in the high.
promptMark :: Word32 -> PromptStep -> PromptMark
promptMark cursor step = PromptMark {step, cursor = Point {col = fromIntegral cursor, row = cursor `shiftR` 16}}
{-# INLINE promptMark #-}

-- | @D@'s exit code, if the shell reported one.
exitCodeOf :: GhosttyTerminalSemanticPrompt -> Maybe Int32
exitCodeOf e = e.exitCode <$ guard (toBool e.hasExitCode)

-- | @D@ with no error text, as nearly every one is: shared, built once,
-- for no exit code or one from 0 to 255 (every code a Unix shell reports),
-- so such a mark allocates no step of its own (DESIGN.md §12.3).
commandEnd :: GhosttyTerminalSemanticPrompt -> PromptStep
commandEnd e
  | not (toBool e.hasExitCode) = commandEndNoCode
  | code >= 0 && code < 256 = commandEnds `V.unsafeIndex` fromIntegral code
  | otherwise = CommandEnd (Just code) mempty
  where
    code = e.exitCode
{-# INLINE commandEnd #-}

-- | @D@ with no exit code and no error text.
commandEndNoCode :: PromptStep
commandEndNoCode = CommandEnd Nothing mempty
{-# NOINLINE commandEndNoCode #-}

-- | @D@ with each exit code from 0 to 255 and no error text.
commandEnds :: V.Vector PromptStep
commandEnds = V.generate 256 (\n -> CommandEnd (Just (fromIntegral n)) mempty)
{-# NOINLINE commandEnds #-}

-- | @UNKNOWN_SEQUENCE@.
onUnknownSequence :: Sink -> GhosttyTerminal -> Ptr () -> Ptr GhosttyTerminalUnknownSequence -> IO ()
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
onResizeWindow :: Sink -> GhosttyTerminal -> Ptr () -> Ptr GhosttyTerminalResizeWindow -> IO ()
onResizeWindow buf _ _ rp = sheltered buf () $ do
  peek rp >>= \case
    ResizeWindowGrid g -> push buf (Ready (ResizeRequest WindowSize {cols = extent g.columns, rows = extent g.rows}))
    ResizeWindowOther (GhosttyTerminalResizeWindowTag n) ->
      push buf (Unnamed (CallSite "GhosttyTerminalResizeWindowFn") ghosttyTerminalResizeWindowTagEnum n)
