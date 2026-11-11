-- | The calls whose C call can run a callback, each returning the events it
-- caused (DESIGN.md §4.5): writes, resizes and resets. Each drains what the
-- callbacks appended after its C call, and turns it into events. (They are
-- here, not in "Ghostty.Effectful.Internal.Terminal", because they need the
-- terminal's representation and the callbacks' buffer together.)
module Ghostty.Effectful.Internal.Feed
  ( feedTerminal,
    resizeTerminal,
    resetTerminal,
    drainEvents,
  )
where

import Control.Exception (catch, onException, throwIO)
import Data.ByteString.Unsafe qualified as BSU
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Ghostty.Effectful.Error (CallSite (..))
import Ghostty.Effectful.Event (TerminalEvent (..))
import Ghostty.Effectful.Internal.Callbacks (Answers (..), Pending (..), joinWritesNewestFirst, takePending)
import Ghostty.Effectful.Internal.Convert (unknownEnum)
import Ghostty.Effectful.Internal.Error (expectSuccess)
import Ghostty.Effectful.Internal.Size (CellPixels (..), GridSize (..), fromExtent)
import Ghostty.Effectful.Internal.Terminal (Terminal (..), wrote)
import Ghostty.FFI.Terminal (ghosttyTerminalReset, ghosttyTerminalResize, ghosttyTerminalVtWrite)

-- | @ghostty_terminal_resize@, and the grid the size callback reports with
-- it. Its one documented refusal, a zero extent or a null terminal, cannot
-- be expressed, so only running out of memory remains. Returns the events
-- it caused: the end of a render hold, and an in-band size report (mode
-- 2048).
resizeTerminal :: Terminal s -> GridSize -> CellPixels -> IO [TerminalEvent]
resizeTerminal t g px = do
  -- The size callback runs inside the resize, so the answers change first,
  -- and change back if the resize is refused.
  wrote t
  old <- readIORef t.answers
  writeIORef t.answers old {grid = g, pixels = px}
  ( expectSuccess (CallSite "ghostty_terminal_resize")
      =<< ghosttyTerminalResize t.handle (fromExtent g.cols) (fromExtent g.rows) px.width px.height
    )
    `onException` writeIORef t.answers old
  drainEvents t

-- | @ghostty_terminal_reset@: a full reset (RIS), keeping the size. Returns
-- the events it caused: the end of a render hold. (The 'Reset' event is for
-- a reset the program asks for.)
resetTerminal :: Terminal s -> IO [TerminalEvent]
resetTerminal t = do
  wrote t
  ghosttyTerminalReset t.handle
  drainEvents t

-- | The events buffered since the last drain, resolved.
drainEvents :: Terminal s -> IO [TerminalEvent]
drainEvents t = joinWritesNewestFirst <$> (resolveOnto [] =<< drained t)

-- | Turn drained entries into events, newest first onto the given ones.
-- Throws what a callback recorded.
resolveOnto :: [TerminalEvent] -> [Pending] -> IO [TerminalEvent]
resolveOnto = foldlM (\acc p -> (: acc) <$> event p)
  where
    event = \case
      Ready e -> pure e
      Unnamed site e n -> unknownEnum site e n
      Failed e -> throwIO e

-- | What the callbacks appended since the last drain, oldest first, prompt
-- marks among the rest in the stream's order ('takePending').
drained :: Terminal s -> IO [Pending]
drained t = takePending t.events t.installed

-- | Write VT to the terminal: one @ghostty_terminal_vt_write@. Every
-- callback reads what it needs inside itself (a prompt mark's cursor, a new
-- title or working directory), and each prompt mark records its place among
-- the Haskell callbacks' entries, so the write need not stop at them
-- (DESIGN.md §4.5, D20). It is written in slices of 'sliceBytes', each
-- resolved before the next, so what one write holds unresolved in C and in
-- the buffer stays bounded; a split anywhere gives the same events. Events
-- buffered before the write come first, and adjacent 'WritePty's are
-- joined. Every byte is written even if a callback failed (an exception, an
-- unnamed enum value): the first failure is thrown after the write.
feedTerminal :: Terminal s -> ByteString -> IO [TerminalEvent]
feedTerminal t bytes = do
  wrote t
  before <- resolved (Resolved Nothing [])
  Resolved failure events <-
    BSU.unsafeUseAsCStringLen bytes $ \(p, n) ->
      -- One slice, as for nearly every PTY read: no loop to build.
      if n <= sliceBytes
        then ghosttyTerminalVtWrite t.handle (castPtr p) (fromIntegral n) >> resolved before
        else go (castPtr p) n before
  whenJust failure throwIO
  pure $! joinWritesNewestFirst events
  where
    go :: Ptr Word8 -> Int -> Resolved -> IO Resolved
    go p n acc
      | n <= 0 = pure acc
      | otherwise = do
          let k = min n sliceBytes
          ghosttyTerminalVtWrite t.handle p (fromIntegral k)
          acc' <- resolved acc
          go (p `plusPtr` k) (n - k) acc'

    -- Resolve what the callbacks buffered onto the events so far; if that
    -- throws, the first exception is kept and the drained entries dropped.
    -- Nothing drained, as before most writes, skips the resolve and its
    -- 'catch', which would allocate their closures for nothing. A 'catch'
    -- returning the 'Resolved' built, not 'try' and a map, allocates no
    -- 'Either' and leaves no thunk to force.
    resolved :: Resolved -> IO Resolved
    resolved r@(Resolved failure events) =
      drained t >>= \case
        [] -> pure r
        ps ->
          (Resolved failure <$!> resolveOnto events ps)
            `catch` \e -> pure (Resolved (failure <|> Just e) events)

-- | How much of a write goes to the library at once: a PTY read is usually
-- less, so most writes are one slice.
sliceBytes :: Int
sliceBytes = 65536

-- | A write's progress: the first exception resolving threw, and the events
-- resolved, newest first.
data Resolved = Resolved !(Maybe SomeException) ![TerminalEvent]
