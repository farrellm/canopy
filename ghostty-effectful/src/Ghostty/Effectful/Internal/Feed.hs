{-# LANGUAGE PatternSynonyms #-}

-- | The calls whose C call can run a callback, each returning the events it
-- caused (DESIGN.md §4.5): writes, resizes and resets. Each
-- drains the terminal's buffer after its C call and resolves what the
-- callbacks appended, reading the terminal through
-- "Ghostty.Effectful.Internal.Keys". (They are here, not in
-- "Ghostty.Effectful.Internal.Terminal", because the keys need the terminal's
-- representation.)
module Ghostty.Effectful.Internal.Feed
  ( feedTerminal,
    resizeTerminal,
    resetTerminal,
    drainEvents,
  )
where

import Control.Exception (catch, onException, throwIO)
import Data.ByteString.Unsafe qualified as BSU
import Foreign.C.Types (CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek)
import Ghostty.Effectful.Error (CallSite (..))
import Ghostty.Effectful.Event (TerminalEvent (..))
import Ghostty.Effectful.Internal.Callbacks (Answers (..), Pending (..), drain, joinWritesNewestFirst, takeMarks, writeVt)
import Ghostty.Effectful.Internal.Convert (unknownEnum)
import Ghostty.Effectful.Internal.Error (expectSuccess)
import Ghostty.Effectful.Internal.Keys (getData)
import Ghostty.Effectful.Internal.Size (CellPixels (..), GridSize (..), fromExtent)
import Ghostty.Effectful.Internal.Terminal (Terminal (..), wrote)
import Ghostty.Effectful.Terminal.Keys (TerminalData (..))
import Ghostty.FFI.Terminal (ghosttyTerminalReset, ghosttyTerminalResize)
import Ghostty.FFI.Types (pattern GhosttySuccess)

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
drainEvents t = joinWritesNewestFirst <$> (resolveOnto t [] =<< drained t)

-- | Turn drained entries into events, newest first onto the given ones,
-- reading what they need from the terminal, which is between writes now.
-- Throws what a callback recorded. (Here, not in
-- "Ghostty.Effectful.Internal.Callbacks", so it calls the reads directly:
-- passed in, each would be a closure built for every stop with entries.)
resolveOnto :: Terminal s -> [TerminalEvent] -> [Pending] -> IO [TerminalEvent]
resolveOnto t = foldlM (\acc p -> (: acc) <$> event p)
  where
    event = \case
      Ready e -> pure e
      TitlePending -> TitleChanged <$> getData t.handle Title
      PwdPending -> PwdChanged <$> getData t.handle Pwd
      Unnamed site e n -> unknownEnum site e n
      Failed e -> throwIO e

-- | What the callbacks appended since the last drain, oldest first: the
-- Haskell callbacks' buffer, with the C callback's prompt marks around it in
-- the stream's order ('takeMarks').
drained :: Terminal s -> IO [Pending]
drained t = takeMarks t.installed =<< drain t.events

-- | Write VT to the terminal. C writes it (@gveff_write@, D19), stopping
-- after each OSC and reading the cursor right after each prompt mark, and
-- returns here when a Haskell callback appended (whose entries may need
-- reading at that stop: a title, a working directory), or the marks fill a
-- batch, or the input ends; each return resolves the events so far
-- (DESIGN.md §4.5). Events buffered before the write come first, and
-- adjacent 'WritePty's are joined across returns. Every byte is written even
-- if resolving throws (a callback's exception, an unnamed enum value): the
-- first such exception is thrown after the whole write, so the terminal
-- never stops part-way through the input.
feedTerminal :: Terminal s -> ByteString -> IO [TerminalEvent]
feedTerminal t bytes = do
  wrote t
  before <- resolved (Resolved Nothing [])
  Resolved failure events <-
    BSU.unsafeUseAsCStringLen bytes $ \(p, n) ->
      alloca $ \used -> go used (castPtr p) n before
  whenJust failure throwIO
  pure $! joinWritesNewestFirst events
  where
    go :: Ptr CSize -> Ptr Word8 -> Int -> Resolved -> IO Resolved
    go used p n acc
      | n <= 0 = pure acc
      | otherwise = do
          r <- writeVt t.handle t.installed p n used
          acc' <- resolved acc
          -- Success tested here, so a write that succeeds boxes no result
          -- for 'expectSuccess'.
          unless (r == GhosttySuccess) $
            expectSuccess (CallSite "ghostty_terminal_vt_write_until") r
          k <- fromIntegral <$> peek used
          go used (p `plusPtr` k) (n - k) acc'

    -- Resolve what the callbacks buffered onto the events so far; if that
    -- throws, the first exception is kept and the drained entries dropped.
    -- Nothing drained, as after most stops, skips the resolve and its
    -- 'catch', which would allocate their closures for nothing. A 'catch'
    -- returning the 'Resolved' built, not 'try' and a map, allocates no
    -- 'Either' and leaves no thunk to force.
    resolved :: Resolved -> IO Resolved
    resolved r@(Resolved failure events) =
      drained t >>= \case
        [] -> pure r
        ps ->
          (Resolved failure <$!> resolveOnto t events ps)
            `catch` \e -> pure (Resolved (failure <|> Just e) events)

-- | A write's progress: the first exception resolving threw, and the events
-- resolved, newest first.
data Resolved = Resolved !(Maybe SomeException) ![TerminalEvent]
