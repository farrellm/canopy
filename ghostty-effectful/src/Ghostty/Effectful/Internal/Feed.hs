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

import Control.Exception (onException, throwIO, try)
import Data.ByteString.Unsafe qualified as BSU
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..), peek)
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), ResultCode (..))
import Ghostty.Effectful.Event (TerminalEvent)
import Ghostty.Effectful.Internal.Callbacks (Answers (..), drain, joinWrites, resolve)
import Ghostty.Effectful.Internal.Error (expectSuccess, resultCode)
import Ghostty.Effectful.Internal.Keys (getData)
import Ghostty.Effectful.Internal.Size (CellPixels (..), GridSize (..), fromExtent)
import Ghostty.Effectful.Internal.Terminal (Terminal (..), wrote)
import Ghostty.FFI.Terminal
  ( GhosttyTerminalBoundary (..),
    GhosttyTerminalStringEnd (..),
    GhosttyTerminalStringIntroducer (..),
    GhosttyTerminalStringKind (..),
    GhosttyTerminalWriteStop (..),
    ghosttyTerminalReset,
    ghosttyTerminalResize,
    ghosttyTerminalVtWriteUntil,
    pattern GhosttyTerminalBoundaryOscEnd,
  )

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
drainEvents t = resolve (getData t.handle) =<< drain t.events

-- | Write VT to the terminal, stopping after each OSC
-- (@GHOSTTY_TERMINAL_BOUNDARY_OSC_END@) to resolve the events so far, so a
-- prompt mark's cursor is read right after the mark (DESIGN.md §4.5).
-- Events buffered before the write come first, and adjacent 'WritePty's are
-- joined across stops. Every byte is written even if resolving throws (a
-- callback's exception, an unnamed enum value): the first such exception is
-- thrown after the whole write, so the terminal never stops part-way through
-- the input.
feedTerminal :: Terminal s -> ByteString -> IO [TerminalEvent]
feedTerminal t bytes = do
  wrote t
  before <- try (drainEvents t)
  (failure, after) <- BSU.unsafeUseAsCStringLen bytes $ \(p, n) -> alloca $ \stop -> go stop (castPtr p) n (leftToMaybe before) []
  whenJust failure throwIO
  pure (joinWrites (fromRight [] before <> after))
  where
    go :: Ptr GhosttyTerminalWriteStop -> Ptr Word8 -> Int -> Maybe SomeException -> [[TerminalEvent]] -> IO (Maybe SomeException, [TerminalEvent])
    go stop p n failure acc
      | n <= 0 = pure (failure, concat (reverse acc))
      | otherwise = do
          poke stop emptyStop
          r <- ghosttyTerminalVtWriteUntil t.handle p (fromIntegral n) boundary stop
          resolved <- try (drainEvents t)
          let failure' = failure <|> leftToMaybe resolved
              acc' = fromRight [] resolved : acc
          case resultCode r of
            ResultNoValue -> pure (failure', concat (reverse acc'))
            ResultSuccess -> do
              used <- fromIntegral . (.consumed) <$> peek stop
              go stop (p `plusPtr` used) (n - used) failure' acc'
            other -> throwIO (Invariant (CallSite "ghostty_terminal_vt_write_until") other)

    boundary = let GhosttyTerminalBoundary b = GhosttyTerminalBoundaryOscEnd in fromIntegral b

    -- GHOSTTY_INIT_SIZED: the size, and zeros. ('sizeOf' ignores its
    -- argument.)
    emptyStop :: GhosttyTerminalWriteStop
    emptyStop =
      GhosttyTerminalWriteStop
        { size = fromIntegral (sizeOf emptyStop),
          consumed = 0,
          reached = 0,
          kind = GhosttyTerminalStringKind 0,
          introducer = GhosttyTerminalStringIntroducer 0,
          end = GhosttyTerminalStringEnd 0,
          byte = 0
        }
