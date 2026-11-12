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

import Control.Exception (onException)
import Data.ByteString.Unsafe qualified as BSU
import Foreign.Ptr (castPtr)
import Ghostty.Effectful.Error (CallSite (..))
import Ghostty.Effectful.Event (TerminalEvent (..))
import Ghostty.Effectful.Internal.Callbacks (Answers (..), takeEvents)
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

-- | The events buffered since the last drain, oldest first, adjacent
-- 'WritePty's joined; throws the first failure a callback recorded.
drainEvents :: Terminal s -> IO [TerminalEvent]
drainEvents t = takeEvents t.installed

-- | Write VT to the terminal: one @ghostty_terminal_vt_write@, and one
-- take. Every callback reads what it needs inside itself (a prompt mark's
-- cursor, a new title or working directory), and each prompt mark records
-- its place among the Haskell callbacks' entries, so the write need not
-- stop at them (DESIGN.md §4.5, D20), and the take puts the whole write's
-- events in order as it builds them (§12.3). Events buffered before the
-- write come first, and adjacent 'WritePty's are joined. Every byte is
-- written even if a callback failed (an exception, an unnamed enum value):
-- the first failure is thrown after the write.
feedTerminal :: Terminal s -> ByteString -> IO [TerminalEvent]
feedTerminal t bytes = do
  wrote t
  BSU.unsafeUseAsCStringLen bytes $ \(p, n) -> ghosttyTerminalVtWrite t.handle (castPtr p) (fromIntegral n)
  drainEvents t
