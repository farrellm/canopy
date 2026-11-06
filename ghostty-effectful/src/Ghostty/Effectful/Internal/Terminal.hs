-- | A terminal's representation and its raw calls (DESIGN.md §4.2). The
-- public API is "Ghostty.Effectful.Terminal", which exports these types
-- without their constructors; the calls here are what the 'Ghostty'
-- interpreter runs, after its owner-thread check.
--
-- A terminal owns the tracked grid references made from it, in a registry
-- keyed by number. A 'TrackedRef' names its entry, not the C handle, so a
-- reference that was untracked, or whose terminal was freed, is never
-- passed to the library again: it finds no entry.
module Ghostty.Effectful.Internal.Terminal
  ( -- * Handles
    Terminal (..),
    TrackedRef (..),

    -- * Sizes
    Extent (..),
    extent,
    fromExtent,
    GridSize (..),
    CellPixels (..),
    noCellPixels,

    -- * Configuration
    TerminalConfig (..),
    terminalConfig,

    -- * Calls
    openTerminal,
    closeTerminal,
    resizeTerminal,
    resetTerminal,

    -- * Tracked references
    trackPoint,
    untrackRef,
    refCoordinate,
    moveTrackedRef,
    refHasValue,
  )
where

import Control.Exception (throwIO)
import Data.IntMap.Strict qualified as IntMap
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), ResultCode (..))
import Ghostty.Effectful.Internal.Error (expectSuccess, resultCode)
import Ghostty.FFI.GridRef
  ( GhosttyTrackedGridRef,
    ghosttyTrackedGridRefFree,
    ghosttyTrackedGridRefHasValue,
    ghosttyTrackedGridRefPoint,
    ghosttyTrackedGridRefSet,
  )
import Ghostty.FFI.Point (GhosttyPoint, GhosttyPointCoordinate, GhosttyPointTag)
import Ghostty.FFI.Terminal
  ( GhosttyTerminal,
    ghosttyTerminalFree,
    ghosttyTerminalGridRefTrack,
    ghosttyTerminalNew,
    ghosttyTerminalReset,
    ghosttyTerminalResize,
  )

-- | A terminal, alive in the region @s@ that
-- 'Ghostty.Effectful.Terminal.withTerminal' opens. The type variable is
-- rank-2 there, as in @ST@, so a 'Terminal' cannot be used after its region
-- has freed it.
data Terminal s = Terminal
  { -- | the library's handle
    handle :: GhosttyTerminal,
    -- | the tracked references made from it and not yet untracked
    refs :: IORef (IntMap GhosttyTrackedGridRef),
    -- | the next registry key
    nextRef :: IORef Int
  }

-- | A tracked reference to a cell of a terminal in the region @s@: it
-- follows its cell as the terminal scrolls, prunes and reflows, and is
-- freed with the terminal if not before.
data TrackedRef s = TrackedRef
  { -- | the terminal it was made from
    terminal :: Terminal s,
    -- | its key in the terminal's registry
    key :: Int
  }

instance Eq (TrackedRef s) where
  a == b = a.key == b.key

-- | A number of cells along one axis: never zero, which the library refuses
-- for a terminal's columns or rows.
newtype Extent = Extent Word16
  deriving stock (Eq, Ord, Show)

-- | An extent of @n@ cells, if @n@ is not zero.
--
-- >>> extent 80
-- Just (Extent 80)
-- >>> extent 0
-- Nothing
extent :: Word16 -> Maybe Extent
extent n = if n == 0 then Nothing else Just (Extent n)

-- | The number of cells.
fromExtent :: Extent -> Word16
fromExtent (Extent n) = n

-- | A terminal's grid, in cells.
data GridSize = GridSize
  { -- | columns
    cols :: Extent,
    -- | rows
    rows :: Extent
  }
  deriving stock (Eq, Show)

-- | One cell's size in pixels, which image protocols and size reports use.
-- Zero means unknown.
data CellPixels = CellPixels
  { -- | width
    width :: Word32,
    -- | height
    height :: Word32
  }
  deriving stock (Eq, Show)

-- | A cell size of zero by zero: unknown, the library's own starting value.
noCellPixels :: CellPixels
noCellPixels = CellPixels 0 0

-- | How a terminal starts.
data TerminalConfig = TerminalConfig
  { -- | the grid
    size :: GridSize,
    -- | one cell, in pixels
    cellPixels :: CellPixels
  }
  deriving stock (Eq, Show)

-- | A terminal of the given grid, with the cell size unknown.
terminalConfig :: GridSize -> TerminalConfig
terminalConfig g = TerminalConfig {size = g, cellPixels = noCellPixels}

-- | @ghostty_terminal_new@ with the default allocator (§3.3), at the
-- configured grid, with an empty registry. The cell size is the caller's to
-- set, inside the scope that frees the terminal on every path.
openTerminal :: TerminalConfig -> IO (Terminal s)
openTerminal cfg = do
  handle <- alloca $ \out -> do
    r <- ghosttyTerminalNew nullPtr out (fromExtent cfg.size.cols) (fromExtent cfg.size.rows)
    expectSuccess (CallSite "ghostty_terminal_new") r
    peek out
  refs <- newIORef mempty
  nextRef <- newIORef 0
  pure Terminal {handle, refs, nextRef}

-- | Free every tracked reference still registered, taking their number off
-- the counter, then @ghostty_terminal_free@.
closeTerminal :: IORef Int -> Terminal s -> IO ()
closeTerminal open t = do
  live <- atomicModifyIORef' t.refs (mempty,)
  traverse_ ghosttyTrackedGridRefFree live
  modifyIORef' open (subtract (IntMap.size live))
  ghosttyTerminalFree t.handle

-- | @ghostty_terminal_resize@. Its one documented refusal, a zero extent or a
-- null terminal, cannot be expressed, so only running out of memory remains.
resizeTerminal :: GhosttyTerminal -> GridSize -> CellPixels -> IO ()
resizeTerminal h g px =
  expectSuccess (CallSite "ghostty_terminal_resize")
    =<< ghosttyTerminalResize h (fromExtent g.cols) (fromExtent g.rows) px.width px.height

-- | @ghostty_terminal_reset@: a full reset (RIS), keeping the size.
resetTerminal :: GhosttyTerminal -> IO ()
resetTerminal = ghosttyTerminalReset

-- | @ghostty_terminal_grid_ref_track@: a tracked reference to the cell at a
-- point of the active screen, registered with the terminal and counted;
-- 'Nothing' if the point is outside its coordinate system, which is the
-- header's @GHOSTTY_INVALID_VALUE@ for a valid terminal.
trackPoint :: IORef Int -> Terminal s -> GhosttyPoint -> IO (Maybe (TrackedRef s))
trackPoint open t p = alloca $ \out -> do
  r <- with p $ \pp -> ghosttyTerminalGridRefTrack t.handle pp out
  case resultCode r of
    ResultSuccess -> do
      ref <- peek out
      key <- atomicModifyIORef' t.nextRef (\n -> (n + 1, n))
      modifyIORef' t.refs (IntMap.insert key ref)
      modifyIORef' open (+ 1)
      pure (Just TrackedRef {terminal = t, key})
    ResultInvalidValue -> pure Nothing
    ResultOutOfMemory -> throwIO (OutOfMemory site)
    other -> throwIO (Invariant site other)
  where
    site = CallSite "ghostty_terminal_grid_ref_track"

-- | Free a reference and drop it from its terminal's registry. A reference
-- already untracked, or whose terminal is closed, is left alone.
untrackRef :: IORef Int -> TrackedRef s -> IO ()
untrackRef open ref = do
  found <- atomicModifyIORef' ref.terminal.refs $ \m ->
    (IntMap.delete ref.key m, IntMap.lookup ref.key m)
  for_ found $ \h -> do
    ghosttyTrackedGridRefFree h
    modifyIORef' open (subtract 1)

-- | The library's handle for a reference still registered.
registered :: TrackedRef s -> IO (Maybe GhosttyTrackedGridRef)
registered ref = IntMap.lookup ref.key <$> readIORef ref.terminal.refs

-- | @ghostty_tracked_grid_ref_point@: the reference's position in a
-- coordinate system; 'Nothing' if it has lost its cell, has no position
-- there, or was untracked.
refCoordinate :: TrackedRef s -> GhosttyPointTag -> IO (Maybe GhosttyPointCoordinate)
refCoordinate ref tag =
  registered ref >>= \case
    Nothing -> pure Nothing
    Just h -> alloca $ \out -> do
      r <- ghosttyTrackedGridRefPoint h tag out
      case resultCode r of
        ResultSuccess -> Just <$> peek out
        ResultNoValue -> pure Nothing
        other -> throwIO (Invariant (CallSite "ghostty_tracked_grid_ref_point") other)

-- | @ghostty_tracked_grid_ref_set@: move the reference to a point of the
-- terminal's active screen. 'False' if the point is outside its coordinate
-- system (@GHOSTTY_INVALID_VALUE@, the reference and terminal being valid)
-- or the reference was untracked; the reference is then unchanged.
moveTrackedRef :: TrackedRef s -> GhosttyPoint -> IO Bool
moveTrackedRef ref p =
  registered ref >>= \case
    Nothing -> pure False
    Just h -> do
      r <- with p (ghosttyTrackedGridRefSet h ref.terminal.handle)
      case resultCode r of
        ResultSuccess -> pure True
        ResultInvalidValue -> pure False
        ResultOutOfMemory -> throwIO (OutOfMemory site)
        other -> throwIO (Invariant site other)
  where
    site = CallSite "ghostty_tracked_grid_ref_set"

-- | @ghostty_tracked_grid_ref_has_value@: whether the reference still names
-- a cell; 'False' once untracked.
refHasValue :: TrackedRef s -> IO Bool
refHasValue ref =
  registered ref >>= \case
    Nothing -> pure False
    Just h -> (/= 0) <$> ghosttyTrackedGridRefHasValue h
