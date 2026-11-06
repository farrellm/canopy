-- | Terminals (DESIGN.md §4.2, §4.4, §4.8): scoped to a region, so none
-- outlives its @free@; read and written through typed keys; and tracked
-- references into their grids.
--
-- Writing VT to a terminal and the events it produces arrive with @feed@
-- (PLAN.md M1.6).
module Ghostty.Effectful.Terminal
  ( -- * Terminals
    Terminal,
    withTerminal,
    resize,
    reset,
    openTerminals,

    -- * Typed keys
    getT,
    setT,
    module Ghostty.Effectful.Terminal.Keys,

    -- * Tracked references
    TrackedRef,
    track,
    untrack,
    refPoint,
    moveRef,
    refAlive,
    openTrackedRefs,

    -- * Configuration
    TerminalConfig (..),
    terminalConfig,

    -- * Sizes
    GridSize (..),
    Extent,
    extent,
    fromExtent,
    CellPixels (..),
    noCellPixels,
  )
where

import Control.Exception (bracket)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Static (unsafeEff_, unsafeSeqUnliftIO)
import Ghostty.Effectful.Internal.Convert (fromCoordinate, pointTag, toPoint)
import Ghostty.Effectful.Internal.Effect (Ghostty, Handles (..), owned)
import Ghostty.Effectful.Internal.Keys (getData, setOption)
import Ghostty.Effectful.Internal.Terminal
  ( CellPixels (..),
    Extent,
    GridSize (..),
    Terminal (..),
    TerminalConfig (..),
    TrackedRef,
    closeTerminal,
    extent,
    fromExtent,
    moveTrackedRef,
    noCellPixels,
    openTerminal,
    refCoordinate,
    refHasValue,
    resetTerminal,
    resizeTerminal,
    terminalConfig,
    trackPoint,
    untrackRef,
  )
import Ghostty.Effectful.Point (KnownCoord (..), Point)
import Ghostty.Effectful.Terminal.Keys

-- | Open a terminal for the duration of the action, and free it when the
-- action ends, by return or by exception, with every tracked reference made
-- from it. The terminal's type variable is rank-2, so it cannot leave the
-- action (§4.2).
withTerminal :: (Ghostty :> es) => TerminalConfig -> (forall s. Terminal s -> Eff es a) -> Eff es a
withTerminal cfg k = do
  hs <- owned "withTerminal"
  unsafeSeqUnliftIO $ \unlift ->
    bracket
      (openTerminal cfg <* modifyIORef' hs.openCount (+ 1))
      (\t -> closeTerminal hs.refCount t <* modifyIORef' hs.openCount (subtract 1))
      ( \t -> do
          when (cfg.cellPixels /= noCellPixels) $ resizeTerminal t.handle cfg.size cfg.cellPixels
          unlift (k t)
      )

-- | Resize the grid and set the cell size. A terminal's primary screen
-- reflows if wraparound is on; the alternate screen does not.
resize :: (Ghostty :> es) => Terminal s -> GridSize -> CellPixels -> Eff es ()
resize t g px = do
  _ <- owned "resize"
  unsafeEff_ (resizeTerminal t.handle g px)

-- | A full reset (RIS): modes, scrollback, scrolling region and screen
-- contents, keeping the size.
reset :: (Ghostty :> es) => Terminal s -> Eff es ()
reset t = do
  _ <- owned "reset"
  unsafeEff_ (resetTerminal t.handle)

-- | How many terminals are open in this 'Ghostty.Effectful.runGhostty': each
-- 'withTerminal' adds one for its duration.
openTerminals :: (Ghostty :> es) => Eff es Int
openTerminals = do
  hs <- owned "openTerminals"
  unsafeEff_ (readIORef hs.openCount)

-- | Read a key; the result's type follows from it (§4.4).
--
-- > cols <- getT t Cols            -- Word16
-- > fg   <- getT t ColorForeground -- Maybe Rgb
getT :: (Ghostty :> es) => Terminal s -> TerminalData a -> Eff es a
getT t key = do
  _ <- owned "getT"
  unsafeEff_ (getData t.handle key)

-- | Write an option; the value's type follows from it (§4.4).
--
-- > setT t SetTitle "canopy"
-- > setT t (SetMode ModeBracketedPaste) True
setT :: (Ghostty :> es) => Terminal s -> TerminalOption a -> a -> Eff es ()
setT t opt v = do
  _ <- owned "setT"
  unsafeEff_ (setOption t.handle opt v)

-- | Track the cell at a point of the active screen: the reference follows
-- it as the terminal scrolls, prunes and reflows. 'Nothing' if the point is
-- outside its coordinate system. The reference lives until 'untrack' or the
-- end of the terminal's region, whichever is first.
track :: (Ghostty :> es, KnownCoord c) => Terminal s -> Point c -> Eff es (Maybe (TrackedRef s))
track t p = do
  hs <- owned "track"
  unsafeEff_ (trackPoint hs.refCount t (toPoint p))

-- | Free a reference now rather than with its terminal. Each live reference
-- costs the terminal work on every scroll, so a long session untracks what
-- it no longer needs. Untracking twice is harmless; an untracked reference
-- has no point and cannot be moved.
untrack :: (Ghostty :> es) => TrackedRef s -> Eff es ()
untrack ref = do
  hs <- owned "untrack"
  unsafeEff_ (untrackRef hs.refCount ref)

-- | Where the reference's cell is now, in the coordinate system @c@:
-- 'Nothing' if it has lost its cell (pruned from the scrollback), is not
-- representable in @c@ (scrolled out of the viewport, say), or was
-- untracked. The position is on the screen the reference was made on,
-- which may not be the active one.
refPoint :: forall c s es. (Ghostty :> es, KnownCoord c) => TrackedRef s -> Eff es (Maybe (Point c))
refPoint ref = do
  _ <- owned "refPoint"
  unsafeEff_ (fmap fromCoordinate <$> refCoordinate ref (pointTag (coordSing @c)))

-- | Move a reference to the cell at a point of the active screen. 'False',
-- and the reference unchanged, if the point is outside its coordinate
-- system or the reference was untracked.
moveRef :: (Ghostty :> es, KnownCoord c) => TrackedRef s -> Point c -> Eff es Bool
moveRef ref p = do
  _ <- owned "moveRef"
  unsafeEff_ (moveTrackedRef ref (toPoint p))

-- | Whether the reference still names a cell.
refAlive :: (Ghostty :> es) => TrackedRef s -> Eff es Bool
refAlive ref = do
  _ <- owned "refAlive"
  unsafeEff_ (refHasValue ref)

-- | How many tracked references are live in this
-- 'Ghostty.Effectful.runGhostty', across its terminals.
openTrackedRefs :: (Ghostty :> es) => Eff es Int
openTrackedRefs = do
  hs <- owned "openTrackedRefs"
  unsafeEff_ (readIORef hs.refCount)
