-- | Terminals (DESIGN.md §4.2, §4.4, §4.5, §4.8): scoped to a region, so
-- none outlives its @free@; fed VT, which they report on as events
-- ("Ghostty.Effectful.Event"); read and written through typed keys; and
-- tracked references into their grids.
module Ghostty.Effectful.Terminal
  ( -- * Terminals
    Terminal,
    withTerminal,
    feed,
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

import Effectful (Eff, (:>))
import Effectful.Dispatch.Static (unsafeEff_)
import Ghostty.Effectful.Event (TerminalEvent)
import Ghostty.Effectful.Internal.Convert (fromCoordinate, pointTag, toPoint)
import Ghostty.Effectful.Internal.Effect (Ghostty, Handles (..), countOf, ownedIO, scoped)
import Ghostty.Effectful.Internal.Feed (feedTerminal, resetTerminal, resizeTerminal)
import Ghostty.Effectful.Internal.Keys (getData, setOption)
import Ghostty.Effectful.Internal.Size (CellPixels (..), Extent, GridSize (..), extent, fromExtent, noCellPixels)
import Ghostty.Effectful.Internal.Terminal
  ( Terminal (..),
    TerminalConfig (..),
    TrackedRef (..),
    closeTerminal,
    moveTrackedRef,
    openTerminal,
    refCoordinate,
    refHasValue,
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
withTerminal cfg k =
  scoped "withTerminal" (.terminalCount) (\hs -> openTerminal hs.refCount cfg) closeTerminal $ \t -> do
    -- A new terminal has no mode on that a resize reports to, so this
    -- causes no event.
    when (cfg.cellPixels /= noCellPixels) $ void (unsafeEff_ (resizeTerminal t cfg.size cfg.cellPixels))
    k t

-- | Write the program's output to the terminal, and return what the
-- terminal reported while processing it, in the order the bytes produced
-- it (§4.5). A sequence may be split across calls. Replies to the
-- program's queries are 'Ghostty.Effectful.Event.WritePty' events, for the
-- caller to write to its PTY.
--
-- The write stops after each OSC, so that a prompt mark's cursor is read
-- right after the mark, and a title change's title right after the change.
-- If an earlier 'feed', 'resize' or 'reset' threw before returning its
-- events, what it left comes first. Every byte is written even if resolving an event throws; the
-- exception is thrown after the write.
feed :: (Ghostty :> es) => Terminal s -> ByteString -> Eff es [TerminalEvent]
feed t bytes =
  ownedIO "feed" (feedTerminal t bytes)

-- | Resize the grid and set the cell size. A terminal's primary screen
-- reflows if wraparound is on; the alternate screen does not. Returns what
-- the resize reported: the end of a render hold, and the in-band size
-- report (mode 2048) for the program.
resize :: (Ghostty :> es) => Terminal s -> GridSize -> CellPixels -> Eff es [TerminalEvent]
resize t g px =
  ownedIO "resize" (resizeTerminal t g px)

-- | A full reset (RIS): modes, scrollback, scrolling region and screen
-- contents, keeping the size. Returns what the reset reported: the end of a
-- render hold.
reset :: (Ghostty :> es) => Terminal s -> Eff es [TerminalEvent]
reset t =
  ownedIO "reset" (resetTerminal t)

-- | How many terminals are open in this 'Ghostty.Effectful.runGhostty': each
-- 'withTerminal' adds one for its duration.
openTerminals :: (Ghostty :> es) => Eff es Int
openTerminals =
  countOf "openTerminals" (.terminalCount)

-- | Read a key; the result's type follows from it (§4.4).
--
-- > cols <- getT t Cols            -- Word16
-- > fg   <- getT t ColorForeground -- Maybe Rgb
getT :: (Ghostty :> es) => Terminal s -> TerminalData a -> Eff es a
getT t key =
  ownedIO "getT" (getData t.handle key)

-- | Write an option; the value's type follows from it (§4.4). The options
-- that answer the program's queries take effect at its next query. A write
-- causes no event: the header says setting a mode never invokes a callback
-- (not even the end of a render hold), and a test writes every option and
-- finds nothing reported (§4.5).
--
-- > setT t SetTitle "canopy"
-- > setT t (SetMode ModeBracketedPaste) True
-- > setT t SetColorScheme (Just Dark)
setT :: (Ghostty :> es) => Terminal s -> TerminalOption a -> a -> Eff es ()
setT t opt v =
  ownedIO "setT" (setOption t opt v)

-- | Track the cell at a point of the active screen: the reference follows
-- it as the terminal scrolls, prunes and reflows. 'Nothing' if the point is
-- outside its coordinate system. The reference lives until 'untrack' or the
-- end of the terminal's region, whichever is first.
track :: (Ghostty :> es, KnownCoord c) => Terminal s -> Point c -> Eff es (Maybe (TrackedRef s))
track t p =
  ownedIO "track" (trackPoint t (toPoint p))

-- | Free a reference now rather than with its terminal. Each live reference
-- costs the terminal work on every scroll, so a long session untracks what
-- it no longer needs. Untracking twice is harmless; an untracked reference
-- has no point and cannot be moved.
untrack :: (Ghostty :> es) => TrackedRef s -> Eff es ()
untrack ref =
  ownedIO "untrack" (untrackRef ref)

-- | Where the reference's cell is now, in the coordinate system @c@:
-- 'Nothing' if it has lost its cell (pruned from the scrollback), is not
-- representable in @c@ (scrolled out of the viewport, say), or was
-- untracked. The position is on the screen the reference was made on,
-- which may not be the active one.
refPoint :: forall c s es. (Ghostty :> es, KnownCoord c) => TrackedRef s -> Eff es (Maybe (Point c))
refPoint ref =
  ownedIO "refPoint" (fmap fromCoordinate <$> refCoordinate ref (pointTag (coordSing @c)))

-- | Move a reference to the cell at a point of the active screen. 'False',
-- and the reference unchanged, if the point is outside its coordinate
-- system or the reference was untracked.
moveRef :: (Ghostty :> es, KnownCoord c) => TrackedRef s -> Point c -> Eff es Bool
moveRef ref p =
  ownedIO "moveRef" (moveTrackedRef ref (toPoint p))

-- | Whether the reference still names a cell.
refAlive :: (Ghostty :> es) => TrackedRef s -> Eff es Bool
refAlive ref =
  ownedIO "refAlive" (refHasValue ref)

-- | How many tracked references are live in this
-- 'Ghostty.Effectful.runGhostty', across its terminals.
openTrackedRefs :: (Ghostty :> es) => Eff es Int
openTrackedRefs =
  countOf "openTrackedRefs" (.refCount)
