-- | Render states and frames (DESIGN.md §4.3): what a terminal shows, copied
-- out for drawing.
--
-- A render state is an incremental copy of one terminal's viewport. Each
-- 'withFrame' updates it and opens a frame, whose rows and their data are
-- borrowed from the render state until its next update; so they carry the
-- frame's tag @f@, which cannot leave 'withFrame'. What leaves is copied: a
-- row's 'cells' are plain 'Cell' values. A frame that runs to its end marks
-- its changes consumed, so the next frame's 'dirtyRows' are only the rows
-- that changed after it. A 'Cell' and its parts are in
-- "Ghostty.Effectful.Cell".
--
-- __Synchronized output.__ When a program begins a hold (mode 2026), the
-- render state is updated to the frame the program wants left on screen,
-- before the bytes after it are processed, and frames show that capture
-- until the hold ends ('frameHeld'). The terminal has no clock, so a program
-- that never ends its hold would freeze the frame; the caller ends a hold
-- that lasts too long by turning the mode off
-- (@setT t (SetMode ModeSynchronizedOutput) False@), and the next frame is live.
module Ghostty.Effectful.Render
  ( -- * Render states
    RenderState,
    withRenderState,
    openRenderStates,

    -- * Frames
    Frame,
    withFrame,
    frameHeld,
    getF,
    dirtyRows,
    allRows,

    -- * Rows
    Row,
    rowY,
    getR,
    cells,

    -- * Keys
    module Ghostty.Effectful.Render.Keys,
  )
where

import Control.Exception (mask, onException)
import Data.Vector (Vector)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Static (unsafeSeqUnliftIO)
import Ghostty.Effectful.Cell (Cell)
import Ghostty.Effectful.Internal.Effect (Ghostty, Handles (..), countOf, owned, ownedIO, scoped)
import Ghostty.Effectful.Internal.Render
  ( Frame (..),
    FrameInfo (..),
    Opened (..),
    RenderState,
    Row (..),
    abandonFrame,
    allRowsOf,
    beginFrame,
    closeRenderState,
    copyCells,
    dirtyRowsOf,
    endFrame,
    getFrameData,
    getRowData,
    openRenderState,
  )
import Ghostty.Effectful.Internal.Terminal (Terminal)
import Ghostty.Effectful.Render.Keys

-- | Open a render state of the terminal for the duration of the action, and
-- free it when the action ends, by return or by exception. Its type variable
-- is rank-2, so it cannot leave the action, and the action runs inside the
-- terminal's region, so the terminal outlives it.
withRenderState :: (Ghostty :> es) => Terminal t -> (forall s. RenderState s -> Eff es a) -> Eff es a
withRenderState t k =
  scoped "withRenderState" (.renderCount) (const (openRenderState t)) closeRenderState k

-- | How many render states are open in this 'Ghostty.Effectful.runGhostty'.
openRenderStates :: (Ghostty :> es) => Eff es Int
openRenderStates =
  countOf "openRenderStates" (.renderCount)

-- | Update the render state from its terminal and read the frame. Its type
-- variable is rank-2, so nothing borrowed from it outlives the action. If the
-- action returns, the frame's changes are consumed; if it throws, they are
-- not, and the next frame reports them again.
--
-- While a program holds synchronized output, the render state is not
-- updated: the frame is the one captured when the hold began. A 'withFrame'
-- inside another on the same render state is the same frame: it neither
-- updates nor consumes.
withFrame :: (Ghostty :> es) => RenderState s -> (forall f. Frame s f -> Eff es a) -> Eff es a
withFrame rs k = do
  _ <- owned "withFrame"
  -- Masked between opening the frame and arming its close, so an
  -- asynchronous exception cannot leave the frame marked open for good.
  unsafeSeqUnliftIO $ \unlift -> mask $ \restore -> do
    let run info = restore (unlift (k Frame {state = rs, info}))
    beginFrame rs >>= \case
      Nested info -> run info
      Outermost info -> do
        a <- run info `onException` abandonFrame rs
        a <$ endFrame rs

-- | Whether the frame is the one captured when a synchronized-output hold
-- began, the hold still on.
frameHeld :: Frame s f -> Bool
frameHeld f = f.info.held

-- | Read the frame; the result's type follows from the key (§4.4).
--
-- > cursor <- getF f RenderCursor   -- Cursor
-- > dirty  <- getF f RenderDirty    -- Dirty
getF :: (Ghostty :> es) => Frame s f -> FrameData a -> Eff es a
getF f key =
  ownedIO "getF" (getFrameData f.state key)

-- | The rows that need drawing, top to bottom: none if nothing changed
-- since the last frame that ran to its end, all if the whole frame did
-- ('FullyDirty'), and otherwise the rows that changed. The library may name
-- a row that did not change (every row is dirty after the viewport
-- scrolls); drawing one again is always right.
dirtyRows :: (Ghostty :> es) => Frame s f -> Eff es [Row s f]
dirtyRows f =
  ownedIO "dirtyRows" (dirtyRowsOf f)

-- | Every row of the frame, top to bottom.
allRows :: (Ghostty :> es) => Frame s f -> Eff es [Row s f]
allRows f =
  ownedIO "allRows" (allRowsOf f)

-- | The row's position from the top of the viewport.
rowY :: Row s f -> Word16
rowY r = r.y

-- | Read the row; the result's type follows from the key (§4.4).
getR :: (Ghostty :> es) => Row s f -> RowData a -> Eff es a
getR r key =
  ownedIO "getR" (getRowData r key)

-- | The row's cells, left to right, one per column, copied out: they
-- outlive the frame.
cells :: (Ghostty :> es) => Row s f -> Eff es (Vector Cell)
cells r =
  ownedIO "cells" (copyCells r)
