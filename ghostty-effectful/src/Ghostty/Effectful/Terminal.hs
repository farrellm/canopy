-- | Terminals (DESIGN.md §4.2, §4.8): scoped to a region, so none outlives
-- its @free@.
--
-- This step has the lifecycle: open, resize, reset. Writing to a terminal and
-- reading it back arrive with the typed keys and events.
module Ghostty.Effectful.Terminal
  ( -- * Terminals
    Terminal,
    withTerminal,
    resize,
    reset,
    openTerminals,

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
import Ghostty.Effectful.Internal.Effect (Ghostty, Handles (..), owned)
import Ghostty.Effectful.Internal.Terminal
  ( CellPixels (..),
    Extent,
    GridSize (..),
    Terminal (..),
    TerminalConfig (..),
    extent,
    freeTerminal,
    fromExtent,
    newTerminal,
    noCellPixels,
    resetTerminal,
    resizeTerminal,
    terminalConfig,
  )

-- | Open a terminal for the duration of the action, and free it when the
-- action ends, by return or by exception. The terminal's type variable is
-- rank-2, so it cannot leave the action (§4.2).
withTerminal :: (Ghostty :> es) => TerminalConfig -> (forall s. Terminal s -> Eff es a) -> Eff es a
withTerminal cfg k = do
  hs <- owned "withTerminal"
  unsafeSeqUnliftIO $ \unlift ->
    bracket
      (newTerminal cfg <* modifyIORef' hs.openCount (+ 1))
      (\h -> freeTerminal h <* modifyIORef' hs.openCount (subtract 1))
      ( \h -> do
          when (cfg.cellPixels /= noCellPixels) $ resizeTerminal h cfg.size cfg.cellPixels
          unlift (k (Terminal h))
      )

-- | Resize the grid and set the cell size. A terminal's primary screen
-- reflows if wraparound is on; the alternate screen does not.
resize :: (Ghostty :> es) => Terminal s -> GridSize -> CellPixels -> Eff es ()
resize (Terminal h) g px = do
  _ <- owned "resize"
  unsafeEff_ (resizeTerminal h g px)

-- | A full reset (RIS): modes, scrollback, scrolling region and screen
-- contents, keeping the size.
reset :: (Ghostty :> es) => Terminal s -> Eff es ()
reset (Terminal h) = do
  _ <- owned "reset"
  unsafeEff_ (resetTerminal h)

-- | How many terminals are open in this 'Ghostty.Effectful.runGhostty': each
-- 'withTerminal' adds one for its duration.
openTerminals :: (Ghostty :> es) => Eff es Int
openTerminals = do
  hs <- owned "openTerminals"
  unsafeEff_ (readIORef hs.openCount)
