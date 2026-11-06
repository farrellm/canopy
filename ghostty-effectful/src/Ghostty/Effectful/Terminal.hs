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

import Ghostty.Effectful.Internal.Effect (openTerminals, reset, resize, withTerminal)
import Ghostty.Effectful.Internal.Terminal
  ( CellPixels (..),
    Extent,
    GridSize (..),
    Terminal,
    TerminalConfig (..),
    extent,
    fromExtent,
    noCellPixels,
    terminalConfig,
  )
