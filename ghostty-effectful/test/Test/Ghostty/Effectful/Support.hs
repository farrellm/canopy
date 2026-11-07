-- | Helpers shared by the test modules.
module Test.Ghostty.Effectful.Support
  ( ghostty,
    config,
    grid,
  )
where

import Effectful (Eff, IOE, runEff)
import Ghostty.Effectful (Ghostty, runGhostty)
import Ghostty.Effectful.Internal.Terminal (Extent (..))
import Ghostty.Effectful.Terminal (GridSize (..), TerminalConfig, terminalConfig)
import Test.Tasty.HUnit (assertFailure)

-- | Run under the effect, failing the test on a version mismatch.
ghostty :: Eff '[Ghostty, IOE] a -> IO a
ghostty act = runEff (runGhostty act) >>= either (assertFailure . show) pure

-- | An 80×24 terminal.
config :: TerminalConfig
config = terminalConfig (grid 80 24)

-- | A grid of non-zero extents, built with the internal constructor.
grid :: Word16 -> Word16 -> GridSize
grid c r = GridSize {cols = Extent c, rows = Extent r}
