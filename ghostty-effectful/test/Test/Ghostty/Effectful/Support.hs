{-# LANGUAGE GADTs #-}

-- | Helpers shared by the test modules.
module Test.Ghostty.Effectful.Support
  ( -- * Running
    ghostty,
    config,
    grid,
    withFresh,
    withConfig,
    expectJust,

    -- * Header names
    Some (..),
    headerName,
    rowName,
    squash,
  )
where

import Data.Char (isAlphaNum, isUpper, toLower, toUpper)
import Data.Text qualified as T
import Effectful (Eff, IOE, runEff)
import Ghostty.Effectful (Ghostty, runGhostty)
import Ghostty.Effectful.Internal.Size (Extent (..))
import Ghostty.Effectful.Terminal (GridSize (..), Terminal, TerminalConfig, terminalConfig, withTerminal)
import Ghostty.FFI.Layout (CEnum (..))
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

-- | Run with a fresh terminal of 'config'.
withFresh :: (forall s. Terminal s -> Eff '[Ghostty, IOE] a) -> IO a
withFresh = withConfig config

-- | Run with a fresh terminal, failing the test on a version mismatch.
withConfig :: TerminalConfig -> (forall s. Terminal s -> Eff '[Ghostty, IOE] a) -> IO a
withConfig cfg k = ghostty (withTerminal cfg k)

-- | The value, or the test fails with the message.
expectJust :: (MonadIO m) => String -> Maybe a -> m a
expectJust msg = maybe (liftIO (assertFailure msg)) pure

-- | Some key, its type forgotten.
data Some f where
  Some :: f a -> Some f

-- | A constructor's name as the header spells its row: the first word of
-- its 'show', in upper snake case (@CursorX@ is @CURSOR_X@).
headerName :: String -> Text
headerName = toText . snake . takeWhile (/= ' ')
  where
    snake = \case
      c : cs -> toUpper c : concatMap (\x -> if isUpper x then ['_', x] else [toUpper x]) cs
      [] -> []

-- | The header's name for a value.
rowName :: CEnum -> Integer -> Maybe Text
rowName e n = fst <$> find ((== n) . snd) e.values

-- | A name without case or underscores, so that @KeyDigit0@ and
-- @GHOSTTY_KEY_DIGIT_0@, and @KeyF10@ and @GHOSTTY_KEY_F10@, compare equal.
squash :: Text -> Text
squash = T.filter isAlphaNum . T.map toLower
