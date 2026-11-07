-- | Tracked grid references (PLAN.md M1.5): they follow their cell, refuse
-- points outside the grid, and are freed by 'untrack' or with their
-- terminal, never twice.
module Test.Ghostty.Effectful.TrackedRef (tests) where

import Data.ByteString qualified as BS
import Effectful (Eff, IOE, runEff)
import Ghostty.Effectful (Ghostty, runGhostty)
import Ghostty.Effectful.Internal.Terminal (Extent (..))
import Ghostty.Effectful.Point (Coord (..), Point (..))
import Ghostty.Effectful.Terminal
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "TrackedRef"
    [ testCase "a reference reads back its point" $ do
        r <- ghostty $ withTerminal config $ \t -> do
          ref <- tracked t (Point 5 3 :: Point 'Active)
          (,,) <$> refPoint @'Active ref <*> refPoint @'Grid ref <*> refAlive ref
        r @?= (Just (Point 5 3), Just (Point 5 3), True),
      testCase "a point outside the grid is refused" $ do
        r <- ghostty $ withTerminal config $ \t -> do
          a <- track t (Point 80 0 :: Point 'Active)
          b <- track t (Point 0 24 :: Point 'Active)
          n <- openTrackedRefs
          pure (isJust a, isJust b, n)
        r @?= (False, False, 0),
      testCase "a reference follows its cell into the scrollback" $ do
        r <- ghostty $ withTerminal config $ \t -> do
          ref <- tracked t (Point 0 0 :: Point 'Active)
          write t (BS.replicate 30 10)
          (,,) <$> refPoint @'Active ref <*> refPoint @'Grid ref <*> refPoint @'History ref
        r @?= (Nothing, Just (Point 0 0), Just (Point 0 0)),
      -- Regression 6: the library resolves a scrollback row past the
      -- scrollback to the active row that many rows down. With 5 rows of
      -- scrollback, History row 5 was tracked as Active row 0.
      testCase "a scrollback row past the scrollback is refused" $ do
        r <- ghostty $ withTerminal config $ \t -> do
          write t (BS.replicate 28 10)
          past <- track t (Point 0 5 :: Point 'History)
          lastRow <- track t (Point 0 4 :: Point 'History)
          ref <- tracked t (Point 0 0 :: Point 'Active)
          moved <- moveRef ref (Point 0 5 :: Point 'History)
          p <- refPoint @'Active ref
          pure (isJust past, isJust lastRow, moved, p)
        r @?= (False, True, False, Just (Point 0 0)),
      testCase "a reference moves, and stays when the point is outside" $ do
        r <- ghostty $ withTerminal config $ \t -> do
          ref <- tracked t (Point 0 0 :: Point 'Active)
          a <- moveRef ref (Point 7 9 :: Point 'Active)
          b <- moveRef ref (Point 0 99 :: Point 'Active)
          p <- refPoint @'Active ref
          pure (a, b, p)
        r @?= (True, False, Just (Point 7 9)),
      testCase "untrack frees once, and the reference is then inert" $ do
        r <- ghostty $ withTerminal config $ \t -> do
          ref <- tracked t (Point 1 1 :: Point 'Active)
          before <- openTrackedRefs
          untrack ref
          untrack ref
          after <- openTrackedRefs
          p <- refPoint @'Active ref
          alive <- refAlive ref
          moved <- moveRef ref (Point 2 2 :: Point 'Active)
          pure (before, after, p, alive, moved)
        r @?= (1, 0, Nothing, False, False),
      testCase "a terminal frees its references when its region ends" $ do
        r <- ghostty $ do
          inside <- withTerminal config $ \t -> do
            traverse_ (tracked t) [Point 0 0, Point 1 1, Point 2 2 :: Point 'Active]
            ref <- tracked t (Point 3 3 :: Point 'Active)
            untrack ref
            openTrackedRefs
          after <- openTrackedRefs
          pure (inside, after)
        r @?= (3, 0)
    ]

-- | Run under the effect, failing the test on a version mismatch.
ghostty :: Eff '[Ghostty, IOE] a -> IO a
ghostty act = runEff (runGhostty act) >>= either (assertFailure . show) pure

-- | An 80×24 terminal.
config :: TerminalConfig
config = terminalConfig GridSize {cols = Extent 80, rows = Extent 24}

-- | Track a point that must be inside the grid.
tracked :: Terminal s -> Point 'Active -> Eff '[Ghostty, IOE] (TrackedRef s)
tracked t p = track t p >>= maybe (liftIO (assertFailure ("not tracked: " <> show p))) pure

-- | Feed bytes, dropping the events.
write :: Terminal s -> ByteString -> Eff '[Ghostty, IOE] ()
write t = void . feed t
