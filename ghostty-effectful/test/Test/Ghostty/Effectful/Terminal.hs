-- | Scoped terminals and the effect's checks (PLAN.md M1.4): the version check
-- refuses, a handle is released on every path, and the effect refuses use
-- from a thread that does not own it.
module Test.Ghostty.Effectful.Terminal (tests) where

import Control.Concurrent (forkIO)
import Control.Exception (ErrorCall (..), throwIO, try)
import Data.Text qualified as T
import Effectful (Eff, IOE, Limit (..), Persistence (..), UnliftStrategy (..), runEff, withEffToIO, (:>))
import Effectful.Exception qualified as E
import Ghostty.Effectful (runGhostty)
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), VersionMismatch (..))
import Ghostty.Effectful.Internal.Effect (linkedVersion, optionalFeatures, runGhosttyPinnedTo)
import Ghostty.Effectful.Terminal
import Ghostty.FFI.BuildInfo (GhosttyBuildInfo (..), ghosttyBuildInfoEnum)
import Ghostty.FFI.Layout (CEnum (..))
import Test.Ghostty.Effectful.Support (config, ghostty, grid, withFresh)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Terminal"
    [ testCase "runGhostty runs against the pinned build" $ do
        r <- runEff (runGhostty (pure ()))
        r @?= Right (),
      testCase "a version mismatch refuses, and runs nothing" $ do
        ran <- newIORef False
        linked <- linkedVersion
        r <- runEff (runGhosttyPinnedTo "not-the-pin" (writeIORef ran True))
        r @?= Left VersionMismatch {pinned = "not-the-pin", linked}
        readIORef ran >>= (@?= False),
      testCase "regression 12: the version check asks for every optional feature, kitty graphics included" $
        sort [toInteger n | (GhosttyBuildInfo n, _) <- optionalFeatures]
          @?= sort
            [ v
            | (name, v) <- ghosttyBuildInfoEnum.values,
              "GHOSTTY_BUILD_INFO_FEATURE_" `T.isPrefixOf` name || name == "GHOSTTY_BUILD_INFO_KITTY_GRAPHICS"
            ],
      testCase "a terminal is open inside its scope and freed after" $ do
        r <- ghostty $ do
          inside <- withTerminal config (const openTerminals)
          after <- openTerminals
          pure (inside, after)
        r @?= (1, 0),
      testCase "nested terminals are each freed" $ do
        r <- ghostty $ do
          inside <- withTerminal config $ \_ -> withTerminal config (const openTerminals)
          after <- openTerminals
          pure (inside, after)
        r @?= (2, 0),
      testCase "a terminal is freed when its scope throws" $ do
        r <- ghostty $ do
          e <- E.try @ErrorCall (withTerminal config $ \_ -> liftIO (throwIO (ErrorCall "boom")))
          n <- openTerminals
          pure (either (\(ErrorCall m) -> m) (const "no exception") e, n)
        r @?= ("boom", 0),
      testCase "resize and reset run on an open terminal" $ do
        r <- ghostty $ withTerminal config {cellPixels = CellPixels 9 18} $ \t -> do
          _ <- resize t (grid 120 40) (CellPixels 10 20)
          _ <- reset t
          openTerminals
        r @?= 1,
      testCase "an operation from a forked thread throws WrongThread" $ do
        r <- withFresh $ \t -> fromForkedThread (void (reset t))
        r @?= Left (WrongThread (CallSite "reset")),
      testCase "opening a terminal from a forked thread throws WrongThread" $ do
        r <- ghostty $ fromForkedThread (withTerminal config (const pass))
        r @?= Left (WrongThread (CallSite "withTerminal"))
    ]

-- | Run an action on another thread with the effect's environment cloned
-- into it, as effectful's own @forkIO@ does (§4.6), and wait for its result.
fromForkedThread :: (IOE :> es) => Eff es () -> Eff es (Either GhosttyError ())
fromForkedThread act = withEffToIO (ConcUnlift Persistent Unlimited) $ \run -> do
  done <- newEmptyMVar
  _ <- forkIO (try (run act) >>= putMVar done)
  takeMVar done
