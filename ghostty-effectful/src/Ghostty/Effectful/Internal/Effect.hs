{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- | The 'Ghostty' effect and its interpreter (DESIGN.md §4.2, §4.6, §4.7).
--
-- 'Ghostty' is statically dispatched. Its representation is the owner thread
-- and the count of open handles, and its operations are plain functions in the
-- modules of what they operate on ("Ghostty.Effectful.Terminal" and its
-- siblings), each starting with 'owned'. There is one interpreter:
-- libghostty-vt is headless and deterministic, so the library itself is what
-- tests run against, and a fake would only test the fake.
module Ghostty.Effectful.Internal.Effect
  ( -- * The effect
    Ghostty,
    Handles (..),
    owned,

    -- * Interpreters
    runGhostty,
    runGhosttyPinnedTo,

    -- * Build identity
    linkedBuild,
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (throwIO)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), VersionMismatch (..))
import Ghostty.FFI.BuildInfo (ghosttyBuildInfo, ghosttyPinnedCommit, pattern GhosttyBuildInfoVersionBuild)
import Ghostty.FFI.Types (GhosttyString, ghosttyStringBytes, pattern GhosttySuccess)

-- | The libghostty-vt binding: the right to make calls, from the thread that
-- ran 'runGhostty', on handles opened in its scope.
data Ghostty :: Effect

type instance DispatchOf Ghostty = Static WithSideEffects

newtype instance StaticRep Ghostty = GhosttyRep Handles

-- | What the interpreter holds: the owner, and counts of what is open, by
-- which release is tested.
data Handles = Handles
  { -- | the thread that ran 'runGhostty', the only one allowed to call (§4.6)
    owner :: ThreadId,
    -- | how many terminals are open
    openCount :: IORef Int,
    -- | how many tracked grid references are live
    refCount :: IORef Int
  }

-- | The start of every operation: on the owner thread, the interpreter's
-- state; on any other, 'WrongThread', before any C call. The argument names
-- the operation.
owned :: (Ghostty :> es) => Text -> Eff es Handles
owned op = do
  GhosttyRep hs <- getStaticRep
  unsafeEff_ $ do
    me <- myThreadId
    when (me /= hs.owner) $ throwIO (WrongThread (CallSite op))
    pure hs

-- | Run the effect against the linked libghostty-vt, which must be the pinned
-- build (§3.4): otherwise nothing runs, and the result is the mismatch. Every
-- operation must then come from this thread, or it throws 'WrongThread'
-- (§4.6).
runGhostty :: (IOE :> es) => Eff (Ghostty : es) a -> Eff es (Either VersionMismatch a)
runGhostty = runGhosttyPinnedTo (decodeUtf8 ghosttyPinnedCommit)

-- | 'runGhostty' with another pin in place of @GHOSTTY_COMMIT@, which is how
-- the mismatch is tested.
runGhosttyPinnedTo :: (IOE :> es) => Text -> Eff (Ghostty : es) a -> Eff es (Either VersionMismatch a)
runGhosttyPinnedTo pin act = do
  linked <- liftIO linkedBuild
  if linked /= Just pin
    then pure (Left VersionMismatch {pinned = pin, linked})
    else do
      owner <- liftIO myThreadId
      open <- newIORef 0
      refs <- newIORef 0
      Right <$> evalStaticRep (GhosttyRep Handles {owner, openCount = open, refCount = refs}) act

-- | The linked library's build metadata (@VERSION_BUILD@), which
-- @scripts/build-libghostty.sh@ stamps with the commit it built; 'Nothing' if
-- the library would not say. Build info is a constant of the library, so this
-- needs no owner.
linkedBuild :: IO (Maybe Text)
linkedBuild = alloca $ \(p :: Ptr GhosttyString) -> do
  r <- ghosttyBuildInfo GhosttyBuildInfoVersionBuild (castPtr p)
  if r /= GhosttySuccess
    then pure Nothing
    else Just . decodeUtf8 <$> (ghosttyStringBytes =<< peek p)
