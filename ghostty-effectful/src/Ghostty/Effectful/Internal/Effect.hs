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
    ownedIO,
    scoped,
    countOf,

    -- * Interpreters
    runGhostty,
    runGhosttyPinnedTo,

    -- * Build identity
    linkedVersion,
    optionalFeatures,
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (bracket, throwIO)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, unsafeEff_, unsafeSeqUnliftIO)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), VersionMismatch (..))
import Ghostty.FFI.BuildInfo
  ( GhosttyBuildInfo,
    ghosttyBuildInfo,
    ghosttyPinnedVersion,
    ghosttyVersion,
    pattern GhosttyBuildInfoFeatureColor,
    pattern GhosttyBuildInfoFeatureFormatter,
    pattern GhosttyBuildInfoFeatureGlyphProtocol,
    pattern GhosttyBuildInfoFeatureGridIntrospection,
    pattern GhosttyBuildInfoFeatureInputEncode,
    pattern GhosttyBuildInfoFeatureRenderState,
    pattern GhosttyBuildInfoFeatureSearch,
    pattern GhosttyBuildInfoFeatureSelection,
    pattern GhosttyBuildInfoFeatureSnapshot,
    pattern GhosttyBuildInfoKittyGraphics,
  )
import Ghostty.FFI.Types (pattern GhosttySuccess)

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
    terminalCount :: IORef Int,
    -- | how many tracked grid references are live
    refCount :: IORef Int,
    -- | how many render states are open
    renderCount :: IORef Int,
    -- | how many key and mouse encoders are open
    encoderCount :: IORef Int
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

-- | An operation that is one IO call: 'owned', then the call.
ownedIO :: (Ghostty :> es) => Text -> IO a -> Eff es a
ownedIO op io = owned op *> unsafeEff_ io
{-# INLINE ownedIO #-}

-- | An operation that reads one of the interpreter's counters.
countOf :: (Ghostty :> es) => Text -> (Handles -> IORef Int) -> Eff es Int
countOf op counter = owned op >>= unsafeEff_ . readIORef . counter

-- | An operation that scopes a handle to an action: 'owned', then the
-- handle opened (from the interpreter's state), counted in the given
-- counter while it is open, and closed when the action ends, by return or
-- by exception.
scoped :: (Ghostty :> es) => Text -> (Handles -> IORef Int) -> (Handles -> IO h) -> (h -> IO ()) -> (h -> Eff es a) -> Eff es a
scoped op counter open close k = do
  hs <- owned op
  let count = modifyIORef' (counter hs)
  unsafeSeqUnliftIO $ \unlift ->
    bracket (open hs <* count (+ 1)) (\h -> close h <* count (subtract 1)) (unlift . k)

-- | Run the effect against the linked libghostty-vt, which must be the pinned
-- release, built with every optional feature (§3.4, D-1, D-2): otherwise
-- nothing runs, and the result is the mismatch. Every operation must then
-- come from this thread, or it throws 'WrongThread' (§4.6).
runGhostty :: (IOE :> es) => Eff (Ghostty : es) a -> Eff es (Either VersionMismatch a)
runGhostty = runGhosttyPinnedTo (decodeUtf8 ghosttyPinnedVersion)

-- | 'runGhostty' with another pin in place of @LIBGHOSTTY_VT_VERSION@, which is
-- how the mismatch is tested.
runGhosttyPinnedTo :: (IOE :> es) => Text -> Eff (Ghostty : es) a -> Eff es (Either VersionMismatch a)
runGhosttyPinnedTo pin act = do
  linked <- liftIO linkedVersion
  if linked /= Just pin
    then pure (Left VersionMismatch {pinned = pin, linked})
    else do
      owner <- liftIO myThreadId
      terminalCount <- newIORef 0
      refCount <- newIORef 0
      renderCount <- newIORef 0
      encoderCount <- newIORef 0
      Right <$> evalStaticRep (GhosttyRep Handles {owner, terminalCount, refCount, renderCount, encoderCount}) act

-- | The linked library's identity: its full version (@ghostty_version@, which
-- a development build suffixes with @-dev+…@), then @without@ each optional
-- feature it was built without (@-Dvt-features@, 'optionalFeatures'), whose
-- calls would return @GHOSTTY_UNSUPPORTED@. So it equals the pin exactly when
-- the library is the pinned release, whole. 'Nothing' if the library would
-- not say whether it has a feature. Build info is a constant of the library,
-- so this needs no owner.
linkedVersion :: IO (Maybe Text)
linkedVersion = do
  ConstPtr p <- ghosttyVersion
  version <- decodeUtf8 <$> BS.packCString p
  missing <- traverse absent optionalFeatures
  pure $ T.unwords . (version :) . concatMap (\m -> ["without", m]) . catMaybes <$> sequence missing
  where
    -- Whether the library was built with the feature: 'Nothing' if it
    -- would not say, else the feature's name if it was built without it.
    absent :: (GhosttyBuildInfo, Text) -> IO (Maybe (Maybe Text))
    absent (key, name) = alloca $ \(b :: Ptr CBool) -> do
      r <- ghosttyBuildInfo key (castPtr b)
      if r /= GhosttySuccess
        then pure Nothing
        else Just . (name <$) . guard . not . toBool <$> peek b

-- | Every optional feature @-Dvt-features@ can leave out, with its build-info
-- key and its name there: the nine @FEATURE_*@ keys, and @KITTY_GRAPHICS@,
-- whose terminal keys return @GHOSTTY_UNSUPPORTED@ in a build without it
-- (regression 12).
optionalFeatures :: [(GhosttyBuildInfo, Text)]
optionalFeatures =
  [ (GhosttyBuildInfoFeatureSnapshot, "snapshot"),
    (GhosttyBuildInfoFeatureFormatter, "formatter"),
    (GhosttyBuildInfoFeatureSelection, "selection"),
    (GhosttyBuildInfoFeatureSearch, "search"),
    (GhosttyBuildInfoFeatureRenderState, "render-state"),
    (GhosttyBuildInfoFeatureInputEncode, "input-encode"),
    (GhosttyBuildInfoFeatureColor, "color"),
    (GhosttyBuildInfoFeatureGridIntrospection, "grid-introspection"),
    (GhosttyBuildInfoFeatureGlyphProtocol, "glyph-protocol"),
    (GhosttyBuildInfoKittyGraphics, "kitty-graphics")
  ]
