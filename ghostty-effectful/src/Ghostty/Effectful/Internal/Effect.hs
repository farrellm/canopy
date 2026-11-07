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
    linkedVersion,
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (throwIO)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool)
import Foreign.Marshal.Alloc (alloca)
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
    openCount :: IORef Int,
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
      open <- newIORef 0
      refs <- newIORef 0
      renders <- newIORef 0
      encoders <- newIORef 0
      Right <$> evalStaticRep (GhosttyRep Handles {owner, openCount = open, refCount = refs, renderCount = renders, encoderCount = encoders}) act

-- | The linked library's identity: its full version (@ghostty_version@, which
-- a development build suffixes with @-dev+…@), then @without@ each optional
-- feature it was built without (@-Dvt-features@), whose calls would return
-- @GHOSTTY_UNSUPPORTED@. So it equals the pin exactly when the library is the
-- pinned release, whole. 'Nothing' if the library would not say whether it
-- has a feature. Build info is a constant of the library, so this needs no
-- owner.
linkedVersion :: IO (Maybe Text)
linkedVersion = do
  ConstPtr p <- ghosttyVersion
  version <- decodeUtf8 <$> BS.packCString p
  missing <- fmap concat . sequence <$> traverse absent features
  pure $ (\ms -> T.unwords (version : concatMap (\m -> ["without", m]) ms)) <$> missing
  where
    -- The feature's name if the library was built without it.
    absent :: (GhosttyBuildInfo, Text) -> IO (Maybe [Text])
    absent (key, name) = alloca $ \(b :: Ptr CBool) -> do
      r <- ghosttyBuildInfo key (castPtr b)
      if r /= GhosttySuccess
        then pure Nothing
        else do
          present <- peek b
          pure (Just [name | present == 0])
    features :: [(GhosttyBuildInfo, Text)]
    features =
      [ (GhosttyBuildInfoFeatureSnapshot, "snapshot"),
        (GhosttyBuildInfoFeatureFormatter, "formatter"),
        (GhosttyBuildInfoFeatureSelection, "selection"),
        (GhosttyBuildInfoFeatureSearch, "search"),
        (GhosttyBuildInfoFeatureRenderState, "render-state"),
        (GhosttyBuildInfoFeatureInputEncode, "input-encode"),
        (GhosttyBuildInfoFeatureColor, "color"),
        (GhosttyBuildInfoFeatureGridIntrospection, "grid-introspection"),
        (GhosttyBuildInfoFeatureGlyphProtocol, "glyph-protocol")
      ]
