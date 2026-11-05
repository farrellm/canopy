{-# LANGUAGE PatternSynonyms #-}

-- | The high-level interface to libghostty-vt (DESIGN.md §4).
--
-- M0 skeleton: only the build identity, read through the raw binding, so the
-- package edge to @ghostty-vt-ffi@ and the link to the C library are wired.
module Ghostty.Effectful (buildMetadata) where

import Data.ByteString qualified as BS
import Effectful (Eff, IOE, (:>))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import Ghostty.FFI.BuildInfo (ghosttyBuildInfo, pattern GhosttyBuildInfoVersionBuild)
import Ghostty.FFI.Types (GhosttyString (..), pattern GhosttySuccess)

-- | The linked library's build metadata: the commit it was built from, or
-- 'Nothing' if the library would not say.
--
-- >>> import Effectful (runEff)
-- >>> isJust <$> runEff buildMetadata
-- True
buildMetadata :: (IOE :> es) => Eff es (Maybe Text)
buildMetadata = liftIO $ alloca $ \(p :: Ptr GhosttyString) -> do
  r <- ghosttyBuildInfo GhosttyBuildInfoVersionBuild (castPtr p)
  if r /= GhosttySuccess
    then pure Nothing
    else do
      s <- peek p
      Just . decodeUtf8 <$> BS.packCStringLen (castPtr s.ptr, fromIntegral s.len)
