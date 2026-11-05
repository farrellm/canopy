{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PatternSynonyms #-}

-- | @include/ghostty/vt/build_info.h@: compile-time facts about the linked
-- library (DESIGN.md §3.4).
module Ghostty.FFI.BuildInfo
  ( -- * Keys
    GhosttyBuildInfo (..),
    pattern GhosttyBuildInfoVersionString,
    pattern GhosttyBuildInfoVersionBuild,

    -- * Calls
    ghosttyBuildInfo,
  )
where

import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Ghostty.FFI.Types (GhosttyResult (..))

#include <ghostty/vt.h>

-- | @GhosttyBuildInfo@: which fact to read. The out type depends on the key
-- and is given on each pattern.
newtype GhosttyBuildInfo = GhosttyBuildInfo CInt
  deriving stock (Eq, Show)

-- | @GHOSTTY_BUILD_INFO_VERSION_STRING@: the Ghostty application version, e.g.
-- @1.4.1-dev+c7f68a7f@. Out type: @GhosttyString *@.
pattern GhosttyBuildInfoVersionString :: GhosttyBuildInfo
pattern GhosttyBuildInfoVersionString = GhosttyBuildInfo (#const GHOSTTY_BUILD_INFO_VERSION_STRING)

-- | @GHOSTTY_BUILD_INFO_VERSION_BUILD@: the build metadata, a commit hash.
-- Out type: @GhosttyString *@.
pattern GhosttyBuildInfoVersionBuild :: GhosttyBuildInfo
pattern GhosttyBuildInfoVersionBuild = GhosttyBuildInfo (#const GHOSTTY_BUILD_INFO_VERSION_BUILD)

-- | @ghostty_build_info@. @unsafe@: it does not call back into Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_build_info"
  ghosttyBuildInfo :: GhosttyBuildInfo -> Ptr () -> IO GhosttyResult
