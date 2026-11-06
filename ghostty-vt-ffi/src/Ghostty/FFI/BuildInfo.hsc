{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

-- | @include/ghostty/vt/build_info.h@: compile-time facts about the linked
-- library (DESIGN.md §3.4).
module Ghostty.FFI.BuildInfo
  ( -- * Keys
    GhosttyBuildInfo (..),
    pattern GhosttyBuildInfoVersionString,
    pattern GhosttyBuildInfoVersionBuild,
    ghosttyBuildInfoEnum,

    -- * Calls
    ghosttyBuildInfo,
  )
where

import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Ghostty.FFI.Layout (CEnum (..))
import Ghostty.FFI.Types (GhosttyResult (..))

#include <ghostty/vt.h>

-- | @GhosttyBuildInfo@: which fact to read. The out type depends on the key
-- and is given on each pattern.
newtype GhosttyBuildInfo = GhosttyBuildInfo CInt
  deriving stock (Eq, Show)

-- | @GHOSTTY_BUILD_INFO_VERSION_STRING@: the library's version, e.g.
-- @0.1.0-dev+\<commit\>@ as scripts/build-libghostty.sh stamps it. Out type:
-- @GhosttyString *@.
pattern GhosttyBuildInfoVersionString :: GhosttyBuildInfo
pattern GhosttyBuildInfoVersionString = GhosttyBuildInfo (#const GHOSTTY_BUILD_INFO_VERSION_STRING)

-- | @GHOSTTY_BUILD_INFO_VERSION_BUILD@: the version's build metadata, empty
-- unless the build was given one; the pinned commit, as built here. Out type:
-- @GhosttyString *@.
pattern GhosttyBuildInfoVersionBuild :: GhosttyBuildInfo
pattern GhosttyBuildInfoVersionBuild = GhosttyBuildInfo (#const GHOSTTY_BUILD_INFO_VERSION_BUILD)

-- | Every value of @GhosttyBuildInfo@ this binding names, for the layout test
-- (§3.4).
ghosttyBuildInfoEnum :: CEnum
ghosttyBuildInfoEnum =
  CEnum
    { name = "GhosttyBuildInfo",
      size = (#size GhosttyBuildInfo),
      values =
        [ ("GHOSTTY_BUILD_INFO_VERSION_STRING", (#const GHOSTTY_BUILD_INFO_VERSION_STRING)),
          ("GHOSTTY_BUILD_INFO_VERSION_BUILD", (#const GHOSTTY_BUILD_INFO_VERSION_BUILD))
        ]
    }

-- | @ghostty_build_info@. @unsafe@: it does not call back into Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_build_info"
  ghosttyBuildInfo :: GhosttyBuildInfo -> Ptr () -> IO GhosttyResult
