-- | The identity check of DESIGN.md §3.4: the linked library is the pinned
-- build. scripts/build-libghostty.sh stamps the pinned commit into the
-- library's version as semver build metadata, so build info's
-- @VERSION_BUILD@ must be exactly @GHOSTTY_COMMIT@.
module Test.Ghostty.FFI.BuildInfo (tests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import Ghostty.FFI.BuildInfo
import Ghostty.FFI.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "BuildInfo"
    [ testCase "build metadata is the pinned commit" $ do
        pinned <- T.strip . decodeUtf8 <$> readFileBS "GHOSTTY_COMMIT"
        build <- buildString GhosttyBuildInfoVersionBuild
        build @?= pinned,
      testCase "the version string carries the build metadata" $ do
        build <- buildString GhosttyBuildInfoVersionBuild
        version <- buildString GhosttyBuildInfoVersionString
        T.takeWhileEnd (/= '+') version @?= build
    ]

buildString :: GhosttyBuildInfo -> IO Text
buildString key = alloca $ \(p :: Ptr GhosttyString) -> do
  r <- ghosttyBuildInfo key (castPtr p)
  case r of
    GhosttySuccess -> do
      s <- peek p
      decodeUtf8 <$> BS.packCStringLen (castPtr s.ptr, fromIntegral s.len)
    _ -> assertFailure ("ghostty_build_info " <> show key <> ": " <> show r)
