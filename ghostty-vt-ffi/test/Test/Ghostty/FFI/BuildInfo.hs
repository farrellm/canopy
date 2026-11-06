{-# LANGUAGE TemplateHaskell #-}

-- | The identity check of DESIGN.md §3.4: the linked library is the pinned
-- build. scripts/build-libghostty.sh stamps the pinned commit into the
-- library's version as semver build metadata, so build info's
-- @VERSION_BUILD@ must be exactly @GHOSTTY_COMMIT@.
module Test.Ghostty.FFI.BuildInfo (tests) where

import Data.Text qualified as T
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import Ghostty.FFI.BuildInfo
import Ghostty.FFI.Types
import Language.Haskell.TH.Syntax (addDependentFile, makeRelativeToProject, runIO)
import Language.Haskell.TH.Syntax qualified as TH
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- | @GHOSTTY_COMMIT@, read when this module is compiled, so the suite checks
-- the same pin wherever it is run from.
pinned :: Text
pinned =
  T.strip $
    toText
      ( $( do
             path <- makeRelativeToProject "GHOSTTY_COMMIT"
             addDependentFile path
             bytes <- runIO (readFileBS path)
             TH.lift (decodeUtf8 bytes :: String)
         ) ::
          String
      )

tests :: TestTree
tests =
  testGroup
    "BuildInfo"
    [ testCase "build metadata is the pinned commit" $ do
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
    GhosttySuccess -> decodeUtf8 <$> (ghosttyStringBytes =<< peek p)
    _ -> assertFailure ("ghostty_build_info " <> show key <> ": " <> show r)
