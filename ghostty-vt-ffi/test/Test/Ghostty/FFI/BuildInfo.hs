{-# LANGUAGE TemplateHaskell #-}

-- | The identity check of DESIGN.md §3.4: the linked library is the pinned
-- release, built whole. Its runtime version (@ghostty_version@) and the
-- headers' @GHOSTTY_VT_VERSION@ must both be exactly @LIBGHOSTTY_VT_VERSION@,
-- with no pre-release or build suffix, and every optional feature must be
-- compiled in.
module Test.Ghostty.FFI.BuildInfo (tests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import Ghostty.FFI.BuildInfo
import Ghostty.FFI.Types
import Language.Haskell.TH.Syntax (addDependentFile, makeRelativeToProject, runIO)
import Language.Haskell.TH.Syntax qualified as TH
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- | @LIBGHOSTTY_VT_VERSION@, read when this module is compiled, so the suite
-- checks the same pin wherever it is run from.
pinned :: Text
pinned =
  T.strip $
    toText
      ( $( do
             path <- makeRelativeToProject "LIBGHOSTTY_VT_VERSION"
             addDependentFile path
             bytes <- runIO (readFileBS path)
             TH.lift (decodeUtf8 bytes :: String)
         ) ::
          String
      )

-- | Every optional feature's key: the @FEATURE_*@ keys and @KITTY_GRAPHICS@.
-- A library built with @-Dvt-features@ leaving one out would turn the bound
-- calls of that feature into @GHOSTTY_UNSUPPORTED@.
features :: [GhosttyBuildInfo]
features =
  [ GhosttyBuildInfoFeatureSnapshot,
    GhosttyBuildInfoFeatureFormatter,
    GhosttyBuildInfoFeatureSelection,
    GhosttyBuildInfoFeatureSearch,
    GhosttyBuildInfoFeatureRenderState,
    GhosttyBuildInfoFeatureInputEncode,
    GhosttyBuildInfoFeatureColor,
    GhosttyBuildInfoFeatureGridIntrospection,
    GhosttyBuildInfoFeatureGlyphProtocol,
    GhosttyBuildInfoKittyGraphics
  ]

tests :: TestTree
tests =
  testGroup
    "BuildInfo"
    [ testCase "the linked library's version is the pin" $ do
        ConstPtr p <- ghosttyVersion
        version <- BS.packCString p
        decodeUtf8 version @?= pinned,
      testCase "the headers' version is the pin" $
        decodeUtf8 ghosttyVtVersion @?= pinned,
      testCase "the version string is the pin, with no suffix" $ do
        buildString GhosttyBuildInfoVersionString >>= (@?= pinned)
        buildString GhosttyBuildInfoVersionPre >>= (@?= "")
        buildString GhosttyBuildInfoVersionBuild >>= (@?= ""),
      testCase "every optional feature is compiled in" $
        for_ features $ \key -> do
          present <- buildBool key
          (key, present) @?= (key, 1)
    ]

buildString :: GhosttyBuildInfo -> IO Text
buildString key = alloca $ \(p :: Ptr GhosttyString) -> do
  r <- ghosttyBuildInfo key (castPtr p)
  case r of
    GhosttySuccess -> decodeUtf8 <$> (ghosttyStringBytes =<< peek p)
    _ -> assertFailure ("ghostty_build_info " <> show key <> ": " <> show r)

buildBool :: GhosttyBuildInfo -> IO CBool
buildBool key = alloca $ \(p :: Ptr CBool) -> do
  r <- ghosttyBuildInfo key (castPtr p)
  case r of
    GhosttySuccess -> peek p
    _ -> assertFailure ("ghostty_build_info " <> show key <> ": " <> show r)
