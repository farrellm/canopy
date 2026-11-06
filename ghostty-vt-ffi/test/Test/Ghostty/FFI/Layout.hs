{-# LANGUAGE DuplicateRecordFields #-}

-- | The layout test of DESIGN.md §3.4: the binding's @hsc2hs@ values, from
-- the headers it was compiled against, agree with @ghostty_type_json()@ from
-- the library actually linked.
--
-- A struct or enum is added by adding its descriptor to 'structs' or 'enums'.
module Test.Ghostty.FFI.Layout (tests) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Foreign.C.ConstPtr (ConstPtr (..))
import Ghostty.FFI.BuildInfo (ghosttyBuildInfoEnum)
import Ghostty.FFI.Layout (CEnum (..), CField (..), CStruct (..))
import Ghostty.FFI.Types
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- | Every struct the binding marshals.
structs :: [CStruct]
structs =
  [ ghosttyStringLayout
  ]

-- | Every enum the binding names values of.
enums :: [CEnum]
enums =
  [ ghosttyResultEnum,
    ghosttyBuildInfoEnum
  ]

tests :: TestTree
tests =
  withResource loadManifest (const pass) $ \getManifest ->
    testGroup
      "Layout"
      [ testGroup "structs" [structCase getManifest s | s <- structs],
        testGroup "enums" [enumCase getManifest e | e <- enums],
        testCase "GhosttyResultUnknown excludes exactly the named values" $ do
          entry <- lookupType "GhosttyResult" =<< getManifest
          for_ (Map.elems entry.values) $ \v ->
            resultNamed (GhosttyResult (fromInteger v))
              @?= (v `elem` map snd ghosttyResultEnum.values)
      ]

-- | Whether a pattern other than the 'GhosttyResultUnknown' view matches.
-- Total over the @COMPLETE@ set, so a new pattern that is missing here fails
-- to compile, and one that is missing from 'ghosttyResultEnum' fails the test.
resultNamed :: GhosttyResult -> Bool
resultNamed = \case
  GhosttySuccess -> True
  GhosttyOutOfMemory -> True
  GhosttyInvalidValue -> True
  GhosttyOutOfSpace -> True
  GhosttyNoValue -> True
  GhosttyIoError -> True
  GhosttyLimitExceeded -> True
  GhosttyRejected -> True
  GhosttyResultUnknown _ -> False

structCase :: IO Manifest -> CStruct -> TestTree
structCase getManifest s = testCase (toString s.name) $ do
  entry <- lookupType s.name =<< getManifest
  entry.kind @?= "struct"
  (entry.size, entry.align) @?= (Just s.size, Just s.align)
  -- Every field of the library's struct, and no other, with its place.
  Map.keysSet entry.fields @?= Set.fromList (map (.name) s.fields)
  for_ s.fields $ \f ->
    (f.name, (\jf -> (jf.offset, jf.size)) <$> Map.lookup f.name entry.fields)
      @?= (f.name, Just (f.offset, f.size))

enumCase :: IO Manifest -> CEnum -> TestTree
enumCase getManifest e = testCase (toString e.name) $ do
  entry <- lookupType e.name =<< getManifest
  entry.kind @?= "enum"
  entry.size @?= Just e.size
  for_ e.values $ \(cName, v) -> do
    let key = T.stripPrefix (fromMaybe "" entry.prefix) cName
    (cName, (`Map.lookup` entry.values) =<< key) @?= (cName, Just v)

-- * The manifest

-- | The parts of @ghostty_type_json()@ the test reads.
newtype Manifest = Manifest {types :: Map Text Entry}

instance FromJSON Manifest where
  parseJSON = withObject "manifest" $ \o -> Manifest <$> o .: "types"

-- | One type. Fields absent for its kind are empty.
data Entry = Entry
  { kind :: Text,
    size :: Maybe Int,
    align :: Maybe Int,
    fields :: Map Text Field,
    prefix :: Maybe Text,
    values :: Map Text Integer
  }

instance FromJSON Entry where
  parseJSON = withObject "type" $ \o ->
    Entry
      <$> o .: "kind"
      <*> o .:? "size"
      <*> o .:? "align"
      <*> (fromMaybe mempty <$> o .:? "fields")
      <*> o .:? "prefix"
      <*> (fromMaybe mempty <$> o .:? "values")

data Field = Field {offset :: Int, size :: Int}

instance FromJSON Field where
  parseJSON = withObject "field" $ \o -> Field <$> o .: "offset" <*> o .: "size"

loadManifest :: IO Manifest
loadManifest = do
  ConstPtr p <- ghosttyTypeJson
  bytes <- BS.packCString p
  either (assertFailure . ("ghostty_type_json: " <>)) pure (Aeson.eitherDecodeStrict bytes)

lookupType :: Text -> Manifest -> IO Entry
lookupType name m =
  maybe (assertFailure ("ghostty_type_json has no type " <> toString name)) pure (Map.lookup name m.types)
