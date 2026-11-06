{-# LANGUAGE DuplicateRecordFields #-}

-- | The layout test of DESIGN.md §3.4: the binding's @hsc2hs@ values, from
-- the headers it was compiled against, agree with @ghostty_type_json()@ from
-- the library actually linked, and the binding agrees with itself.
--
-- A struct or enum is added by one row in 'structs' or 'enums'.
-- @scripts/check-layout-rows.py@ fails if a bound type has no row.
module Test.Ghostty.FFI.Layout (tests) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..))
import Ghostty.FFI.BuildInfo
import Ghostty.FFI.Layout (CEnum (..), CField (..), CStruct (..))
import Ghostty.FFI.Types
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- | A struct: its descriptor, and a sample value of its 'Storable' type with
-- every field non-zero and not all ones, for the round trip.
data StructRow = forall a. (Storable a, Eq a, Show a) => StructRow CStruct a

-- | An enum: its descriptor, and whether a pattern other than the @Unknown@
-- view matches a value. That function is a total match on the @COMPLETE@ set,
-- so a new pattern missing from it fails to compile, and one missing from the
-- descriptor fails the test.
data EnumRow = EnumRow CEnum (Integer -> Bool)

-- | Every struct the binding marshals.
structs :: [StructRow]
structs =
  [ StructRow ghosttyStringLayout (GhosttyString (nullPtr `plusPtr` 0x0102030405) 0x0a0b0c)
  ]

-- | Every enum the binding names values of.
enums :: [EnumRow]
enums =
  [ EnumRow ghosttyResultEnum $ \n -> case GhosttyResult (fromInteger n) of
      GhosttySuccess -> True
      GhosttyOutOfMemory -> True
      GhosttyInvalidValue -> True
      GhosttyOutOfSpace -> True
      GhosttyNoValue -> True
      GhosttyIoError -> True
      GhosttyLimitExceeded -> True
      GhosttyRejected -> True
      GhosttyResultUnknown _ -> False,
    EnumRow ghosttyBuildInfoEnum $ \n -> case GhosttyBuildInfo (fromInteger n) of
      GhosttyBuildInfoVersionString -> True
      GhosttyBuildInfoVersionBuild -> True
      GhosttyBuildInfoUnknown _ -> False
  ]

tests :: TestTree
tests =
  withResource loadManifest (const pass) $ \getManifest ->
    testGroup
      "Layout"
      [ testGroup "structs" [structCase getManifest r | r <- structs],
        testGroup "enums" [enumCase getManifest r | r <- enums]
      ]

structCase :: IO Manifest -> StructRow -> TestTree
structCase getManifest (StructRow s sample) =
  testGroup
    (toString s.name)
    [ testCase "matches the library" $ do
        entry <- lookupType s.name =<< getManifest
        entry.kind @?= "struct"
        (entry.size, entry.align) @?= (Just s.size, Just s.align)
        -- Every field of the library's struct, and no other, with its place.
        Map.keysSet entry.fields @?= Set.fromList (map (.name) s.fields)
        for_ s.fields $ \f ->
          (f.name, (\jf -> (jf.offset, jf.size)) <$> Map.lookup f.name entry.fields)
            @?= (f.name, Just (f.offset, f.size)),
      testCase "Storable agrees with the descriptor" $ do
        (sizeOf sample, alignment sample) @?= (s.size, s.align)
        -- Over zeroed and over all-ones memory, so a field that poke skips
        -- (or peek invents) cannot read back as the sample's.
        for_ [0x00, 0xff] $ \fill ->
          allocaBytesAligned s.size s.align $ \p -> do
            fillBytes p fill s.size
            poke (castPtr p) sample
            back <- peek (castPtr p)
            (fill, back) @?= (fill, sample)
    ]

enumCase :: IO Manifest -> EnumRow -> TestTree
enumCase getManifest (EnumRow e isNamed) =
  testGroup
    (toString e.name)
    [ testCase "matches the library" $ do
        entry <- lookupType e.name =<< getManifest
        entry.kind @?= "enum"
        entry.size @?= Just e.size
        for_ e.values $ \(cName, v) -> do
          let key = T.stripPrefix (fromMaybe "" entry.prefix) cName
          (cName, (`Map.lookup` entry.values) =<< key) @?= (cName, Just v),
      testCase "the Unknown view excludes exactly the named values" $ do
        entry <- lookupType e.name =<< getManifest
        for_ (Map.toList entry.values) $ \(key, v) ->
          (key, isNamed v) @?= (key, v `elem` map snd e.values)
    ]

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
