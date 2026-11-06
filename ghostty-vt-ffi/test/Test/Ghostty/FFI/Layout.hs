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
import Foreign.Ptr (FunPtr, Ptr, castPtr, castPtrToFunPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..))
import Ghostty.FFI.BuildInfo
import Ghostty.FFI.Color
import Ghostty.FFI.Device
import Ghostty.FFI.GridRef
import Ghostty.FFI.Io
import Ghostty.FFI.Layout (CEnum (..), CField (..), CStruct (..))
import Ghostty.FFI.Modes
import Ghostty.FFI.Osc
import Ghostty.FFI.Point
import Ghostty.FFI.SizeReport
import Ghostty.FFI.Style
import Ghostty.FFI.Terminal
import Ghostty.FFI.Terminal.Callbacks
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
  [ StructRow ghosttyStringLayout (string 0x0102030405),
    -- color.h, style.h
    StructRow ghosttyColorRgbLayout aRgb,
    StructRow ghosttyStyleColorLayout (StyleColorRgb aRgb),
    StructRow ghosttyStyleLayout $
      GhosttyStyle 0x48 (StyleColorPalette 7) (StyleColorRgb aRgb) (StyleColorPalette 9) 1 1 1 1 1 1 1 1 2,
    -- point.h, grid_ref.h
    StructRow ghosttyPointCoordinateLayout aCoordinate,
    StructRow ghosttyPointLayout (GhosttyPoint GhosttyPointTagScreen aCoordinate),
    StructRow ghosttyGridRefLayout (GhosttyGridRef 0x18 (addr 0x0102030405) 0x0a0b 0x0c0d),
    -- device.h, size_report.h, io.h
    StructRow ghosttyDeviceAttributesPrimaryLayout aPrimary,
    StructRow ghosttyDeviceAttributesSecondaryLayout aSecondary,
    StructRow ghosttyDeviceAttributesTertiaryLayout aTertiary,
    StructRow ghosttyDeviceAttributesLayout (GhosttyDeviceAttributes aPrimary aSecondary aTertiary),
    StructRow ghosttySizeReportSizeLayout (GhosttySizeReportSize 24 80 0x0a0b0c 0x0d0e0f),
    StructRow ghosttyWriterLayout (GhosttyWriter (funAddr 0x0102030405) (addr 0x060708090a)),
    -- terminal.h
    StructRow ghosttyTerminalScrollbarLayout (GhosttyTerminalScrollbar 0x0102030405 0x060708 0x090a),
    StructRow ghosttyTerminalMemoryUsageLayout $
      GhosttyTerminalMemoryUsage 0x70 1 1 2 3 4 5 6 7 8 9 10 11 12,
    StructRow ghosttyTerminalModeConfigLayout (GhosttyTerminalModeConfig GhosttyModeBracketedPaste 1),
    StructRow ghosttyTerminalWriteStopLayout $
      GhosttyTerminalWriteStop
        0x28
        0x0102030405
        0x40
        GhosttyTerminalStringKindOsc
        GhosttyTerminalStringIntroducerC1
        GhosttyTerminalStringEndBel
        0x07,
    StructRow ghosttyTerminalScrollViewportLayout (ScrollViewportRow 0x0102030405),
    -- terminal.h, callbacks
    StructRow ghosttyTerminalUnknownStringSequenceLayout aStringSequence,
    StructRow ghosttyTerminalUnknownOscSequenceLayout anOscSequence,
    StructRow ghosttyTerminalUnknownSequenceLayout (UnknownSequenceOsc anOscSequence),
    StructRow ghosttyClipboardContentLayout aContent,
    StructRow ghosttyClipboardWriteReplyLayout (GhosttyClipboardWriteReply 0x10 GhosttyClipboardWriteResultBusy 1),
    StructRow ghosttyClipboardWriteLayout $
      GhosttyClipboardWrite
        0x48
        GhosttyClipboardLocationPrimary
        (addr 0x0102030405)
        3
        (string 0x060708090a)
        1
        1
        (addr 0x0b0c0d0e0f)
        (funAddr 0x1011121314),
    StructRow ghosttyClipboardReadReplyLayout $
      GhosttyClipboardReadReply
        0x38
        GhosttyClipboardReadResultDenied
        (addr 0x0102030405)
        2
        (addr 0x060708090a)
        3
        1,
    StructRow ghosttyClipboardReadLayout $
      GhosttyClipboardRead
        0x50
        GhosttyClipboardLocationSelection
        (addr 0x0102030405)
        2
        1
        (string 0x060708090a)
        1
        1
        (addr 0x0b0c0d0e0f)
        (funAddr 0x1011121314),
    StructRow ghosttyTerminalDesktopNotificationLayout $
      GhosttyTerminalDesktopNotification 0x28 (string 0x0102030405) (string 0x060708090a),
    StructRow ghosttyTerminalProgressReportLayout $
      GhosttyTerminalProgressReport 0x10 GhosttyTerminalProgressStateIndeterminate 42,
    StructRow ghosttyTerminalProgramStatusLayout $
      GhosttyTerminalProgramStatus
        0x58
        GhosttyProgramStatusStateBlocked
        GhosttyProgramStatusKindAuth
        42
        (string 0x0102030405)
        (string 0x060708090a)
        (string 0x0b0c0d0e0f)
        (string 0x1011121314),
    StructRow ghosttyTerminalSemanticPromptLayout $
      GhosttyTerminalSemanticPrompt
        0x38
        GhosttySemanticPromptCommandEnd
        GhosttySemanticPromptPromptSecondary
        1
        (-2)
        (string 0x0102030405)
        (string 0x060708090a),
    StructRow ghosttyTerminalResizeWindowGridLayout aGrid,
    StructRow ghosttyTerminalResizeWindowLayout (ResizeWindowGrid aGrid)
  ]
  where
    aRgb = GhosttyColorRgb 1 2 3
    aCoordinate = GhosttyPointCoordinate 0x0a0b 0x0c0d0e0f
    aPrimary = GhosttyDeviceAttributesPrimary 62 [1 .. 64] 3
    aSecondary = GhosttyDeviceAttributesSecondary 1 0x0a0b 0x0c0d
    aTertiary = GhosttyDeviceAttributesTertiary 0x0a0b0c0d
    aStringSequence = GhosttyTerminalUnknownStringSequence 1 (string 0x0102030405)
    anOscSequence = GhosttyTerminalUnknownOscSequence 1 (string 0x0102030405) GhosttyOscTerminatorBel
    aContent = GhosttyClipboardContent (string 0x0102030405) (string 0x060708090a)
    aGrid = GhosttyTerminalResizeWindowGrid 24 80

-- | A pointer sample: an address no field could hold by accident.
addr :: Int -> Ptr a
addr = plusPtr nullPtr

-- | A function pointer sample, as 'addr'.
funAddr :: Int -> FunPtr a
funAddr = castPtrToFunPtr . addr

-- | A string sample whose length differs from its address.
string :: Int -> GhosttyString
string n = GhosttyString (addr n) (fromIntegral (n `div` 0x100))

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
      GhosttyBuildInfoUnknown _ -> False,
    EnumRow ghosttyOscTerminatorEnum $ \n -> case GhosttyOscTerminator (fromInteger n) of
      GhosttyOscTerminatorSt -> True
      GhosttyOscTerminatorBel -> True
      GhosttyOscTerminatorUnknown _ -> False,
    EnumRow ghosttyPointTagEnum $ \n -> case GhosttyPointTag (fromInteger n) of
      GhosttyPointTagActive -> True
      GhosttyPointTagViewport -> True
      GhosttyPointTagScreen -> True
      GhosttyPointTagHistory -> True
      GhosttyPointTagUnknown _ -> False,
    EnumRow ghosttyStyleColorTagEnum $ \n -> case GhosttyStyleColorTag (fromInteger n) of
      GhosttyStyleColorNone -> True
      GhosttyStyleColorPalette -> True
      GhosttyStyleColorRgb -> True
      GhosttyStyleColorTagUnknown _ -> False,
    EnumRow ghosttyColorSchemeEnum $ \n -> case GhosttyColorScheme (fromInteger n) of
      GhosttyColorSchemeLight -> True
      GhosttyColorSchemeDark -> True
      GhosttyColorSchemeUnknown _ -> False,
    EnumRow ghosttyTerminalOptionEnum $ \n -> case GhosttyTerminalOption (fromInteger n) of
      GhosttyTerminalOptUserdata -> True
      GhosttyTerminalOptWritePty -> True
      GhosttyTerminalOptBell -> True
      GhosttyTerminalOptEnquiry -> True
      GhosttyTerminalOptXtversion -> True
      GhosttyTerminalOptTitleChanged -> True
      GhosttyTerminalOptSize -> True
      GhosttyTerminalOptColorScheme -> True
      GhosttyTerminalOptDeviceAttributes -> True
      GhosttyTerminalOptTitle -> True
      GhosttyTerminalOptPwd -> True
      GhosttyTerminalOptColorForeground -> True
      GhosttyTerminalOptColorBackground -> True
      GhosttyTerminalOptColorCursor -> True
      GhosttyTerminalOptColorPalette -> True
      GhosttyTerminalOptKittyImageStorageLimit -> True
      GhosttyTerminalOptKittyImageMediumFile -> True
      GhosttyTerminalOptKittyImageMediumTempFile -> True
      GhosttyTerminalOptKittyImageMediumSharedMem -> True
      GhosttyTerminalOptApcMaxBytes -> True
      GhosttyTerminalOptApcMaxBytesKitty -> True
      GhosttyTerminalOptSelection -> True
      GhosttyTerminalOptDefaultCursorStyle -> True
      GhosttyTerminalOptDefaultCursorBlink -> True
      GhosttyTerminalOptGlyphProtocol -> True
      GhosttyTerminalOptPwdChanged -> True
      GhosttyTerminalOptClipboardWrite -> True
      GhosttyTerminalOptScrollbackMaxBytes -> True
      GhosttyTerminalOptScrollbackMaxLines -> True
      GhosttyTerminalOptDesktopNotification -> True
      GhosttyTerminalOptProgressReport -> True
      GhosttyTerminalOptContinuationMaxBytes -> True
      GhosttyTerminalOptTitleReport -> True
      GhosttyTerminalOptModeDefault -> True
      GhosttyTerminalOptMode -> True
      GhosttyTerminalOptUnknownSequence -> True
      GhosttyTerminalOptUnknownMaxBytes -> True
      GhosttyTerminalOptTerminfoName -> True
      GhosttyTerminalOptClipboardRead -> True
      GhosttyTerminalOptClipboardWriteMaxBytes -> True
      GhosttyTerminalOptResizePullScrollback -> True
      GhosttyTerminalOptRenderHold -> True
      GhosttyTerminalOptSemanticPrompt -> True
      GhosttyTerminalOptReset -> True
      GhosttyTerminalOptXtChecksumReport -> True
      GhosttyTerminalOptXtChecksumExtension -> True
      GhosttyTerminalOptProgramStatus -> True
      GhosttyTerminalOptResizeWindow -> True
      GhosttyTerminalOptionUnknown _ -> False,
    EnumRow ghosttyTerminalDataEnum $ \n -> case GhosttyTerminalData (fromInteger n) of
      GhosttyTerminalDataInvalid -> True
      GhosttyTerminalDataCols -> True
      GhosttyTerminalDataRows -> True
      GhosttyTerminalDataCursorX -> True
      GhosttyTerminalDataCursorY -> True
      GhosttyTerminalDataCursorPendingWrap -> True
      GhosttyTerminalDataActiveScreen -> True
      GhosttyTerminalDataCursorVisible -> True
      GhosttyTerminalDataKittyKeyboardFlags -> True
      GhosttyTerminalDataScrollbar -> True
      GhosttyTerminalDataCursorStyle -> True
      GhosttyTerminalDataMouseTracking -> True
      GhosttyTerminalDataTitle -> True
      GhosttyTerminalDataPwd -> True
      GhosttyTerminalDataTotalRows -> True
      GhosttyTerminalDataScrollbackRows -> True
      GhosttyTerminalDataWidthPx -> True
      GhosttyTerminalDataHeightPx -> True
      GhosttyTerminalDataColorForeground -> True
      GhosttyTerminalDataColorBackground -> True
      GhosttyTerminalDataColorCursor -> True
      GhosttyTerminalDataColorPalette -> True
      GhosttyTerminalDataColorForegroundDefault -> True
      GhosttyTerminalDataColorBackgroundDefault -> True
      GhosttyTerminalDataColorCursorDefault -> True
      GhosttyTerminalDataColorPaletteDefault -> True
      GhosttyTerminalDataKittyImageStorageLimit -> True
      GhosttyTerminalDataKittyImageMediumFile -> True
      GhosttyTerminalDataKittyImageMediumTempFile -> True
      GhosttyTerminalDataKittyImageMediumSharedMem -> True
      GhosttyTerminalDataKittyGraphics -> True
      GhosttyTerminalDataSelection -> True
      GhosttyTerminalDataViewportActive -> True
      GhosttyTerminalDataVtProcessingError -> True
      GhosttyTerminalDataScrollbackMaxBytes -> True
      GhosttyTerminalDataScrollbackMaxLines -> True
      GhosttyTerminalDataContinuationMaxBytes -> True
      GhosttyTerminalDataMode -> True
      GhosttyTerminalDataVtGround -> True
      GhosttyTerminalDataCursorAtPrompt -> True
      GhosttyTerminalDataClipboardWriteMaxBytes -> True
      GhosttyTerminalDataMouseShape -> True
      GhosttyTerminalDataMemoryUsage -> True
      GhosttyTerminalDataUnknown _ -> False,
    EnumRow ghosttyTerminalScreenEnum $ \n -> case GhosttyTerminalScreen (fromInteger n) of
      GhosttyTerminalScreenPrimary -> True
      GhosttyTerminalScreenAlternate -> True
      GhosttyTerminalScreenUnknown _ -> False,
    EnumRow ghosttyTerminalCursorStyleEnum $ \n -> case GhosttyTerminalCursorStyle (fromInteger n) of
      GhosttyTerminalCursorStyleBar -> True
      GhosttyTerminalCursorStyleBlock -> True
      GhosttyTerminalCursorStyleUnderline -> True
      GhosttyTerminalCursorStyleBlockHollow -> True
      GhosttyTerminalCursorStyleUnknown _ -> False,
    EnumRow ghosttyTerminalBoundaryEnum $ \n -> case GhosttyTerminalBoundary (fromInteger n) of
      GhosttyTerminalBoundaryGround -> True
      GhosttyTerminalBoundaryApcStart -> True
      GhosttyTerminalBoundaryApcEnd -> True
      GhosttyTerminalBoundaryDcsStart -> True
      GhosttyTerminalBoundaryDcsEnd -> True
      GhosttyTerminalBoundaryOscStart -> True
      GhosttyTerminalBoundaryOscEnd -> True
      GhosttyTerminalBoundaryUnknown _ -> False,
    EnumRow ghosttyTerminalStringKindEnum $ \n -> case GhosttyTerminalStringKind (fromInteger n) of
      GhosttyTerminalStringKindApc -> True
      GhosttyTerminalStringKindDcs -> True
      GhosttyTerminalStringKindOsc -> True
      GhosttyTerminalStringKindUnknown _ -> False,
    EnumRow ghosttyTerminalStringIntroducerEnum $ \n -> case GhosttyTerminalStringIntroducer (fromInteger n) of
      GhosttyTerminalStringIntroducerEsc -> True
      GhosttyTerminalStringIntroducerC1 -> True
      GhosttyTerminalStringIntroducerUnknown _ -> False,
    EnumRow ghosttyTerminalStringEndEnum $ \n -> case GhosttyTerminalStringEnd (fromInteger n) of
      GhosttyTerminalStringEndStEsc -> True
      GhosttyTerminalStringEndStC1 -> True
      GhosttyTerminalStringEndCancelled -> True
      GhosttyTerminalStringEndAborted -> True
      GhosttyTerminalStringEndBel -> True
      GhosttyTerminalStringEndUnknown _ -> False,
    EnumRow ghosttyTerminalScrollViewportTagEnum $ \n -> case GhosttyTerminalScrollViewportTag (fromInteger n) of
      GhosttyScrollViewportTop -> True
      GhosttyScrollViewportBottom -> True
      GhosttyScrollViewportDelta -> True
      GhosttyScrollViewportRow -> True
      GhosttyScrollViewportDeltaPrompt -> True
      GhosttyTerminalScrollViewportTagUnknown _ -> False,
    EnumRow ghosttyTerminalCompressionModeEnum $ \n -> case GhosttyTerminalCompressionMode (fromInteger n) of
      GhosttyTerminalCompressionModeIncremental -> True
      GhosttyTerminalCompressionModeFull -> True
      GhosttyTerminalCompressionModeUnknown _ -> False,
    EnumRow ghosttyTerminalCompressionResultEnum $ \n -> case GhosttyTerminalCompressionResult (fromInteger n) of
      GhosttyTerminalCompressionResultUnsupported -> True
      GhosttyTerminalCompressionResultPending -> True
      GhosttyTerminalCompressionResultComplete -> True
      GhosttyTerminalCompressionResultUnknown _ -> False,
    EnumRow ghosttyTerminalUnknownSequenceTagEnum $ \n -> case GhosttyTerminalUnknownSequenceTag (fromInteger n) of
      GhosttyTerminalUnknownSequenceApc -> True
      GhosttyTerminalUnknownSequenceOsc -> True
      GhosttyTerminalUnknownSequenceTagUnknown _ -> False,
    EnumRow ghosttyClipboardLocationEnum $ \n -> case GhosttyClipboardLocation (fromInteger n) of
      GhosttyClipboardLocationStandard -> True
      GhosttyClipboardLocationSelection -> True
      GhosttyClipboardLocationPrimary -> True
      GhosttyClipboardLocationUnknown _ -> False,
    EnumRow ghosttyClipboardWriteResultEnum $ \n -> case GhosttyClipboardWriteResult (fromInteger n) of
      GhosttyClipboardWriteResultSuccess -> True
      GhosttyClipboardWriteResultDenied -> True
      GhosttyClipboardWriteResultUnsupported -> True
      GhosttyClipboardWriteResultBusy -> True
      GhosttyClipboardWriteResultInvalidData -> True
      GhosttyClipboardWriteResultIoError -> True
      GhosttyClipboardWriteResultUnknown _ -> False,
    EnumRow ghosttyClipboardReadResultEnum $ \n -> case GhosttyClipboardReadResult (fromInteger n) of
      GhosttyClipboardReadResultSuccess -> True
      GhosttyClipboardReadResultDenied -> True
      GhosttyClipboardReadResultUnsupported -> True
      GhosttyClipboardReadResultBusy -> True
      GhosttyClipboardReadResultIoError -> True
      GhosttyClipboardReadResultUnknown _ -> False,
    EnumRow ghosttyTerminalProgressStateEnum $ \n -> case GhosttyTerminalProgressState (fromInteger n) of
      GhosttyTerminalProgressStateRemove -> True
      GhosttyTerminalProgressStateSet -> True
      GhosttyTerminalProgressStateError -> True
      GhosttyTerminalProgressStateIndeterminate -> True
      GhosttyTerminalProgressStatePause -> True
      GhosttyTerminalProgressStateUnknown _ -> False,
    EnumRow ghosttyProgramStatusStateEnum $ \n -> case GhosttyProgramStatusState (fromInteger n) of
      GhosttyProgramStatusStateIdle -> True
      GhosttyProgramStatusStateWorking -> True
      GhosttyProgramStatusStateDone -> True
      GhosttyProgramStatusStateBlocked -> True
      GhosttyProgramStatusStateError -> True
      GhosttyProgramStatusStateClear -> True
      GhosttyProgramStatusStateUnknown _ -> False,
    EnumRow ghosttyProgramStatusKindEnum $ \n -> case GhosttyProgramStatusKind (fromInteger n) of
      GhosttyProgramStatusKindNone -> True
      GhosttyProgramStatusKindPermission -> True
      GhosttyProgramStatusKindQuestion -> True
      GhosttyProgramStatusKindAuth -> True
      GhosttyProgramStatusKindUnknown _ -> False,
    EnumRow ghosttySemanticPromptKindEnum $ \n -> case GhosttySemanticPromptKind (fromInteger n) of
      GhosttySemanticPromptInvalid -> True
      GhosttySemanticPromptPromptStart -> True
      GhosttySemanticPromptInputStart -> True
      GhosttySemanticPromptOutputStart -> True
      GhosttySemanticPromptCommandEnd -> True
      GhosttySemanticPromptKindUnknown _ -> False,
    EnumRow ghosttySemanticPromptPromptKindEnum $ \n -> case GhosttySemanticPromptPromptKind (fromInteger n) of
      GhosttySemanticPromptPromptPrimary -> True
      GhosttySemanticPromptPromptRight -> True
      GhosttySemanticPromptPromptContinuation -> True
      GhosttySemanticPromptPromptSecondary -> True
      GhosttySemanticPromptPromptKindUnknown _ -> False,
    EnumRow ghosttyTerminalResizeWindowTagEnum $ \n -> case GhosttyTerminalResizeWindowTag (fromInteger n) of
      GhosttyTerminalResizeWindowGridTag -> True
      GhosttyTerminalResizeWindowTagUnknown _ -> False
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
