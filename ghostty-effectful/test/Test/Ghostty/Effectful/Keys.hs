{-# LANGUAGE PatternSynonyms #-}

-- | Typed keys (PLAN.md M1.5, DESIGN.md §4.4): every 'TerminalData'
-- constructor read on a fresh terminal, every 'TerminalOption' written (and
-- read back where a key reads it), the keys held against the header's
-- tables, and the mode list and its index held against the library.
module Test.Ghostty.Effectful.Keys (tests) where

import Data.Char (isUpper, toUpper)
import Data.List (stripPrefix)
import Effectful (Eff, IOE, runEff)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Foreign.Storable (poke)
import Ghostty.Effectful (Ghostty, runGhostty)
import Ghostty.Effectful.Color
import Ghostty.Effectful.Internal.Convert (modeCode)
import Ghostty.Effectful.Internal.Keys (dataKey, optionKey)
import Ghostty.Effectful.Internal.Terminal (Extent (..), Terminal (..))
import Ghostty.Effectful.Mode
import Ghostty.Effectful.Style (defaultStyle)
import Ghostty.Effectful.Terminal
import Ghostty.FFI.Layout (CEnum (..))
import Ghostty.FFI.Modes (GhosttyMode, ghosttyModeNew)
import Ghostty.FFI.Terminal
  ( GhosttyTerminalData (..),
    GhosttyTerminalModeConfig (..),
    GhosttyTerminalOption (..),
    ghosttyTerminalDataEnum,
    ghosttyTerminalGet,
    ghosttyTerminalOptionEnum,
    ghosttyTerminalSet,
    pattern GhosttyTerminalDataKittyGraphics,
    pattern GhosttyTerminalDataMode,
    pattern GhosttyTerminalDataSelection,
    pattern GhosttyTerminalOptBell,
    pattern GhosttyTerminalOptClipboardRead,
    pattern GhosttyTerminalOptClipboardWrite,
    pattern GhosttyTerminalOptColorScheme,
    pattern GhosttyTerminalOptDesktopNotification,
    pattern GhosttyTerminalOptDeviceAttributes,
    pattern GhosttyTerminalOptEnquiry,
    pattern GhosttyTerminalOptModeDefault,
    pattern GhosttyTerminalOptProgramStatus,
    pattern GhosttyTerminalOptProgressReport,
    pattern GhosttyTerminalOptPwdChanged,
    pattern GhosttyTerminalOptRenderHold,
    pattern GhosttyTerminalOptReset,
    pattern GhosttyTerminalOptResizeWindow,
    pattern GhosttyTerminalOptSelection,
    pattern GhosttyTerminalOptSemanticPrompt,
    pattern GhosttyTerminalOptSize,
    pattern GhosttyTerminalOptTitleChanged,
    pattern GhosttyTerminalOptUnknownSequence,
    pattern GhosttyTerminalOptUserdata,
    pattern GhosttyTerminalOptWritePty,
    pattern GhosttyTerminalOptXtversion,
  )
import Ghostty.FFI.Types (pattern GhosttyInvalidValue, pattern GhosttySuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- | Some key, its type forgotten.
data Some f where
  Some :: f a -> Some f

tests :: TestTree
tests =
  testGroup
    "Keys"
    [ testCase "every data key reads its fresh-terminal value" $ do
        failures <- ghostty $ withTerminal config $ \t ->
          concat <$> traverse (\(Some k) -> fresh t k) allData
        failures @?= [],
      testCase "every data key is a constructor, or deferred" $
        sort (ordNub (map (\(Some k) -> dataNumber (dataKey k)) allData) <> map snd deferredData)
          @?= sort [v | (n :: Text, v) <- ghosttyTerminalDataEnum.values, n /= "GHOSTTY_TERMINAL_DATA_INVALID"],
      testCase "every data key is its header row's name" $
        filter (\(c, h) -> h /= Just ("GHOSTTY_TERMINAL_DATA_" <> c)) (map (\(Some k) -> (headerName (show k), rowName ghosttyTerminalDataEnum (dataNumber (dataKey k)))) allData)
          @?= [],
      testCase "every option writes, and reads back where a key reads it" $ do
        failures <- ghostty $ withTerminal config $ \t ->
          concat <$> traverse (\(Some o) -> written t o) allOptions
        failures @?= [],
      testCase "every option is a constructor, or deferred" $
        sort (ordNub (map (\(Some o) -> optionNumber (optionKey o)) allOptions) <> map snd deferredOptions)
          @?= sort (map snd ghosttyTerminalOptionEnum.values),
      testCase "every option is its header row's name" $
        filter (\(c, h) -> h /= Just ("GHOSTTY_TERMINAL_OPT_" <> c)) (map (\(Some o) -> (headerName (fromMaybe "" (stripPrefix "Set" (show o))), rowName ghosttyTerminalOptionEnum (optionNumber (optionKey o)))) allOptions)
          @?= [],
      testCase "the modes are exactly the ones the library recognises" $ do
        recognised <- ghostty $ withTerminal config $ \t ->
          liftIO (filterM (recognises t) [ghosttyModeNew n a | n <- [0 .. 0x7fff], a <- [0, 1]])
        sort recognised @?= sort (map (\(SomeModeKey m) -> modeCode m) allModes),
      testCase "a mode's index says whether the library lets its default be set" $ do
        wrong <- ghostty $ withTerminal config $ \t ->
          liftIO (filterM (\m -> (/=) (configurable m) <$> defaultSettable t m) allModes)
        map show wrong @?= ([] :: [String]),
      testCase "a palette has a colour at every index" $ do
        p <- maybe (assertFailure "not 256") pure (paletteFromList (map (\i -> Rgb i i i) universe))
        paletteColor (setPaletteColor 7 (Rgb 1 2 3) p) 7 @?= Rgb 1 2 3
        paletteColor p 255 @?= Rgb 255 255 255
    ]

-- | Run under the effect, failing the test on a version mismatch.
ghostty :: Eff '[Ghostty, IOE] a -> IO a
ghostty act = runEff (runGhostty act) >>= either (assertFailure . show) pure

-- | An 80×24 terminal.
config :: TerminalConfig
config = terminalConfig GridSize {cols = Extent 80, rows = Extent 24}

-- | A constructor's name as the header spells its row: the first word of
-- its 'show', in upper snake case (@CursorX@ is @CURSOR_X@).
headerName :: String -> Text
headerName = toText . go . takeWhile (/= ' ')
  where
    go (c : cs) = toUpper c : concatMap (\x -> if isUpper x then ['_', x] else [toUpper x]) cs
    go [] = []

-- | The header's name for a row's number.
rowName :: CEnum -> Integer -> Maybe Text
rowName e n = fst <$> find ((== n) . snd) e.values

-- * Data

-- | Every data key: each constructor once, and 'Mode' at every mode.
allData :: [Some TerminalData]
allData =
  [ Some Cols,
    Some Rows,
    Some CursorX,
    Some CursorY,
    Some CursorPendingWrap,
    Some ActiveScreen,
    Some CursorVisible,
    Some KittyKeyboardFlags,
    Some Scrollbar,
    Some CursorStyle,
    Some MouseTracking,
    Some Title,
    Some Pwd,
    Some TotalRows,
    Some ScrollbackRows,
    Some WidthPx,
    Some HeightPx,
    Some ColorForeground,
    Some ColorBackground,
    Some ColorCursor,
    Some ColorPalette,
    Some ColorForegroundDefault,
    Some ColorBackgroundDefault,
    Some ColorCursorDefault,
    Some ColorPaletteDefault,
    Some KittyImageStorageLimit,
    Some KittyImageMediumFile,
    Some KittyImageMediumTempFile,
    Some KittyImageMediumSharedMem,
    Some ViewportActive,
    Some VtProcessingError,
    Some ScrollbackMaxBytes,
    Some ScrollbackMaxLines,
    Some ContinuationMaxBytes,
    Some VtGround,
    Some CursorAtPrompt,
    Some ClipboardWriteMaxBytes,
    Some MouseShape,
    Some MemoryUsage
  ]
    <> [Some (Mode m) | SomeModeKey m <- allModes]

-- | The header's data keys with no constructor, and why (DESIGN.md §4.4).
deferredData :: [(Text, Integer)]
deferredData =
  [ ("KITTY_GRAPHICS: a borrowed handle, PLAN.md M6.15", dataNumber GhosttyTerminalDataKittyGraphics),
    ("SELECTION: grid references, PLAN.md M6.5", dataNumber GhosttyTerminalDataSelection)
  ]

-- | A key's number.
dataNumber :: GhosttyTerminalData -> Integer
dataNumber (GhosttyTerminalData n) = toInteger n

-- | Read a key on a fresh 80×24 terminal and compare it with what the header
-- and the library's defaults say it is. The match is total, so a new
-- constructor fails to compile until it is given an expectation here (and a
-- place in 'allData', which the key-coverage test requires).
fresh :: Terminal s -> TerminalData a -> Eff '[Ghostty, IOE] [String]
fresh t k = do
  v <- getT t k
  let is :: (Eq b, Show b) => b -> b -> [String]
      is want got = [show k <> ": " <> show got <> ", expected " <> show want | want /= got]
      holds :: (Show b) => (b -> Bool) -> b -> [String]
      holds p got = [show k <> ": " <> show got | not (p got)]
  pure $ case k of
    Cols -> is 80 v
    Rows -> is 24 v
    CursorX -> is 0 v
    CursorY -> is 0 v
    CursorPendingWrap -> is False v
    ActiveScreen -> is Primary v
    CursorVisible -> is True v
    KittyKeyboardFlags -> is noKittyFlags v
    Scrollbar -> is (ScrollState 24 0 24) v
    CursorStyle -> is defaultStyle v
    MouseTracking -> is False v
    Title -> is "" v
    Pwd -> is "" v
    TotalRows -> is 24 v
    ScrollbackRows -> is 0 v
    WidthPx -> is 0 v
    HeightPx -> is 0 v
    ColorForeground -> is Nothing v
    ColorBackground -> is Nothing v
    ColorCursor -> is Nothing v
    ColorPalette -> holds ((== 256) . length . paletteColors) v
    ColorForegroundDefault -> is Nothing v
    ColorBackgroundDefault -> is Nothing v
    ColorCursorDefault -> is Nothing v
    ColorPaletteDefault -> holds ((== 256) . length . paletteColors) v
    KittyImageStorageLimit -> holds isJust v
    KittyImageMediumFile -> holds isJust v
    KittyImageMediumTempFile -> holds isJust v
    KittyImageMediumSharedMem -> holds isJust v
    ViewportActive -> is True v
    VtProcessingError -> is False v
    ScrollbackMaxBytes -> holds (const True) v
    ScrollbackMaxLines -> holds (const True) v
    ContinuationMaxBytes -> is 0 v
    Mode m -> is (modeOnAtStart m) v
    VtGround -> is True v
    CursorAtPrompt -> is False v
    ClipboardWriteMaxBytes -> is (64 * 1024 * 1024) v
    MouseShape -> is MouseText v
    MemoryUsage -> holds (\u -> u.primary.pages >= 1) v

-- | The modes a fresh terminal has set.
modeOnAtStart :: ModeKey r -> Bool
modeOnAtStart m =
  modeCode m
    `elem` map
      (\(SomeModeKey k) -> modeCode k)
      [ SomeModeKey ModeSrm,
        SomeModeKey ModeWraparound,
        SomeModeKey ModeCursorVisible,
        SomeModeKey ModeAltScroll,
        SomeModeKey ModeNumlockKeypad,
        SomeModeKey ModeAltEscPrefix
      ]

-- * Options

-- | Every option: each constructor once, 'SetMode' at every mode and
-- 'SetModeDefault' at every mode whose default may be set.
allOptions :: [Some TerminalOption]
allOptions =
  [ Some SetTitle,
    Some SetPwd,
    Some SetColorForeground,
    Some SetColorBackground,
    Some SetColorCursor,
    Some SetColorPalette,
    Some SetKittyImageStorageLimit,
    Some SetKittyImageMediumFile,
    Some SetKittyImageMediumTempFile,
    Some SetKittyImageMediumSharedMem,
    Some SetApcMaxBytes,
    Some SetApcMaxBytesKitty,
    Some SetDefaultCursorStyle,
    Some SetDefaultCursorBlink,
    Some SetGlyphProtocol,
    Some SetScrollbackMaxBytes,
    Some SetScrollbackMaxLines,
    Some SetContinuationMaxBytes,
    Some SetTitleReport,
    Some SetUnknownMaxBytes,
    Some SetTerminfoName,
    Some SetClipboardWriteMaxBytes,
    Some SetResizePullScrollback,
    Some SetXtChecksumReport,
    Some SetXtChecksumExtension
  ]
    <> [Some (SetMode m) | SomeModeKey m <- allModes]
    <> [Some (SetModeDefault m) | SomeModeKey k <- allModes, Just m <- [configurableMode k]]

-- | The header's options with no constructor, and why (DESIGN.md §4.4).
deferredOptions :: [(Text, Integer)]
deferredOptions =
  ("SELECTION: grid references, PLAN.md M6.5", optionNumber GhosttyTerminalOptSelection)
    : [ ("a callback or its userdata, installed by the binding, PLAN.md M1.6", optionNumber o)
      | o <-
          [ GhosttyTerminalOptUserdata,
            GhosttyTerminalOptWritePty,
            GhosttyTerminalOptBell,
            GhosttyTerminalOptEnquiry,
            GhosttyTerminalOptXtversion,
            GhosttyTerminalOptTitleChanged,
            GhosttyTerminalOptSize,
            GhosttyTerminalOptColorScheme,
            GhosttyTerminalOptDeviceAttributes,
            GhosttyTerminalOptPwdChanged,
            GhosttyTerminalOptClipboardWrite,
            GhosttyTerminalOptDesktopNotification,
            GhosttyTerminalOptProgressReport,
            GhosttyTerminalOptUnknownSequence,
            GhosttyTerminalOptClipboardRead,
            GhosttyTerminalOptRenderHold,
            GhosttyTerminalOptSemanticPrompt,
            GhosttyTerminalOptReset,
            GhosttyTerminalOptProgramStatus,
            GhosttyTerminalOptResizeWindow
          ]
      ]

-- | An option's number.
optionNumber :: GhosttyTerminalOption -> Integer
optionNumber (GhosttyTerminalOption n) = toInteger n

-- | Write an option, and read it back where a data key reads the same
-- setting. Total, as 'fresh' is.
written :: Terminal s -> TerminalOption a -> Eff '[Ghostty, IOE] [String]
written t o = case o of
  SetTitle -> roundTrip o "canopy ✓" Title id
  SetPwd -> roundTrip o "file:///tmp" Pwd id
  SetColorForeground -> colour o ColorForeground ColorForegroundDefault
  SetColorBackground -> colour o ColorBackground ColorBackgroundDefault
  SetColorCursor -> colour o ColorCursor ColorCursorDefault
  SetColorPalette -> do
    p <- getT t ColorPaletteDefault
    let p' = setPaletteColor 1 red' p
    a <- roundTrip o (Just p') ColorPaletteDefault Just
    b <- roundTrip o (Just p') ColorPalette Just
    c <- roundTrip o Nothing ColorPaletteDefault (\q -> if q == p then Nothing else Just q)
    pure (a <> b <> c)
  -- Without kitty graphics the reads have no value, and there is nothing to
  -- compare: the view then reports what was written.
  SetKittyImageStorageLimit -> roundTrip o 1048576 KittyImageStorageLimit (fromMaybe 1048576)
  SetKittyImageMediumFile -> roundTrip o True KittyImageMediumFile (fromMaybe True)
  SetKittyImageMediumTempFile ->
    (<>)
      <$> roundTrip o (Just "/tmp") KittyImageMediumTempFile (maybe (Just "/tmp") Just)
      <*> roundTrip o Nothing KittyImageMediumTempFile (\r -> if r `elem` [Nothing, Just ""] then Nothing else r)
  SetKittyImageMediumSharedMem -> roundTrip o True KittyImageMediumSharedMem (fromMaybe True)
  SetApcMaxBytes -> (<>) <$> writes o (Just 4096) <*> writes o Nothing
  SetApcMaxBytesKitty -> (<>) <$> writes o (Just 4096) <*> writes o Nothing
  SetDefaultCursorStyle -> concat <$> traverse (writes o) universe
  SetDefaultCursorBlink -> writes o True
  SetGlyphProtocol -> writes o False
  SetScrollbackMaxBytes -> (<>) <$> roundTrip o (Just 10000000) ScrollbackMaxBytes id <*> roundTrip o Nothing ScrollbackMaxBytes id
  SetScrollbackMaxLines -> (<>) <$> roundTrip o (Just 5000) ScrollbackMaxLines id <*> roundTrip o Nothing ScrollbackMaxLines id
  SetContinuationMaxBytes -> roundTrip o 64 ContinuationMaxBytes id
  SetTitleReport -> writes o True
  SetModeDefault m -> (<>) <$> roundTrip o (not (modeOnAtStart m)) (Mode m) id <*> roundTrip o (modeOnAtStart m) (Mode m) id
  SetMode m -> (<>) <$> roundTrip o (not (modeOnAtStart m)) (Mode m) id <*> roundTrip o (modeOnAtStart m) (Mode m) id
  SetUnknownMaxBytes -> writes o 2048
  SetTerminfoName -> maybe (pure ["terminfoName refused a short name"]) (writes o) (terminfoName "xterm-256color")
  SetClipboardWriteMaxBytes -> roundTrip o 1024 ClipboardWriteMaxBytes id
  SetResizePullScrollback -> writes o False
  SetXtChecksumReport -> writes o True
  SetXtChecksumExtension -> writes o decChecksum {fullCodepoints = True, dontNegate = True}
  where
    red' = Rgb 255 0 0

    -- write, and nothing reads it back
    writes :: TerminalOption v -> v -> Eff '[Ghostty, IOE] [String]
    writes opt v = [] <$ setT t opt v

    -- write, read with the key, and compare what was read (through the
    -- view, which normalises what the key reports) with what was written
    roundTrip :: (Eq v, Show v) => TerminalOption v -> v -> TerminalData b -> (b -> v) -> Eff '[Ghostty, IOE] [String]
    roundTrip opt v k view = do
      setT t opt v
      got <- view <$> getT t k
      pure [show opt <> " " <> show v <> ": read back " <> show got | got /= v]

    -- a default colour: it is the default, it is in effect, and it unsets
    colour :: TerminalOption (Maybe Rgb) -> TerminalData (Maybe Rgb) -> TerminalData (Maybe Rgb) -> Eff '[Ghostty, IOE] [String]
    colour opt effective dflt = do
      a <- roundTrip opt (Just red') dflt id
      b <- roundTrip opt (Just red') effective id
      c <- roundTrip opt Nothing dflt id
      pure (a <> b <> c)

-- * Modes against the library

-- | Whether the library reads this mode number (a get of an unknown mode is
-- @GHOSTTY_INVALID_VALUE@).
recognises :: Terminal s -> GhosttyMode -> IO Bool
recognises t m = with (GhosttyTerminalModeConfig {mode = m, value = 0}) $ \p ->
  (== GhosttySuccess) <$> ghosttyTerminalGet t.handle GhosttyTerminalDataMode (castPtr p)

-- | Whether the library accepts this mode's reset default, through the raw
-- call that the types keep 'Fixed' modes away from. Success leaves the
-- default as the library already had it.
defaultSettable :: Terminal s -> SomeModeKey -> IO Bool
defaultSettable t (SomeModeKey m) = alloca $ \p -> do
  poke p GhosttyTerminalModeConfig {mode = modeCode m, value = if modeOnAtStart m then 1 else 0}
  r <- ghosttyTerminalSet t.handle GhosttyTerminalOptModeDefault (castPtr p)
  if r == GhosttySuccess || r == GhosttyInvalidValue
    then pure (r == GhosttySuccess)
    else assertFailure ("MODE_DEFAULT " <> show m <> ": " <> show r)

-- | Whether the mode's type says its default may be set.
configurable :: SomeModeKey -> Bool
configurable (SomeModeKey m) = isJust (configurableMode m)
