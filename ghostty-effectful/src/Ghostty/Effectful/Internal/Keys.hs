-- | The marshalling behind the typed keys (DESIGN.md §4.4): for each
-- 'TerminalData' constructor, the @ghostty_terminal_get@ key and a @peek@
-- at its output type; for each 'TerminalOption', the
-- @ghostty_terminal_set@ key and a @poke@ of its input type.
--
-- A read the header says may have no value maps @GHOSTTY_NO_VALUE@ to
-- 'Nothing'; any other read that fails is 'Invariant', since a constructor
-- is a key the header names and its output is the type the header gives.
module Ghostty.Effectful.Internal.Keys
  ( -- * Keys
    dataKey,
    optionKey,

    -- * Calls
    getData,
    setOption,
  )
where

import Control.Exception (throwIO)
import Data.Bits (bit)
import Foreign.C.Types (CBool, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (fromBool, maybeWith, toBool, with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import Ghostty.Effectful.Color (Palette, paletteColors)
import Ghostty.Effectful.Error (ResultCode (..))
import Ghostty.Effectful.Internal.Callbacks (Answers (..), captureRelease, setEnquiryAnswer, setXtversionAnswer)
import Ghostty.Effectful.Internal.Convert (enumSite, flagBits, fromKittyFlagBits, fromRgb, fromStyle, modeCode, readPalette, stringText, toCursorStyle, toRgb, unknownEnum, withSize, withString)
import Ghostty.Effectful.Internal.Error (expectSuccess, orInvariant, resultCode, resultError)
import Ghostty.Effectful.Internal.Terminal (Terminal (..), wrote)
import Ghostty.Effectful.Mode (ModeKey (..))
import Ghostty.Effectful.Style (Style)
import Ghostty.Effectful.Terminal.Keys
  ( ChecksumExtension (..),
    Memory (..),
    MouseShape (..),
    Screen (..),
    ScreenMemory (..),
    ScrollState (..),
    TerminalData (..),
    TerminalOption (..),
    fromTerminfoName,
  )
import Ghostty.FFI.Mouse
import Ghostty.FFI.Style (ghosttyStyleDefault)
import Ghostty.FFI.Terminal
import Ghostty.FFI.Types (GhosttyString)

-- | The header's key for a read.
dataKey :: TerminalData a -> GhosttyTerminalData
dataKey = \case
  Cols -> GhosttyTerminalDataCols
  Rows -> GhosttyTerminalDataRows
  CursorX -> GhosttyTerminalDataCursorX
  CursorY -> GhosttyTerminalDataCursorY
  CursorPendingWrap -> GhosttyTerminalDataCursorPendingWrap
  ActiveScreen -> GhosttyTerminalDataActiveScreen
  CursorVisible -> GhosttyTerminalDataCursorVisible
  KittyKeyboardFlags -> GhosttyTerminalDataKittyKeyboardFlags
  Scrollbar -> GhosttyTerminalDataScrollbar
  CursorSgrStyle -> GhosttyTerminalDataCursorSgrStyle
  MouseTracking -> GhosttyTerminalDataMouseTracking
  Title -> GhosttyTerminalDataTitle
  Pwd -> GhosttyTerminalDataPwd
  TotalRows -> GhosttyTerminalDataTotalRows
  ScrollbackRows -> GhosttyTerminalDataScrollbackRows
  WidthPx -> GhosttyTerminalDataWidthPx
  HeightPx -> GhosttyTerminalDataHeightPx
  ColorForeground -> GhosttyTerminalDataColorForeground
  ColorBackground -> GhosttyTerminalDataColorBackground
  ColorCursor -> GhosttyTerminalDataColorCursor
  ColorPalette -> GhosttyTerminalDataColorPalette
  ColorForegroundDefault -> GhosttyTerminalDataColorForegroundDefault
  ColorBackgroundDefault -> GhosttyTerminalDataColorBackgroundDefault
  ColorCursorDefault -> GhosttyTerminalDataColorCursorDefault
  ColorPaletteDefault -> GhosttyTerminalDataColorPaletteDefault
  KittyImageStorageLimit -> GhosttyTerminalDataKittyImageStorageLimit
  KittyImageMediumFile -> GhosttyTerminalDataKittyImageMediumFile
  KittyImageMediumTempFile -> GhosttyTerminalDataKittyImageMediumTempFile
  KittyImageMediumSharedMem -> GhosttyTerminalDataKittyImageMediumSharedMem
  ViewportActive -> GhosttyTerminalDataViewportActive
  VtProcessingError -> GhosttyTerminalDataVtProcessingError
  ScrollbackMaxBytes -> GhosttyTerminalDataScrollbackMaxBytes
  ScrollbackMaxLines -> GhosttyTerminalDataScrollbackMaxLines
  ContinuationMaxBytes -> GhosttyTerminalDataContinuationMaxBytes
  Mode _ -> GhosttyTerminalDataMode
  VtGround -> GhosttyTerminalDataVtGround
  CursorAtPrompt -> GhosttyTerminalDataCursorAtPrompt
  ClipboardWriteMaxBytes -> GhosttyTerminalDataClipboardWriteMaxBytes
  MouseShape -> GhosttyTerminalDataMouseShape
  MemoryUsage -> GhosttyTerminalDataMemoryUsage

-- | The header's key for a write. For an option that sets a query's answer,
-- the callback option whose answer it is, which 'setOption' never sends.
optionKey :: TerminalOption a -> GhosttyTerminalOption
optionKey = \case
  SetTitle -> GhosttyTerminalOptTitle
  SetPwd -> GhosttyTerminalOptPwd
  SetColorForeground -> GhosttyTerminalOptColorForeground
  SetColorBackground -> GhosttyTerminalOptColorBackground
  SetColorCursor -> GhosttyTerminalOptColorCursor
  SetColorPalette -> GhosttyTerminalOptColorPalette
  SetKittyImageStorageLimit -> GhosttyTerminalOptKittyImageStorageLimit
  SetKittyImageMediumFile -> GhosttyTerminalOptKittyImageMediumFile
  SetKittyImageMediumTempFile -> GhosttyTerminalOptKittyImageMediumTempFile
  SetKittyImageMediumSharedMem -> GhosttyTerminalOptKittyImageMediumSharedMem
  SetApcMaxBytes -> GhosttyTerminalOptApcMaxBytes
  SetApcMaxBytesKitty -> GhosttyTerminalOptApcMaxBytesKitty
  SetDefaultCursorStyle -> GhosttyTerminalOptDefaultCursorStyle
  SetDefaultCursorBlink -> GhosttyTerminalOptDefaultCursorBlink
  SetGlyphProtocol -> GhosttyTerminalOptGlyphProtocol
  SetScrollbackMaxBytes -> GhosttyTerminalOptScrollbackMaxBytes
  SetScrollbackMaxLines -> GhosttyTerminalOptScrollbackMaxLines
  SetContinuationMaxBytes -> GhosttyTerminalOptContinuationMaxBytes
  SetTitleReport -> GhosttyTerminalOptTitleReport
  SetModeDefault _ -> GhosttyTerminalOptModeDefault
  SetMode _ -> GhosttyTerminalOptMode
  SetUnknownMaxBytes -> GhosttyTerminalOptUnknownMaxBytes
  SetTerminfoName -> GhosttyTerminalOptTerminfoName
  SetClipboardWriteMaxBytes -> GhosttyTerminalOptClipboardWriteMaxBytes
  SetResizePullScrollback -> GhosttyTerminalOptResizePullScrollback
  SetXtChecksumReport -> GhosttyTerminalOptXtChecksumReport
  SetXtChecksumExtension -> GhosttyTerminalOptXtChecksumExtension
  SetColorScheme -> GhosttyTerminalOptColorScheme
  SetDeviceAttributes -> GhosttyTerminalOptDeviceAttributes
  SetEnquiry -> GhosttyTerminalOptEnquiry
  SetXtversion -> GhosttyTerminalOptXtversion
  SetSize -> GhosttyTerminalOptSize
  SetClipboardWrite -> GhosttyTerminalOptClipboardWrite

-- | @ghostty_terminal_get@ at the key's output type, copied out.
getData :: GhosttyTerminal -> TerminalData a -> IO a
getData h key = case key of
  Cols -> required
  Rows -> required
  CursorX -> required
  CursorY -> required
  CursorPendingWrap -> flag
  ActiveScreen ->
    required >>= \case
      GhosttyTerminalScreenPrimary -> pure Primary
      GhosttyTerminalScreenAlternate -> pure Alternate
      GhosttyTerminalScreenUnknown n -> unknownEnum site ghosttyTerminalScreenEnum n
  CursorVisible -> flag
  KittyKeyboardFlags -> required >>= fromKittyFlagBits site
  Scrollbar -> (\(s :: GhosttyTerminalScrollbar) -> ScrollState s.total s.offset s.len) <$> required
  CursorSgrStyle -> style
  MouseTracking -> flag
  Title -> string
  Pwd -> string
  TotalRows -> word
  ScrollbackRows -> word
  WidthPx -> required
  HeightPx -> required
  ColorForeground -> fmap fromRgb <$> maybeValue
  ColorBackground -> fmap fromRgb <$> maybeValue
  ColorCursor -> fmap fromRgb <$> maybeValue
  ColorPalette -> palette
  ColorForegroundDefault -> fmap fromRgb <$> maybeValue
  ColorBackgroundDefault -> fmap fromRgb <$> maybeValue
  ColorCursorDefault -> fmap fromRgb <$> maybeValue
  ColorPaletteDefault -> palette
  KittyImageStorageLimit -> maybeValue
  KittyImageMediumFile -> fmap (toBool @CBool) <$> maybeValue
  KittyImageMediumTempFile -> traverse stringText =<< maybeValue
  KittyImageMediumSharedMem -> fmap (toBool @CBool) <$> maybeValue
  ViewportActive -> flag
  VtProcessingError -> flag
  ScrollbackMaxBytes -> fmap toWord <$> maybeValue
  ScrollbackMaxLines -> fmap toWord <$> maybeValue
  ContinuationMaxBytes -> word
  Mode m -> do
    c <- orInvariant site ResultNoValue =<< fetchInto (GhosttyTerminalModeConfig {mode = modeCode m, value = 0})
    pure (toBool c.value)
  VtGround -> flag
  CursorAtPrompt -> flag
  ClipboardWriteMaxBytes -> word
  MouseShape -> required >>= mouseShape
  MemoryUsage -> do
    fromMemoryUsage <$> (orInvariant site ResultNoValue =<< fetchInto (withSize memoryUsage))
  where
    k = dataKey key
    site = enumSite "ghostty_terminal_get" ghosttyTerminalDataEnum (coerce k)

    -- The read into memory set up by the caller; 'Nothing' on NO_VALUE.
    -- Written out rather than through 'optionalVia': 'resolveOnto' reads
    -- the cursor through here after every prompt mark, and the helper
    -- costs the feed benchmark's prompts about 200 bytes a read.
    fetchWith :: (Storable b) => (Ptr b -> IO ()) -> IO (Maybe b)
    fetchWith prepare = alloca $ \p -> do
      prepare p
      r <- ghosttyTerminalGet h k (castPtr p)
      case resultCode r of
        ResultSuccess -> Just <$> peek p
        ResultNoValue -> pure Nothing
        other -> throwIO (resultError site other)

    fetchInto :: (Storable b) => b -> IO (Maybe b)
    fetchInto v = fetchWith (`poke` v)

    maybeValue :: (Storable b) => IO (Maybe b)
    maybeValue = fetchWith (const pass)

    required :: (Storable b) => IO b
    required = orInvariant site ResultNoValue =<< maybeValue

    flag :: IO Bool
    flag = toBool @CBool <$> required

    word :: IO Word
    word = toWord <$> required

    toWord :: CSize -> Word
    toWord = fromIntegral

    string :: IO Text
    string = stringText =<< required

    style :: IO Style
    style = fromStyle site =<< orInvariant site ResultNoValue =<< fetchWith ghosttyStyleDefault

    palette :: IO Palette
    palette = readPalette site (ghosttyTerminalGet h k . castPtr)

    mouseShape :: GhosttyMouseShape -> IO MouseShape
    mouseShape = \case
      GhosttyMouseShapeDefault -> pure MouseDefault
      GhosttyMouseShapeContextMenu -> pure MouseContextMenu
      GhosttyMouseShapeHelp -> pure MouseHelp
      GhosttyMouseShapePointer -> pure MousePointer
      GhosttyMouseShapeProgress -> pure MouseProgress
      GhosttyMouseShapeWait -> pure MouseWait
      GhosttyMouseShapeCell -> pure MouseCell
      GhosttyMouseShapeCrosshair -> pure MouseCrosshair
      GhosttyMouseShapeText -> pure MouseText
      GhosttyMouseShapeVerticalText -> pure MouseVerticalText
      GhosttyMouseShapeAlias -> pure MouseAlias
      GhosttyMouseShapeCopy -> pure MouseCopy
      GhosttyMouseShapeMove -> pure MouseMove
      GhosttyMouseShapeNoDrop -> pure MouseNoDrop
      GhosttyMouseShapeNotAllowed -> pure MouseNotAllowed
      GhosttyMouseShapeGrab -> pure MouseGrab
      GhosttyMouseShapeGrabbing -> pure MouseGrabbing
      GhosttyMouseShapeAllScroll -> pure MouseAllScroll
      GhosttyMouseShapeColResize -> pure MouseColResize
      GhosttyMouseShapeRowResize -> pure MouseRowResize
      GhosttyMouseShapeNResize -> pure MouseNResize
      GhosttyMouseShapeEResize -> pure MouseEResize
      GhosttyMouseShapeSResize -> pure MouseSResize
      GhosttyMouseShapeWResize -> pure MouseWResize
      GhosttyMouseShapeNeResize -> pure MouseNeResize
      GhosttyMouseShapeNwResize -> pure MouseNwResize
      GhosttyMouseShapeSeResize -> pure MouseSeResize
      GhosttyMouseShapeSwResize -> pure MouseSwResize
      GhosttyMouseShapeEwResize -> pure MouseEwResize
      GhosttyMouseShapeNsResize -> pure MouseNsResize
      GhosttyMouseShapeNeswResize -> pure MouseNeswResize
      GhosttyMouseShapeNwseResize -> pure MouseNwseResize
      GhosttyMouseShapeZoomIn -> pure MouseZoomIn
      GhosttyMouseShapeZoomOut -> pure MouseZoomOut
      GhosttyMouseShapeUnknown n -> unknownEnum site ghosttyMouseShapeEnum n

-- | A memory report of the given struct size, every count zero.
memoryUsage :: CSize -> GhosttyTerminalMemoryUsage
memoryUsage n =
  GhosttyTerminalMemoryUsage
    { size = n,
      compressionSupported = 0,
      primaryPages = 0,
      primaryVirtualBytes = 0,
      primaryResidentBytes = 0,
      primaryCompressedPages = 0,
      primaryCompressedBytes = 0,
      primaryImageBytes = 0,
      alternatePages = 0,
      alternateVirtualBytes = 0,
      alternateResidentBytes = 0,
      alternateCompressedPages = 0,
      alternateCompressedBytes = 0,
      alternateImageBytes = 0
    }

-- | A memory report from the binding's.
fromMemoryUsage :: GhosttyTerminalMemoryUsage -> Memory
fromMemoryUsage u =
  Memory
    { compressionSupported = toBool u.compressionSupported,
      primary =
        ScreenMemory
          u.primaryPages
          u.primaryVirtualBytes
          u.primaryResidentBytes
          u.primaryCompressedPages
          u.primaryCompressedBytes
          u.primaryImageBytes,
      alternate =
        ScreenMemory
          u.alternatePages
          u.alternateVirtualBytes
          u.alternateResidentBytes
          u.alternateCompressedPages
          u.alternateCompressedBytes
          u.alternateImageBytes
    }

-- | @ghostty_terminal_set@ with the value poked at the key's input type.
-- Nothing a constructor can carry is refused, so a refusal is 'Invariant'.
-- An option that sets the answer to a query is not the library's: it
-- changes what the binding's callback answers with (DESIGN.md §4.5).
--
-- Every write counts as a change of the terminal ('wrote'). Turning
-- synchronized output off ends a render hold, which calls no callback, so
-- it drops every render state's captured frame here (§4.3).
setOption :: Terminal s -> TerminalOption a -> a -> IO ()
setOption t opt v = wrote t >> writeOption t opt v

-- | 'setOption' without the count.
writeOption :: Terminal s -> TerminalOption a -> a -> IO ()
writeOption t opt v = case opt of
  SetTitle -> text v put
  SetPwd -> text v put
  SetColorForeground -> maybeWith with (toRgb <$> v) put
  SetColorBackground -> maybeWith with (toRgb <$> v) put
  SetColorCursor -> maybeWith with (toRgb <$> v) put
  SetColorPalette -> maybeWith (withArray . map toRgb . paletteColors) v put
  SetKittyImageStorageLimit -> with v put
  SetKittyImageMediumFile -> flag v
  SetKittyImageMediumTempFile -> maybeWith text v put
  SetKittyImageMediumSharedMem -> flag v
  SetApcMaxBytes -> maybeWith with (toCSize <$> v) put
  SetApcMaxBytesKitty -> maybeWith with (toCSize <$> v) put
  SetDefaultCursorStyle -> with (toCursorStyle v) put
  SetDefaultCursorBlink -> flag v
  SetGlyphProtocol -> flag v
  -- No byte limit is SIZE_MAX: since libghostty-vt 1.0 a null value
  -- restores the default of 10,000 bytes (regression 11).
  SetScrollbackMaxBytes -> with (maybe maxBound toCSize v) put
  SetScrollbackMaxLines -> maybeWith with (toCSize <$> v) put
  SetContinuationMaxBytes -> with (toCSize v) put
  SetTitleReport -> flag v
  SetModeDefault m -> modeValue m v
  SetMode ModeSynchronizedOutput -> do
    modeValue ModeSynchronizedOutput v
    unless v $ captureRelease t.installed
  SetMode m -> modeValue m v
  SetUnknownMaxBytes -> with (toCSize v) put
  SetTerminfoName -> text (fromTerminfoName v) put
  SetClipboardWriteMaxBytes -> with (toCSize v) put
  SetResizePullScrollback -> flag v
  SetXtChecksumReport -> flag v
  SetXtChecksumExtension -> with (checksumBits v) put
  SetColorScheme -> answer (\a -> a {scheme = v})
  SetDeviceAttributes -> answer (\a -> a {attributes = v})
  SetSize -> answer (\a -> a {sizeQueries = v})
  SetClipboardWrite -> answer (\a -> a {clipboard = v})
  SetEnquiry -> setEnquiryAnswer t.installed v
  SetXtversion -> setXtversionAnswer t.installed (encodeUtf8 v)
  where
    h = t.handle
    k = optionKey opt
    answer = modifyIORef' t.answers
    site = enumSite "ghostty_terminal_set" ghosttyTerminalOptionEnum (coerce k)

    put :: Ptr b -> IO ()
    put p = expectSuccess site =<< ghosttyTerminalSet h k (castPtr p)

    flag :: Bool -> IO ()
    flag b = with (fromBool @CBool b) put

    modeValue :: ModeKey r -> Bool -> IO ()
    modeValue m b = with (GhosttyTerminalModeConfig {mode = modeCode m, value = fromBool b}) put

    toCSize :: Word -> CSize
    toCSize = fromIntegral

-- | Run with a pointer to a borrowed 'GhosttyString' of the text's UTF-8,
-- which the library copies.
text :: Text -> (Ptr GhosttyString -> IO a) -> IO a
text = withString . encodeUtf8

-- | XTCHECKSUM's bits, in the order of 'ChecksumExtension''s fields.
checksumBits :: ChecksumExtension -> Word8
checksumBits c =
  flagBits (zip [c.dontNegate, c.dontAddAttributes, c.dontOmitBlanks, c.unwrittenAsSpaces, c.fullCodepoints] (map bit [0 ..]))
