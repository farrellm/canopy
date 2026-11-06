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
import Data.Bits (shiftL, testBit, (.|.))
import Data.ByteString qualified as BS
import Foreign.C.Types (CBool, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Marshal.Utils (fromBool, with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Ghostty.Effectful.Color (Palette, paletteColors, paletteFromList)
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), ResultCode (..))
import Ghostty.Effectful.Internal.Convert (enumName, fromRgb, fromStyle, modeCode, toRgb, unknownEnum)
import Ghostty.Effectful.Internal.Error (expectSuccess, resultCode)
import Ghostty.Effectful.Style (Style)
import Ghostty.Effectful.Terminal.Keys
  ( ChecksumExtension (..),
    CursorShape (..),
    KittyFlags (..),
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
import Ghostty.FFI.Types (GhosttyString (..), ghosttyStringBytes)

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
  CursorStyle -> GhosttyTerminalDataCursorStyle
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

-- | The header's key for a write.
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
  SetDefaultCursorShape -> GhosttyTerminalOptDefaultCursorStyle
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
  KittyKeyboardFlags -> required >>= kittyFlags
  Scrollbar -> (\(s :: GhosttyTerminalScrollbar) -> ScrollState s.total s.offset s.len) <$> required
  CursorStyle -> style
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
  KittyImageMediumFile -> fmap (/= 0) <$> (maybeValue :: IO (Maybe CBool))
  KittyImageMediumTempFile -> traverse decode =<< maybeValue
  KittyImageMediumSharedMem -> fmap (/= 0) <$> (maybeValue :: IO (Maybe CBool))
  ViewportActive -> flag
  VtProcessingError -> flag
  ScrollbackMaxBytes -> fmap toWord <$> maybeValue
  ScrollbackMaxLines -> fmap toWord <$> maybeValue
  ContinuationMaxBytes -> word
  Mode m -> do
    c <- fetchInto (GhosttyTerminalModeConfig {mode = modeCode m, value = 0})
    maybe (throwIO (Invariant site ResultNoValue)) (pure . (/= 0) . (.value)) c
  VtGround -> flag
  CursorAtPrompt -> flag
  ClipboardWriteMaxBytes -> word
  MouseShape -> required >>= mouseShape
  MemoryUsage -> do
    u <- fetchInto (memoryUsage (fromIntegral (sizeOf (memoryUsage 0))))
    maybe (throwIO (Invariant site ResultNoValue)) (pure . fromMemoryUsage) u
  where
    k = dataKey key
    site = CallSite ("ghostty_terminal_get " <> enumName ghosttyTerminalDataEnum (let GhosttyTerminalData n = k in n))

    -- The read into memory set up by the caller; 'Nothing' on NO_VALUE.
    fetchWith :: (Storable b) => (Ptr b -> IO ()) -> IO (Maybe b)
    fetchWith prepare = alloca $ \p -> do
      prepare p
      r <- ghosttyTerminalGet h k (castPtr p)
      case resultCode r of
        ResultSuccess -> Just <$> peek p
        ResultNoValue -> pure Nothing
        ResultOutOfMemory -> throwIO (OutOfMemory site)
        other -> throwIO (Invariant site other)

    fetchInto :: (Storable b) => b -> IO (Maybe b)
    fetchInto v = fetchWith (`poke` v)

    maybeValue :: (Storable b) => IO (Maybe b)
    maybeValue = fetchWith (const pass)

    required :: (Storable b) => IO b
    required = maybeValue >>= maybe (throwIO (Invariant site ResultNoValue)) pure

    flag :: IO Bool
    flag = (/= (0 :: CBool)) <$> required

    word :: IO Word
    word = toWord <$> required

    toWord :: CSize -> Word
    toWord = fromIntegral

    decode :: GhosttyString -> IO Text
    decode s = decodeUtf8 <$> ghosttyStringBytes s

    string :: IO Text
    string = required >>= decode

    style :: IO Style
    style = fetchWith ghosttyStyleDefault >>= maybe (throwIO (Invariant site ResultNoValue)) (fromStyle site)

    palette :: IO Palette
    palette = allocaArray 256 $ \p -> do
      expectSuccess site =<< ghosttyTerminalGet h k (castPtr p)
      cs <- peekArray 256 p
      maybe (throwIO (Invariant site ResultInvalidValue)) pure (paletteFromList (map fromRgb cs))

    kittyFlags :: Word8 -> IO KittyFlags
    kittyFlags w
      | w > 31 = throwIO (UnknownEnum site "GhosttyKittyKeyFlags" (toInteger w))
      | otherwise =
          pure
            KittyFlags
              { disambiguate = testBit w 0,
                reportEvents = testBit w 1,
                reportAlternates = testBit w 2,
                reportAll = testBit w 3,
                reportAssociated = testBit w 4
              }

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
    { compressionSupported = u.compressionSupported /= 0,
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
setOption :: GhosttyTerminal -> TerminalOption a -> a -> IO ()
setOption h opt v = case opt of
  SetTitle -> text v put
  SetPwd -> text v put
  SetColorForeground -> maybeWith (toRgb <$> v) put
  SetColorBackground -> maybeWith (toRgb <$> v) put
  SetColorCursor -> maybeWith (toRgb <$> v) put
  SetColorPalette -> case v of
    Nothing -> put nullPtr
    Just p -> withArray (map toRgb (paletteColors p)) (put . castPtr)
  SetKittyImageStorageLimit -> with v (put . castPtr)
  SetKittyImageMediumFile -> flag v
  SetKittyImageMediumTempFile -> maybe (put nullPtr) (`text` put) v
  SetKittyImageMediumSharedMem -> flag v
  SetApcMaxBytes -> maybeWith (toCSize <$> v) put
  SetApcMaxBytesKitty -> maybeWith (toCSize <$> v) put
  SetDefaultCursorShape -> with (cursorStyle v) (put . castPtr)
  SetDefaultCursorBlink -> flag v
  SetGlyphProtocol -> flag v
  SetScrollbackMaxBytes -> maybeWith (toCSize <$> v) put
  SetScrollbackMaxLines -> maybeWith (toCSize <$> v) put
  SetContinuationMaxBytes -> with (toCSize v) (put . castPtr)
  SetTitleReport -> flag v
  SetModeDefault m -> with (GhosttyTerminalModeConfig {mode = modeCode m, value = fromBool v}) (put . castPtr)
  SetMode m -> with (GhosttyTerminalModeConfig {mode = modeCode m, value = fromBool v}) (put . castPtr)
  SetUnknownMaxBytes -> with (toCSize v) (put . castPtr)
  SetTerminfoName -> text (fromTerminfoName v) put
  SetClipboardWriteMaxBytes -> with (toCSize v) (put . castPtr)
  SetResizePullScrollback -> flag v
  SetXtChecksumReport -> flag v
  SetXtChecksumExtension -> with (checksumBits v) (put . castPtr)
  where
    k = optionKey opt
    site = CallSite ("ghostty_terminal_set " <> enumName ghosttyTerminalOptionEnum (let GhosttyTerminalOption n = k in n))

    put :: Ptr () -> IO ()
    put p = expectSuccess site =<< ghosttyTerminalSet h k p

    flag :: Bool -> IO ()
    flag b = with (fromBool b :: CBool) (put . castPtr)

    toCSize :: Word -> CSize
    toCSize = fromIntegral

-- | Run with a pointer to the value, or null.
maybeWith :: (Storable b) => Maybe b -> (Ptr () -> IO ()) -> IO ()
maybeWith Nothing f = f nullPtr
maybeWith (Just b) f = with b (f . castPtr)

-- | Run with a pointer to a borrowed 'GhosttyString' of the text's UTF-8,
-- which the library copies.
text :: Text -> (Ptr () -> IO ()) -> IO ()
text t f = BS.useAsCStringLen (encodeUtf8 t) $ \(p, n) ->
  with (GhosttyString {ptr = castPtr p, len = fromIntegral n}) (f . castPtr)

-- | A cursor shape as the binding's.
cursorStyle :: CursorShape -> GhosttyTerminalCursorStyle
cursorStyle = \case
  BarCursor -> GhosttyTerminalCursorStyleBar
  BlockCursor -> GhosttyTerminalCursorStyleBlock
  UnderlineCursor -> GhosttyTerminalCursorStyleUnderline
  HollowBlockCursor -> GhosttyTerminalCursorStyleBlockHollow

-- | XTCHECKSUM's bits.
checksumBits :: ChecksumExtension -> Word8
checksumBits c =
  foldl' (.|.) 0 [if set then 1 `shiftL` i else 0 | (i, set) <- zip [0 ..] bits]
  where
    bits = [c.dontNegate, c.dontAddAttributes, c.dontOmitBlanks, c.unwrittenAsSpaces, c.fullCodepoints]
