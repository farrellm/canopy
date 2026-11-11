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
    cursorAt,
    setOption,
  )
where

import Data.Bits (bit)
import Foreign.C.Types (CBool, CSize)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (fromBool, maybeWith, toBool, with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import GHC.ForeignPtr (unsafeWithForeignPtr)
import Ghostty.Effectful.Color (Palette, paletteColors)
import Ghostty.Effectful.Error (CallSite, ResultCode (..))
import Ghostty.Effectful.Internal.Callbacks (Answers (..), captureRelease, setEnquiryAnswer, setXtversionAnswer)
import Ghostty.Effectful.Internal.Convert (enumSite, flagBits, fromKittyFlagBits, fromRgb, fromStyle, modeCode, readPalette, stringText, toCursorStyle, toRgb, unknownEnum, withSize, withString)
import Ghostty.Effectful.Internal.Error (expectSuccess, optionalAtWith, optionalVia, orInvariant)
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
  Cols -> required r
  Rows -> required r
  CursorX -> required r
  CursorY -> required r
  CursorPendingWrap -> flag r
  ActiveScreen ->
    required r >>= \case
      GhosttyTerminalScreenPrimary -> pure Primary
      GhosttyTerminalScreenAlternate -> pure Alternate
      GhosttyTerminalScreenUnknown n -> unknownEnum site ghosttyTerminalScreenEnum n
  CursorVisible -> flag r
  KittyKeyboardFlags -> required r >>= fromKittyFlagBits site
  Scrollbar -> (\(s :: GhosttyTerminalScrollbar) -> ScrollState s.total s.offset s.len) <$> required r
  CursorSgrStyle -> style r
  MouseTracking -> flag r
  Title -> string r
  Pwd -> string r
  TotalRows -> word r
  ScrollbackRows -> word r
  WidthPx -> required r
  HeightPx -> required r
  ColorForeground -> fmap fromRgb <$> maybeValue r
  ColorBackground -> fmap fromRgb <$> maybeValue r
  ColorCursor -> fmap fromRgb <$> maybeValue r
  ColorPalette -> palette r
  ColorForegroundDefault -> fmap fromRgb <$> maybeValue r
  ColorBackgroundDefault -> fmap fromRgb <$> maybeValue r
  ColorCursorDefault -> fmap fromRgb <$> maybeValue r
  ColorPaletteDefault -> palette r
  KittyImageStorageLimit -> maybeValue r
  KittyImageMediumFile -> fmap (toBool @CBool) <$> maybeValue r
  KittyImageMediumTempFile -> traverse stringText =<< maybeValue r
  KittyImageMediumSharedMem -> fmap (toBool @CBool) <$> maybeValue r
  ViewportActive -> flag r
  VtProcessingError -> flag r
  ScrollbackMaxBytes -> fmap toWord <$> maybeValue r
  ScrollbackMaxLines -> fmap toWord <$> maybeValue r
  ContinuationMaxBytes -> word r
  Mode m -> do
    c <- orInvariant site ResultNoValue =<< fetchInto r (GhosttyTerminalModeConfig {mode = modeCode m, value = 0})
    pure (toBool c.value)
  VtGround -> flag r
  CursorAtPrompt -> flag r
  ClipboardWriteMaxBytes -> word r
  MouseShape -> required r >>= mouseShape site
  MemoryUsage -> do
    fromMemoryUsage <$> (orInvariant site ResultNoValue =<< fetchInto r (withSize memoryUsage))
  where
    k = dataKey key
    site = getSite k
    r = KeyRead h k site

-- | One @ghostty_terminal_get@ read: the terminal, the key, and where its
-- errors come from (built only on failure). The readers below are top
-- level and take it, rather than local to 'getData': local, each of their
-- specialisations would be a closure 'getData' allocated on every call,
-- whichever key it read (about 400 bytes a read, which the cursor reads
-- after every prompt mark would pay).
data KeyRead = KeyRead GhosttyTerminal GhosttyTerminalData CallSite

-- | 'CursorX' and 'CursorY', read into the terminal's slot rather than
-- each into an 'Foreign.Marshal.Alloc.alloca' of its own: they are read
-- after every prompt mark ("Ghostty.Effectful.Internal.Feed").
-- The keys and their call sites are top-level constants, built once, not
-- a 'KeyRead' and a site for each read.
cursorAt :: Terminal s -> IO (Word16, Word16)
cursorAt t = unsafeWithForeignPtr t.cursorSlot $ \p -> do
  x <- requiredAt p t.handle cursorXKey cursorXSite
  y <- requiredAt p t.handle cursorYKey cursorYSite
  pure (x, y)
{-# INLINE cursorAt #-}

-- | The keys 'cursorAt' reads, and their call sites.
cursorXKey, cursorYKey :: GhosttyTerminalData
cursorXKey = dataKey CursorX
cursorYKey = dataKey CursorY

cursorXSite, cursorYSite :: CallSite
cursorXSite = getSite cursorXKey
cursorYSite = getSite cursorYKey
{-# NOINLINE cursorXSite #-}
{-# NOINLINE cursorYSite #-}

-- | 'required', into the given memory.
requiredAt :: (Storable b) => Ptr b -> GhosttyTerminal -> GhosttyTerminalData -> CallSite -> IO b
requiredAt p h k site =
  orInvariant site ResultNoValue =<< optionalAtWith p id ResultNoValue site (ghosttyTerminalGet h k . castPtr)
{-# INLINE requiredAt #-}

-- | The call site of @ghostty_terminal_get@ with the key.
getSite :: GhosttyTerminalData -> CallSite
getSite k = enumSite "ghostty_terminal_get" ghosttyTerminalDataEnum (coerce k)

-- | The read into memory set up by the caller; 'Nothing' on NO_VALUE.
fetchWith :: (Storable b) => KeyRead -> (Ptr b -> IO ()) -> IO (Maybe b)
fetchWith (KeyRead h k site) prepare = optionalVia ResultNoValue site $ \p -> do
  prepare p
  ghosttyTerminalGet h k (castPtr p)

-- | The read into a value of the key's struct, set up first.
fetchInto :: (Storable b) => KeyRead -> b -> IO (Maybe b)
fetchInto r v = fetchWith r (`poke` v)

-- | The read; 'Nothing' on NO_VALUE.
maybeValue :: (Storable b) => KeyRead -> IO (Maybe b)
maybeValue r = fetchWith r (const pass)

-- | The read, which the header says always has a value.
required :: (Storable b) => KeyRead -> IO b
required r@(KeyRead _ _ site) = orInvariant site ResultNoValue =<< maybeValue r

-- | A @bool@ read.
flag :: KeyRead -> IO Bool
flag r = toBool @CBool <$> required r

-- | A @size_t@ read.
word :: KeyRead -> IO Word
word r = toWord <$> required r

-- | A @size_t@ as a 'Word'.
toWord :: CSize -> Word
toWord = fromIntegral

-- | A string read, copied out.
string :: KeyRead -> IO Text
string r = stringText =<< required r

-- | A style read, into a style set to the default first.
style :: KeyRead -> IO Style
style r@(KeyRead _ _ site) = fromStyle site =<< orInvariant site ResultNoValue =<< fetchWith r ghosttyStyleDefault

-- | A palette read.
palette :: KeyRead -> IO Palette
palette (KeyRead h k site) = readPalette site (ghosttyTerminalGet h k . castPtr)

-- | A mouse shape from the binding's.
mouseShape :: CallSite -> GhosttyMouseShape -> IO MouseShape
mouseShape site = \case
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
  SetKittyImageMediumFile -> putFlag v
  SetKittyImageMediumTempFile -> maybeWith text v put
  SetKittyImageMediumSharedMem -> putFlag v
  SetApcMaxBytes -> maybeWith with (toCSize <$> v) put
  SetApcMaxBytesKitty -> maybeWith with (toCSize <$> v) put
  SetDefaultCursorStyle -> with (toCursorStyle v) put
  SetDefaultCursorBlink -> putFlag v
  SetGlyphProtocol -> putFlag v
  -- No byte limit is SIZE_MAX: since libghostty-vt 1.0 a null value
  -- restores the default of 10,000 bytes (regression 11).
  SetScrollbackMaxBytes -> with (maybe maxBound toCSize v) put
  SetScrollbackMaxLines -> maybeWith with (toCSize <$> v) put
  SetContinuationMaxBytes -> with (toCSize v) put
  SetTitleReport -> putFlag v
  SetModeDefault m -> modeValue m v
  SetMode ModeSynchronizedOutput -> do
    modeValue ModeSynchronizedOutput v
    unless v $ captureRelease t.installed
  SetMode m -> modeValue m v
  SetUnknownMaxBytes -> with (toCSize v) put
  SetTerminfoName -> text (fromTerminfoName v) put
  SetClipboardWriteMaxBytes -> with (toCSize v) put
  SetResizePullScrollback -> putFlag v
  SetXtChecksumReport -> putFlag v
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

    putFlag :: Bool -> IO ()
    putFlag b = with (fromBool @CBool b) put

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
