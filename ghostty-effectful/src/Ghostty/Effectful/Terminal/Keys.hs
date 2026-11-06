-- | A terminal's keyed data and options, typed (DESIGN.md §4.4): the
-- header's tables of @GhosttyTerminalData@ and @GhosttyTerminalOption@
-- become GADTs, so the result of a read and the value of a write follow
-- from the key. Read with 'Ghostty.Effectful.Terminal.getT', write with
-- 'Ghostty.Effectful.Terminal.setT'.
--
-- There is one constructor per header row, except these, which a test holds
-- against the header:
--
-- * the callback options and @USERDATA@: the binding installs those itself,
--   and never lets a caller (§4.5). The callbacks that answer a query, or
--   reply to a request, answer from the terminal's configuration instead, and
--   each is an option here that sets the answer: 'SetColorScheme',
--   'SetDeviceAttributes', 'SetEnquiry', 'SetXtversion', 'SetSize' and
--   'SetClipboardWrite'. @CLIPBOARD_READ@ is never installed, so a program's
--   clipboard read is answered as empty;
-- * @SELECTION@, both ways: its value is a pair of grid references, which
--   arrives with the selection API (PLAN.md M6.5);
-- * the @KITTY_GRAPHICS@ data key: a borrowed handle, which arrives with
--   image placements (PLAN.md M6.15).
--
-- Where the header says a read may have no value, the result is a 'Maybe'.
-- Where an option's null pointer means something no value of the type
-- says, the option takes a 'Maybe' and 'Nothing' is the null.
module Ghostty.Effectful.Terminal.Keys
  ( -- * Data
    TerminalData (..),

    -- * Options
    TerminalOption (..),

    -- * Values
    ColorScheme (..),
    DeviceAttributes (..),
    defaultDeviceAttributes,
    Screen (..),
    CursorShape (..),
    KittyFlags (..),
    noKittyFlags,
    ScrollState (..),
    Memory (..),
    ScreenMemory (..),
    MouseShape (..),
    ChecksumExtension (..),
    decChecksum,
    TerminfoName,
    terminfoName,
    fromTerminfoName,
  )
where

import Data.ByteString qualified as BS
import Ghostty.Effectful.Color (Palette, Rgb)
import Ghostty.Effectful.Mode (ModeKey, Resettable (..))
import Ghostty.Effectful.Style (Style)

-- | What can be read from a terminal, and the type each read returns.
data TerminalData a where
  -- | Width in cells.
  Cols :: TerminalData Word16
  -- | Height in cells.
  Rows :: TerminalData Word16
  -- | The cursor's column, from 0.
  CursorX :: TerminalData Word16
  -- | The cursor's row in the active area, from 0.
  CursorY :: TerminalData Word16
  -- | Whether the next printed character soft-wraps first.
  CursorPendingWrap :: TerminalData Bool
  -- | Which screen is active.
  ActiveScreen :: TerminalData Screen
  -- | Whether the cursor is visible (DEC mode 25).
  CursorVisible :: TerminalData Bool
  -- | The kitty keyboard protocol's current flags.
  KittyKeyboardFlags :: TerminalData KittyFlags
  -- | The viewport's place in the scrollable area.
  Scrollbar :: TerminalData ScrollState
  -- | The pen: the style newly printed characters get.
  CursorStyle :: TerminalData Style
  -- | Whether any mouse tracking mode is on.
  MouseTracking :: TerminalData Bool
  -- | The title set by OSC 0 or 2; empty if none.
  Title :: TerminalData Text
  -- | The working directory set by OSC 7 (and kin); empty if none.
  Pwd :: TerminalData Text
  -- | Rows of the active screen, scrollback included.
  TotalRows :: TerminalData Word
  -- | Rows of scrollback.
  ScrollbackRows :: TerminalData Word
  -- | Width in pixels: columns times the cell width.
  WidthPx :: TerminalData Word32
  -- | Height in pixels: rows times the cell height.
  HeightPx :: TerminalData Word32
  -- | The foreground in effect (an OSC override, or the default), if any.
  ColorForeground :: TerminalData (Maybe Rgb)
  -- | The background in effect, if any.
  ColorBackground :: TerminalData (Maybe Rgb)
  -- | The cursor colour in effect, if any.
  ColorCursor :: TerminalData (Maybe Rgb)
  -- | The palette in effect.
  ColorPalette :: TerminalData Palette
  -- | The default foreground, ignoring OSC overrides, if any.
  ColorForegroundDefault :: TerminalData (Maybe Rgb)
  -- | The default background, ignoring OSC overrides, if any.
  ColorBackgroundDefault :: TerminalData (Maybe Rgb)
  -- | The default cursor colour, ignoring OSC overrides, if any.
  ColorCursorDefault :: TerminalData (Maybe Rgb)
  -- | The default palette, ignoring OSC overrides.
  ColorPaletteDefault :: TerminalData Palette
  -- | The kitty image storage limit in bytes (0: images off); 'Nothing' when
  -- the library was built without kitty graphics.
  KittyImageStorageLimit :: TerminalData (Maybe Word64)
  -- | Whether kitty images may load from files; 'Nothing' without kitty
  -- graphics.
  KittyImageMediumFile :: TerminalData (Maybe Bool)
  -- | The directory kitty images may load temporary files from, empty if
  -- none; 'Nothing' without kitty graphics.
  KittyImageMediumTempFile :: TerminalData (Maybe Text)
  -- | Whether kitty images may load from shared memory; 'Nothing' without
  -- kitty graphics.
  KittyImageMediumSharedMem :: TerminalData (Maybe Bool)
  -- | Whether the viewport follows the active area (not scrolled back).
  ViewportActive :: TerminalData Bool
  -- | Whether VT processing has ever failed in a way that may have lost an
  -- update. Never cleared.
  VtProcessingError :: TerminalData Bool
  -- | The scrollback limit in bytes; 'Nothing' if unlimited.
  ScrollbackMaxBytes :: TerminalData (Maybe Word)
  -- | The scrollback limit in lines; 'Nothing' if unlimited.
  ScrollbackMaxLines :: TerminalData (Maybe Word)
  -- | The most continuation bytes kept (0: tracking off).
  ContinuationMaxBytes :: TerminalData Word
  -- | A mode's current value.
  Mode :: ModeKey r -> TerminalData Bool
  -- | Whether the VT stream is at ground: in no sequence, so bytes of one's
  -- own can be inserted safely.
  VtGround :: TerminalData Bool
  -- | Whether the cursor is at a semantic prompt or its input (OSC 133).
  CursorAtPrompt :: TerminalData Bool
  -- | The most decoded bytes one kitty clipboard write may hold.
  ClipboardWriteMaxBytes :: TerminalData Word
  -- | The pointer shape the program asked for (OSC 22).
  MouseShape :: TerminalData MouseShape
  -- | How much memory the terminal holds. Looks at every page: not for
  -- every write.
  MemoryUsage :: TerminalData Memory

deriving stock instance Show (TerminalData a)

-- | What can be written to a terminal, and the type of each value.
data TerminalOption a where
  -- | The title; empty clears it.
  SetTitle :: TerminalOption Text
  -- | The working directory; empty clears it.
  SetPwd :: TerminalOption Text
  -- | The default foreground; 'Nothing' unsets it.
  SetColorForeground :: TerminalOption (Maybe Rgb)
  -- | The default background; 'Nothing' unsets it.
  SetColorBackground :: TerminalOption (Maybe Rgb)
  -- | The default cursor colour; 'Nothing' unsets it.
  SetColorCursor :: TerminalOption (Maybe Rgb)
  -- | The default palette; 'Nothing' restores the built-in one.
  SetColorPalette :: TerminalOption (Maybe Palette)
  -- | The kitty image storage limit in bytes; 0 turns kitty graphics off
  -- and deletes every image.
  SetKittyImageStorageLimit :: TerminalOption Word64
  -- | Whether kitty images may load from files.
  SetKittyImageMediumFile :: TerminalOption Bool
  -- | The one directory kitty images may load temporary files from;
  -- 'Nothing' turns that medium off.
  SetKittyImageMediumTempFile :: TerminalOption (Maybe Text)
  -- | Whether kitty images may load from shared memory.
  SetKittyImageMediumSharedMem :: TerminalOption Bool
  -- | The most bytes the APC handler buffers, for every protocol;
  -- 'Nothing' restores every built-in limit.
  SetApcMaxBytes :: TerminalOption (Maybe Word)
  -- | The most bytes the APC handler buffers for kitty graphics; 'Nothing'
  -- restores the built-in limit.
  SetApcMaxBytesKitty :: TerminalOption (Maybe Word)
  -- | The cursor shape DECSCUSR 0 resets to.
  SetDefaultCursorStyle :: TerminalOption CursorShape
  -- | Whether the cursor DECSCUSR 0 resets to blinks.
  SetDefaultCursorBlink :: TerminalOption Bool
  -- | Whether Glyph Protocol APC sequences are handled. Turning it off
  -- clears the glyph glossary.
  SetGlyphProtocol :: TerminalOption Bool
  -- | The scrollback limit in bytes; 'Nothing' removes it. Pruned by page,
  -- so approximate.
  SetScrollbackMaxBytes :: TerminalOption (Maybe Word)
  -- | The scrollback limit in lines; 'Nothing' removes it. Pruned by page,
  -- so approximate.
  SetScrollbackMaxLines :: TerminalOption (Maybe Word)
  -- | The most continuation bytes kept; 0 turns tracking off.
  SetContinuationMaxBytes :: TerminalOption Word
  -- | Whether CSI 21 t reports the title. Off by default: a program could
  -- set a title and read it back as input.
  SetTitleReport :: TerminalOption Bool
  -- | A mode's reset default, and its current value. Only a mode whose
  -- reset default may be configured.
  SetModeDefault :: ModeKey 'Configurable -> TerminalOption Bool
  -- | A mode's current value; its reset default is unchanged.
  SetMode :: ModeKey r -> TerminalOption Bool
  -- | The most bytes of an unsupported sequence kept for reporting; 0
  -- turns reporting off.
  SetUnknownMaxBytes :: TerminalOption Word
  -- | The terminfo name XTGETTCAP reports for @TN@.
  SetTerminfoName :: TerminalOption TerminfoName
  -- | The most decoded bytes one kitty clipboard write may hold; 'maxBound'
  -- removes the limit.
  SetClipboardWriteMaxBytes :: TerminalOption Word
  -- | Whether a resize may pull rows back out of scrollback. Survives a
  -- full reset.
  SetResizePullScrollback :: TerminalOption Bool
  -- | Whether DECRQCRA reports checksums. Off by default: a program could
  -- read the whole screen back a cell at a time.
  SetXtChecksumReport :: TerminalOption Bool
  -- | How DECRQCRA checksums are computed, now and after a reset.
  SetXtChecksumExtension :: TerminalOption ChecksumExtension
  -- | The answer to a colour scheme query (CSI ? 996 n); 'Nothing' leaves
  -- the query unanswered.
  SetColorScheme :: TerminalOption (Maybe ColorScheme)
  -- | The answers to device attribute queries (DA1, DA2, DA3).
  SetDeviceAttributes :: TerminalOption DeviceAttributes
  -- | The reply to ENQ; empty sends none.
  SetEnquiry :: TerminalOption ByteString
  -- | The name XTVERSION reports; empty reports the library's own,
  -- @libghostty@.
  SetXtversion :: TerminalOption Text
  -- | Whether size queries (XTWINOPS 14, 16, 18, 19 and in-band reports,
  -- mode 2048) are answered, from the terminal's grid and cell size.
  SetSize :: TerminalOption Bool
  -- | Whether clipboard writes are accepted: each then arrives as a
  -- 'Ghostty.Effectful.Event.ClipboardWrite', and the program is told it
  -- succeeded. Refused, a write is dropped, and a program that asks for a
  -- reply (OSC 5522) is told it was denied.
  SetClipboardWrite :: TerminalOption Bool

deriving stock instance Show (TerminalOption a)

-- | A colour scheme, as a terminal reports it.
data ColorScheme
  = -- | dark text on a light background
    Light
  | -- | light text on a dark background
    Dark
  deriving stock (Eq, Show, Enum, Bounded)

-- | What a terminal reports itself to be (DA1, DA2, DA3).
data DeviceAttributes = DeviceAttributes
  { -- | DA1's conformance level (62 for VT220, 65 for VT520)
    conformanceLevel :: Word16,
    -- | DA1's feature codes (22 for ANSI colour, 52 for the clipboard); the
    -- first 64 are reported
    features :: [Word16],
    -- | DA2's terminal type (1 for VT220)
    deviceType :: Word16,
    -- | DA2's firmware version
    firmwareVersion :: Word16,
    -- | DA2's ROM cartridge number, 0 for an emulator
    romCartridge :: Word16,
    -- | DA3's unit ID
    unitId :: Word32
  }
  deriving stock (Eq, Show)

-- | What the library reports with no answer configured: a VT220 with ANSI
-- colour, firmware 0, unit 0.
defaultDeviceAttributes :: DeviceAttributes
defaultDeviceAttributes =
  DeviceAttributes
    { conformanceLevel = 62,
      features = [22],
      deviceType = 1,
      firmwareVersion = 0,
      romCartridge = 0,
      unitId = 0
    }

-- | A terminal's two screens.
data Screen
  = -- | the primary screen, which has scrollback
    Primary
  | -- | the alternate screen, which full-screen programs use
    Alternate
  deriving stock (Eq, Show, Enum, Bounded)

-- | A cursor's shape.
data CursorShape
  = -- | a bar
    BarCursor
  | -- | a block
    BlockCursor
  | -- | an underline
    UnderlineCursor
  | -- | a hollow block
    HollowBlockCursor
  deriving stock (Eq, Show, Enum, Bounded)

-- | The kitty keyboard protocol's progressive enhancement flags.
data KittyFlags = KittyFlags
  { -- | 1: disambiguate escape codes
    disambiguate :: Bool,
    -- | 2: report event types
    reportEvents :: Bool,
    -- | 4: report alternate keys
    reportAlternates :: Bool,
    -- | 8: report all keys as escape codes
    reportAll :: Bool,
    -- | 16: report associated text
    reportAssociated :: Bool
  }
  deriving stock (Eq, Show)

-- | Every flag off: the legacy encoding.
noKittyFlags :: KittyFlags
noKittyFlags = KittyFlags False False False False False

-- | The viewport's place in the scrollable area, in rows.
data ScrollState = ScrollState
  { -- | the whole scrollable area
    total :: Word64,
    -- | where the viewport starts in it
    offset :: Word64,
    -- | the viewport's length
    len :: Word64
  }
  deriving stock (Eq, Show)

-- | The memory a terminal holds.
data Memory = Memory
  { -- | Whether compressing scrollback can free memory here.
    compressionSupported :: Bool,
    -- | the primary screen's
    primary :: ScreenMemory,
    -- | the alternate screen's
    alternate :: ScreenMemory
  }
  deriving stock (Eq, Show)

-- | The memory one screen holds.
data ScreenMemory = ScreenMemory
  { -- | pages, compressed ones included
    pages :: Word64,
    -- | address space reserved for the pages; at least 'residentBytes'
    virtualBytes :: Word64,
    -- | physical memory the pages use, a compressed page at its compressed
    -- size: the figure for budgets
    residentBytes :: Word64,
    -- | pages that are compressed
    compressedPages :: Word64,
    -- | bytes of compressed data, already in 'residentBytes'
    compressedBytes :: Word64,
    -- | bytes of kitty image data, not in 'residentBytes'
    imageBytes :: Word64
  }
  deriving stock (Eq, Show)

-- | A pointer shape (OSC 22), as CSS names them.
data MouseShape
  = -- | @default@
    MouseDefault
  | -- | @context-menu@
    MouseContextMenu
  | -- | @help@
    MouseHelp
  | -- | @pointer@
    MousePointer
  | -- | @progress@
    MouseProgress
  | -- | @wait@
    MouseWait
  | -- | @cell@
    MouseCell
  | -- | @crosshair@
    MouseCrosshair
  | -- | @text@
    MouseText
  | -- | @vertical-text@
    MouseVerticalText
  | -- | @alias@
    MouseAlias
  | -- | @copy@
    MouseCopy
  | -- | @move@
    MouseMove
  | -- | @no-drop@
    MouseNoDrop
  | -- | @not-allowed@
    MouseNotAllowed
  | -- | @grab@
    MouseGrab
  | -- | @grabbing@
    MouseGrabbing
  | -- | @all-scroll@
    MouseAllScroll
  | -- | @col-resize@
    MouseColResize
  | -- | @row-resize@
    MouseRowResize
  | -- | @n-resize@
    MouseNResize
  | -- | @e-resize@
    MouseEResize
  | -- | @s-resize@
    MouseSResize
  | -- | @w-resize@
    MouseWResize
  | -- | @ne-resize@
    MouseNeResize
  | -- | @nw-resize@
    MouseNwResize
  | -- | @se-resize@
    MouseSeResize
  | -- | @sw-resize@
    MouseSwResize
  | -- | @ew-resize@
    MouseEwResize
  | -- | @ns-resize@
    MouseNsResize
  | -- | @nesw-resize@
    MouseNeswResize
  | -- | @nwse-resize@
    MouseNwseResize
  | -- | @zoom-in@
    MouseZoomIn
  | -- | @zoom-out@
    MouseZoomOut
  deriving stock (Eq, Show, Enum, Bounded)

-- | How a DECRQCRA checksum is computed: XTCHECKSUM's bits, as xterm's
-- @checksumExtension@ names them.
data ChecksumExtension = ChecksumExtension
  { -- | 1: don't negate the result
    dontNegate :: Bool,
    -- | 2: don't add each cell's video attributes
    dontAddAttributes :: Bool,
    -- | 4: don't omit blanks
    dontOmitBlanks :: Bool,
    -- | 8: count cells never written to as spaces
    unwrittenAsSpaces :: Bool,
    -- | 16: full codepoints rather than DEC's 8-bit values
    fullCodepoints :: Bool
  }
  deriving stock (Eq, Show)

-- | Every bit clear: a real DEC terminal's checksum.
decChecksum :: ChecksumExtension
decChecksum = ChecksumExtension False False False False False

-- | A terminfo entry's name (@xterm-256color@): at most 128 bytes of UTF-8,
-- which the library requires.
newtype TerminfoName = TerminfoName Text
  deriving stock (Eq, Show)

-- | The name, if it is at most 128 bytes as UTF-8. Empty clears it.
--
-- >>> terminfoName "xterm-256color"
-- Just (TerminfoName "xterm-256color")
-- >>> terminfoName (toText (replicate 129 'x'))
-- Nothing
terminfoName :: Text -> Maybe TerminfoName
terminfoName t
  | BS.length (encodeUtf8 t) <= 128 = Just (TerminfoName t)
  | otherwise = Nothing

-- | The name as text.
fromTerminfoName :: TerminfoName -> Text
fromTerminfoName (TerminfoName t) = t
