{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Formatting a terminal's content: the options and the calls (DESIGN.md
-- §4.8). The public API is "Ghostty.Effectful.Format".
--
-- A formatter is not a handle at this level. Each format makes one, formats
-- once, and frees it, inside one call, for two reasons. A formatter reads its
-- terminal's state at each format, so keeping one buys nothing but a borrowed
-- pointer to keep alive. And a range is a pair of untracked grid references,
-- valid only until the next call that changes the terminal: resolving the
-- range's points and formatting in the same call is what keeps them valid.
--
-- A new formatter starts with the library's defaults, which are not
-- canopy's (it trims and emits the palette, cursor style and hyperlink as
-- extras). So every option is set on every formatter made, and none is left
-- to the library (regression 10).
module Ghostty.Effectful.Internal.Format
  ( -- * Options
    Format (..),
    FormatOptions (..),
    formatOptions,
    TerminalExtras (..),
    noTerminalExtras,
    ScreenExtras (..),
    noScreenExtras,

    -- * Ranges
    Range (..),

    -- * Calls
    formatTerminal,
    formatTerminalRange,
  )
where

import Control.Exception (bracket, finally)
import Data.ByteString qualified as BS
import Foreign.C.Types (CBool)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool, with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable, peek, poke)
import Ghostty.Effectful.Error (CallSite (..), ResultCode (..))
import Ghostty.Effectful.Internal.Convert (emptyRef, flagBits, toPoint)
import Ghostty.Effectful.Internal.Error (expectSuccess, newHandle, optionalVia)
import Ghostty.Effectful.Internal.Keys (getData)
import Ghostty.Effectful.Internal.Terminal (Terminal (..), unlessPastHistory)
import Ghostty.Effectful.Point (KnownCoord, Point (..))
import Ghostty.Effectful.Terminal.Keys (TerminalData (..))
import Ghostty.FFI.Allocator (ghosttyFree)
import Ghostty.FFI.Formatter
  ( GhosttyFormatter,
    GhosttyFormatterExtras,
    GhosttyFormatterOption,
    ghosttyFormatterFormatAlloc,
    ghosttyFormatterFree,
    ghosttyFormatterSet,
    ghosttyFormatterTerminalNew,
    pattern GhosttyFormatterExtraCharsets,
    pattern GhosttyFormatterExtraCursor,
    pattern GhosttyFormatterExtraHyperlink,
    pattern GhosttyFormatterExtraKeyboard,
    pattern GhosttyFormatterExtraKittyKeyboard,
    pattern GhosttyFormatterExtraModes,
    pattern GhosttyFormatterExtraPalette,
    pattern GhosttyFormatterExtraProtection,
    pattern GhosttyFormatterExtraPwd,
    pattern GhosttyFormatterExtraScrollingRegion,
    pattern GhosttyFormatterExtraStyle,
    pattern GhosttyFormatterExtraTabstops,
    pattern GhosttyFormatterOptEmit,
    pattern GhosttyFormatterOptExtras,
    pattern GhosttyFormatterOptSelection,
    pattern GhosttyFormatterOptTrim,
    pattern GhosttyFormatterOptUnwrap,
  )
import Ghostty.FFI.GridRef (GhosttyGridRef)
import Ghostty.FFI.Selection (GhosttySelection (..))
import Ghostty.FFI.Terminal (ghosttyTerminalGridRef)
import Ghostty.FFI.Types
  ( GhosttyFormatterFormat,
    pattern GhosttyFormatterFormatHtml,
    pattern GhosttyFormatterFormatPlain,
    pattern GhosttyFormatterFormatVt,
  )

-- | What a format emits.
data Format
  = -- | The text alone.
    Plain
  | -- | VT sequences that redraw it with its styles, hyperlinks and, as
    -- the extras ask, the terminal's state.
    Vt
  | -- | HTML with inline styles.
    Html
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | How to format.
data FormatOptions = FormatOptions
  { -- | the output format
    emit :: Format,
    -- | join soft-wrapped rows into one line
    unwrap :: Bool,
    -- | drop trailing whitespace from each line that is not blank. Trailing
    -- blank lines are always dropped, except from 'Vt' output without
    -- trimming, which keeps them as part of the layout.
    trim :: Bool,
    -- | the terminal state to emit beside the content, in 'Vt' output
    extras :: TerminalExtras
  }
  deriving stock (Eq, Show)

-- | The given format, with soft wraps kept, trailing whitespace trimmed,
-- and no extras.
formatOptions :: Format -> FormatOptions
formatOptions f = FormatOptions {emit = f, unwrap = False, trim = True, extras = noTerminalExtras}

-- | Terminal state that 'Vt' output can carry, each as the sequence that
-- sets it.
data TerminalExtras = TerminalExtras
  { -- | the palette (OSC 4)
    palette :: Bool,
    -- | the modes that differ from their defaults (CSI h, CSI l)
    modes :: Bool,
    -- | the scrolling region (DECSTBM, DECSLRM)
    scrollingRegion :: Bool,
    -- | the tab stops
    tabstops :: Bool,
    -- | the working directory (OSC 7)
    pwd :: Bool,
    -- | keyboard modes such as modifyOtherKeys
    keyboard :: Bool,
    -- | the screen's state
    screen :: ScreenExtras
  }
  deriving stock (Eq, Show)

-- | No terminal state.
noTerminalExtras :: TerminalExtras
noTerminalExtras =
  TerminalExtras
    { palette = False,
      modes = False,
      scrollingRegion = False,
      tabstops = False,
      pwd = False,
      keyboard = False,
      screen = noScreenExtras
    }

-- | Screen state that 'Vt' output can carry.
data ScreenExtras = ScreenExtras
  { -- | the cursor's position (CUP)
    cursor :: Bool,
    -- | the cursor's current style (SGR)
    style :: Bool,
    -- | the open hyperlink (OSC 8)
    hyperlink :: Bool,
    -- | character protection (DECSCA)
    protection :: Bool,
    -- | the kitty keyboard flags
    kittyKeyboard :: Bool,
    -- | the character set designations and invocations
    charsets :: Bool
  }
  deriving stock (Eq, Show)

-- | No screen state.
noScreenExtras :: ScreenExtras
noScreenExtras =
  ScreenExtras
    { cursor = False,
      style = False,
      hyperlink = False,
      protection = False,
      kittyKeyboard = False,
      charsets = False
    }

-- | Part of a terminal's active screen, in the coordinate system @c@. Both
-- ends are included, in either order.
data Range c
  = -- | Every cell from one point to the other in reading order, as a
    -- mouse selection runs.
    Span (Point c) (Point c)
  | -- | The rectangle with the two points at opposite corners.
    Block (Point c) (Point c)
  | -- | Whole rows, from one to the other.
    WholeRows Word32 Word32
  deriving stock (Eq, Show)

-- | @ghostty_formatter_terminal_new@, @ghostty_formatter_set@ for every
-- option, then @ghostty_formatter_format_alloc@: the terminal's active
-- screen, scrollback included, or the selection when the pointer is not null
-- (the formatter copies it).
formatWith :: Terminal s -> FormatOptions -> Ptr GhosttySelection -> IO ByteString
formatWith t o sel =
  bracket newFormatter ghosttyFormatterFree $ \f -> do
    setOption f GhosttyFormatterOptEmit (formatCode o.emit)
    setOption f GhosttyFormatterOptUnwrap (fromBool o.unwrap :: CBool)
    setOption f GhosttyFormatterOptTrim (fromBool o.trim :: CBool)
    setOption f GhosttyFormatterOptExtras (extrasMask o.extras)
    -- Null is the default, no selection: the whole screen.
    expectSuccess setSite =<< ghosttyFormatterSet f GhosttyFormatterOptSelection (castPtr sel)
    alloca $ \outPtr -> alloca $ \outLen -> do
      expectSuccess (CallSite "ghostty_formatter_format_alloc")
        =<< ghosttyFormatterFormatAlloc f nullPtr outPtr outLen
      p <- peek outPtr
      n <- peek outLen
      -- Empty output is a null pointer, which ghostty_free accepts.
      BS.packCStringLen (castPtr p, fromIntegral n) `finally` ghosttyFree nullPtr p n
  where
    newFormatter =
      newHandle (CallSite "ghostty_formatter_terminal_new") $ \a out ->
        ghosttyFormatterTerminalNew a out t.handle

-- | @ghostty_formatter_set@ from a value of the option's type. Every value
-- this module passes is valid for its option, so any failure is an
-- invariant.
setOption :: (Storable a) => GhosttyFormatter -> GhosttyFormatterOption -> a -> IO ()
setOption f key v = with v $ \vp -> expectSuccess setSite =<< ghosttyFormatterSet f key (castPtr vp)

-- | Where a formatter option's errors come from.
setSite :: CallSite
setSite = CallSite "ghostty_formatter_set"

-- | The whole active screen, scrollback included.
formatTerminal :: Terminal s -> FormatOptions -> IO ByteString
formatTerminal t o = formatWith t o nullPtr

-- | A range of the active screen; 'Nothing' if an end is outside its
-- coordinate system. Its points are resolved to grid references and
-- formatted in this one call, with nothing between that could change the
-- terminal, which is how long an untracked reference is valid.
formatTerminalRange :: (KnownCoord c) => Terminal s -> FormatOptions -> Range c -> IO (Maybe ByteString)
formatTerminalRange t o range = do
  ends <- case range of
    Span a b -> pure (a, b, False)
    Block a b -> pure (a, b, True)
    WholeRows a b -> do
      cols <- getData t.handle Cols
      -- The span runs from the first cell of the upper row to the last of
      -- the lower, whichever order the rows were given in.
      pure (Point {col = 0, row = min a b}, Point {col = cols - 1, row = max a b}, False)
  let (from, to, rectangle) = ends
  runMaybeT $ do
    start <- MaybeT (gridRef t from)
    end <- MaybeT (gridRef t to)
    lift (with GhosttySelection {start, end, rectangle = fromBool rectangle} (formatWith t o))

-- | @ghostty_terminal_grid_ref@: the untracked reference to the cell at a
-- point; 'Nothing' if the point is outside its coordinate system, which is
-- the header's @GHOSTTY_INVALID_VALUE@ for a valid terminal, or a scrollback
-- row past the scrollback, which the library does not refuse ('pastHistory').
gridRef :: (KnownCoord c) => Terminal s -> Point c -> IO (Maybe GhosttyGridRef)
gridRef t p =
  unlessPastHistory t.handle point Nothing $
    with point $ \pp ->
      optionalVia ResultInvalidValue (CallSite "ghostty_terminal_grid_ref") $ \rp ->
        poke rp emptyRef >> ghosttyTerminalGridRef t.handle pp rp
  where
    point = toPoint p

-- | The library's extras: a bit per field of 'TerminalExtras' and its
-- 'ScreenExtras'.
extrasMask :: TerminalExtras -> GhosttyFormatterExtras
extrasMask e =
  flagBits
    [ (e.palette, GhosttyFormatterExtraPalette),
      (e.modes, GhosttyFormatterExtraModes),
      (e.scrollingRegion, GhosttyFormatterExtraScrollingRegion),
      (e.tabstops, GhosttyFormatterExtraTabstops),
      (e.pwd, GhosttyFormatterExtraPwd),
      (e.keyboard, GhosttyFormatterExtraKeyboard),
      (e.screen.cursor, GhosttyFormatterExtraCursor),
      (e.screen.style, GhosttyFormatterExtraStyle),
      (e.screen.hyperlink, GhosttyFormatterExtraHyperlink),
      (e.screen.protection, GhosttyFormatterExtraProtection),
      (e.screen.kittyKeyboard, GhosttyFormatterExtraKittyKeyboard),
      (e.screen.charsets, GhosttyFormatterExtraCharsets)
    ]

-- | The library's output format.
formatCode :: Format -> GhosttyFormatterFormat
formatCode = \case
  Plain -> GhosttyFormatterFormatPlain
  Vt -> GhosttyFormatterFormatVt
  Html -> GhosttyFormatterFormatHtml
