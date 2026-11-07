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

import Control.Exception (bracket, finally, throwIO)
import Data.ByteString qualified as BS
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool, with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), ResultCode (..))
import Ghostty.Effectful.Internal.Convert (refTemplate, toPoint, withSize)
import Ghostty.Effectful.Internal.Error (expectSuccess, resultCode)
import Ghostty.Effectful.Internal.Keys (getData)
import Ghostty.Effectful.Internal.Terminal (Terminal (..), pastHistory)
import Ghostty.Effectful.Point (KnownCoord, Point (..))
import Ghostty.Effectful.Terminal.Keys (TerminalData (..))
import Ghostty.FFI.Allocator (ghosttyFree)
import Ghostty.FFI.Formatter
  ( GhosttyFormatterScreenExtra (..),
    GhosttyFormatterTerminalExtra (..),
    GhosttyFormatterTerminalOptions (..),
    ghosttyFormatterFormatAlloc,
    ghosttyFormatterFree,
    ghosttyFormatterTerminalNew,
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
  deriving stock (Eq, Show, Enum, Bounded)

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

-- | @ghostty_formatter_terminal_new@, then @ghostty_formatter_format_alloc@:
-- the terminal's active screen, scrollback included, or the selection when
-- the pointer is not null.
formatWith :: Terminal s -> FormatOptions -> Ptr GhosttySelection -> IO ByteString
formatWith t o sel =
  with (terminalOptions o sel) $ \op ->
    bracket (newFormatter op) ghosttyFormatterFree $ \f ->
      alloca $ \outPtr -> alloca $ \outLen -> do
        expectSuccess (CallSite "ghostty_formatter_format_alloc")
          =<< ghosttyFormatterFormatAlloc f nullPtr outPtr outLen
        p <- peek outPtr
        n <- peek outLen
        -- Empty output is a null pointer, which ghostty_free accepts.
        BS.packCStringLen (castPtr p, fromIntegral n) `finally` ghosttyFree nullPtr p n
  where
    newFormatter op = alloca $ \out -> do
      expectSuccess (CallSite "ghostty_formatter_terminal_new")
        =<< ghosttyFormatterTerminalNew nullPtr out t.handle op
      peek out

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
  gridRef t from >>= \case
    Nothing -> pure Nothing
    Just start ->
      gridRef t to >>= \case
        Nothing -> pure Nothing
        Just end ->
          let sel = withSize $ \size -> GhosttySelection {size, start, end, rectangle = fromBool rectangle}
           in Just <$> with sel (formatWith t o)

-- | @ghostty_terminal_grid_ref@: the untracked reference to the cell at a
-- point; 'Nothing' if the point is outside its coordinate system, which is
-- the header's @GHOSTTY_INVALID_VALUE@ for a valid terminal, or a scrollback
-- row past the scrollback, which the library does not refuse ('pastHistory').
gridRef :: (KnownCoord c) => Terminal s -> Point c -> IO (Maybe GhosttyGridRef)
gridRef t p =
  pastHistory t.handle point >>= \case
    True -> pure Nothing
    False -> with point $ \pp -> with (withSize refTemplate) $ \rp -> do
      r <- ghosttyTerminalGridRef t.handle pp rp
      case resultCode r of
        ResultSuccess -> Just <$> peek rp
        ResultInvalidValue -> pure Nothing
        other -> throwIO (Invariant (CallSite "ghostty_terminal_grid_ref") other)
  where
    point = toPoint p

-- | The library's options, the selection pointer included.
terminalOptions :: FormatOptions -> Ptr GhosttySelection -> GhosttyFormatterTerminalOptions
terminalOptions o sel = withSize $ \size ->
  GhosttyFormatterTerminalOptions
    { size,
      emit = formatCode o.emit,
      unwrap = fromBool o.unwrap,
      trim = fromBool o.trim,
      extra = terminalExtra o.extras,
      selection = sel
    }

-- | The library's terminal extras.
terminalExtra :: TerminalExtras -> GhosttyFormatterTerminalExtra
terminalExtra e = withSize $ \size ->
  GhosttyFormatterTerminalExtra
    { size,
      palette = fromBool e.palette,
      modes = fromBool e.modes,
      scrollingRegion = fromBool e.scrollingRegion,
      tabstops = fromBool e.tabstops,
      pwd = fromBool e.pwd,
      keyboard = fromBool e.keyboard,
      screen = screenExtra e.screen
    }

-- | The library's screen extras.
screenExtra :: ScreenExtras -> GhosttyFormatterScreenExtra
screenExtra e = withSize $ \size ->
  GhosttyFormatterScreenExtra
    { size,
      cursor = fromBool e.cursor,
      style = fromBool e.style,
      hyperlink = fromBool e.hyperlink,
      protection = fromBool e.protection,
      kittyKeyboard = fromBool e.kittyKeyboard,
      charsets = fromBool e.charsets
    }

-- | The library's output format.
formatCode :: Format -> GhosttyFormatterFormat
formatCode = \case
  Plain -> GhosttyFormatterFormatPlain
  Vt -> GhosttyFormatterFormatVt
  Html -> GhosttyFormatterFormatHtml
