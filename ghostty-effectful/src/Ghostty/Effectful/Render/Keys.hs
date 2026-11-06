-- | The render state's tables as GADTs (DESIGN.md §4.3, §4.4): a frame's
-- data and a row's data, each read at the type that follows from its key,
-- and the plain values they return.
--
-- A constructor is named for its header row with a prefix:
-- @GHOSTTY_RENDER_STATE_DATA_CURSOR_VISIBLE@ is 'RenderCursorVisible', and
-- @GHOSTTY_RENDER_STATE_ROW_DATA_VIEWPORT_Y@ is 'RowViewportY'. A test holds
-- each against the number its marshalling sends, and the set of them, with
-- the rows deliberately left out, against the header.
module Ghostty.Effectful.Render.Keys
  ( -- * Frame data
    FrameData (..),
    Dirty (..),
    Cursor (..),
    CursorPosition (..),
    Colors (..),

    -- * Row data
    RowData (..),
    RowFlags (..),
    RowPrompt (..),
    RowIdentity (..),
  )
where

import Ghostty.Effectful.Color (Palette, Rgb)
import Ghostty.Effectful.Point (Coord (..), Point)
import Ghostty.Effectful.Terminal.Keys (CursorShape)

-- | A read of a frame (@ghostty_render_state_get@), by its header row. Each
-- describes the frame as it was captured, not the terminal now.
--
-- Rows with no constructor: @ROW_ITERATOR@, which is how rows are read
-- ('Ghostty.Effectful.Render.dirtyRows', 'Ghostty.Effectful.Render.allRows');
-- @OVERSCAN@ and @OVERSCAN_REQUEST@, since the binding requests no overscan
-- (DESIGN.md §4.3); and @IMAGE_ITERATOR@ and
-- @IMAGE_PLACEMENTS_GENERATION@, with image placements (PLAN.md M6.15).
data FrameData a where
  -- | The viewport's width in cells.
  RenderCols :: FrameData Word16
  -- | The viewport's height in cells.
  RenderRows :: FrameData Word16
  -- | What changed since the last frame that ran to its end.
  RenderDirty :: FrameData Dirty
  -- | The background colour: the program's, or the default.
  RenderColorBackground :: FrameData Rgb
  -- | The foreground colour: the program's, or the default.
  RenderColorForeground :: FrameData Rgb
  -- | The cursor colour, if one is set.
  RenderColorCursor :: FrameData (Maybe Rgb)
  -- | Whether a cursor colour is set.
  RenderColorCursorHasValue :: FrameData Bool
  -- | The active 256-colour palette.
  RenderColorPalette :: FrameData Palette
  -- | The cursor's shape (DECSCUSR).
  RenderCursorVisualStyle :: FrameData CursorShape
  -- | Whether the modes show the cursor (DECTCEM).
  RenderCursorVisible :: FrameData Bool
  -- | Whether the modes make the cursor blink.
  RenderCursorBlinking :: FrameData Bool
  -- | Whether the cursor is at a password prompt.
  RenderCursorPasswordInput :: FrameData Bool
  -- | Whether the cursor is in the viewport.
  RenderCursorViewportHasValue :: FrameData Bool
  -- | The cursor's column, if it is in the viewport.
  RenderCursorViewportX :: FrameData (Maybe Word16)
  -- | The cursor's row of the viewport, if it is in the viewport.
  RenderCursorViewportY :: FrameData (Maybe Word16)
  -- | Whether the cursor is on the second column of a wide character, if
  -- it is in the viewport.
  RenderCursorViewportWideTail :: FrameData (Maybe Bool)
  -- | All of the cursor, in one read.
  RenderCursor :: FrameData Cursor
  -- | All of the colours, in one read.
  RenderColors :: FrameData Colors

deriving stock instance Show (FrameData a)

-- | How much of a frame changed since the last frame that ran to its end.
data Dirty
  = -- | nothing: drawing can be skipped
    NotDirty
  | -- | some rows: 'Ghostty.Effectful.Render.dirtyRows' names them
    PartlyDirty
  | -- | the whole frame, every row
    FullyDirty
  deriving stock (Eq, Show, Enum, Bounded)

-- | The cursor.
data Cursor = Cursor
  { -- | where it is, if in the viewport
    position :: Maybe CursorPosition,
    -- | whether the modes show it
    visible :: Bool,
    -- | whether it blinks
    blinking :: Bool,
    -- | whether it is at a password prompt
    passwordInput :: Bool,
    -- | its shape
    shape :: CursorShape
  }
  deriving stock (Eq, Show)

-- | Where the cursor is in the viewport.
data CursorPosition = CursorPosition
  { -- | the cell
    point :: Point 'Viewport,
    -- | whether that cell is the second column of a wide character
    wideTail :: Bool
  }
  deriving stock (Eq, Show)

-- | A frame's colours.
data Colors = Colors
  { -- | the background: the program's, or the default
    background :: Rgb,
    -- | the foreground: the program's, or the default
    foreground :: Rgb,
    -- | the cursor's, if one is set
    cursor :: Maybe Rgb,
    -- | the active palette
    palette :: Palette
  }
  deriving stock (Eq, Show)

-- | A read of a row (@ghostty_render_state_row_get@), by its header row.
--
-- Rows with no constructor: @CELLS@, which is how cells are read
-- ('Ghostty.Effectful.Render.cells'); @CELLS_RAW@, the same cells in one
-- packed view for embedders whose calls are costly; and @SELECTION@, with
-- the selection API (PLAN.md M6.5).
data RowData a where
  -- | Whether the row changed since the last frame that ran to its end.
  RowDirty :: RowData Bool
  -- | The row's flags.
  RowRaw :: RowData RowFlags
  -- | The row's position from the top of the viewport.
  RowViewportY :: RowData Int32
  -- | The row's identity across frames.
  RowId :: RowData RowIdentity

deriving stock instance Show (RowData a)

-- | What the terminal records about a row as a whole.
data RowFlags = RowFlags
  { -- | the row is soft-wrapped: its text continues on the next row
    wrap :: Bool,
    -- | the row continues the soft-wrapped row above it
    wrapContinuation :: Bool,
    -- | some cell has a grapheme cluster of more than one codepoint
    grapheme :: Bool,
    -- | some cell may be styled (false positives possible)
    styled :: Bool,
    -- | some cell may have a hyperlink (false positives possible)
    hasHyperlink :: Bool,
    -- | whether the row is part of a prompt
    prompt :: RowPrompt,
    -- | some cell is a kitty graphics virtual placeholder
    kittyVirtualPlaceholder :: Bool,
    -- | the terminal's own dirty flag for the row
    dirty :: Bool
  }
  deriving stock (Eq, Show)

-- | A row's part in a prompt, by OSC 133.
data RowPrompt
  = -- | no prompt cells
    NoPrompt
  | -- | the first line of a prompt
    PromptLine
  | -- | a continuation line of a prompt
    PromptContinuation
  deriving stock (Eq, Show, Enum, Bounded)

-- | A row's identity: the same row, moved, has the same one in a later
-- frame; one changed in place, pruned or scrolled out of the frame gets a
-- new one, and none is reused. Only equality means anything, so a cache of
-- per-row work can be keyed by it (and 'Ord' is for such a map).
data RowIdentity = RowIdentity Word64 Word64
  deriving stock (Eq, Ord, Show)
