{-# LANGUAGE DuplicateRecordFields #-}

-- | What a terminal reports while it processes VT (DESIGN.md §4.5): the
-- library's callbacks, as values. 'Ghostty.Effectful.Terminal.feed' returns
-- them in the order the bytes produced them.
--
-- The binding installs every callback itself. Each only appends to the
-- terminal's buffer, and anything that needs the terminal (the new title,
-- the cursor at a prompt mark) is read between writes, never inside one.
module Ghostty.Effectful.Event
  ( -- * Events
    TerminalEvent (..),

    -- * Shell integration (OSC 133)
    PromptMark (..),
    PromptStep (..),
    PromptKind (..),

    -- * Clipboard
    ClipboardLocation (..),
    ClipboardContent (..),

    -- * Progress and status
    ProgressReport (..),
    ProgressState (..),
    ProgramReport (..),
    ProgramState (..),
    ProgramNeed (..),

    -- * Unsupported sequences
    UnsupportedSequence (..),
    SequenceKind (..),
    OscTerminator (..),

    -- * Window size
    WindowSize (..),
  )
where

import Ghostty.Effectful.Internal.Size (Extent)
import Ghostty.Effectful.Point (Coord (..), Point)

-- | One thing a terminal reported.
data TerminalEvent
  = -- | Bytes for the program: a reply to one of its queries, or a report
    -- it asked for (DECRQM, DSR, mode 2048). The caller writes them to the
    -- PTY.
    WritePty ByteString
  | -- | BEL.
    Bell
  | -- | The title changed (OSC 0, OSC 2); the title after the write that
    -- changed it.
    TitleChanged Text
  | -- | The working directory changed (OSC 7, OSC 9, OSC 1337), as the
    -- program sent it: for OSC 7 a @file://@ URI, undecoded. Empty when it
    -- was cleared.
    PwdChanged Text
  | -- | A shell integration mark (OSC 133), with where the cursor was just
    -- after it.
    SemanticPrompt PromptMark
  | -- | A clipboard write (OSC 52, OSC 1337, OSC 5522) the terminal's
    -- configuration accepted. An empty list clears the clipboard.
    ClipboardWrite ClipboardLocation [ClipboardContent]
  | -- | A desktop notification (OSC 9, OSC 777): title (empty if the
    -- protocol has none) and body.
    Notification Text Text
  | -- | A progress report (OSC 9;4).
    Progress ProgressReport
  | -- | A program status report (OSC 7501).
    ProgramStatus ProgramReport
  | -- | Synchronized output (mode 2026) began ('True') or ended. The ends
    -- come in pairs with the beginnings: by the program, by a full reset,
    -- or by a resize.
    RenderHold Bool
  | -- | An APC or OSC sequence the library does not implement. Reported only
    -- once 'Ghostty.Effectful.Terminal.SetUnknownMaxBytes' is not zero.
    UnknownSequence UnsupportedSequence
  | -- | The program did a full reset (RIS). The title and working directory
    -- are cleared without a 'TitleChanged' or 'PwdChanged'.
    Reset
  | -- | The program asked for a window size (CSI 8 t). The terminal is not
    -- resized: that is the caller's decision.
    ResizeRequest WindowSize
  deriving stock (Eq, Show)

-- | An OSC 133 mark and the cursor just after it.
data PromptMark = PromptMark
  { -- | which mark
    step :: PromptStep,
    -- | the cursor, in the active area
    cursor :: Point 'Active
  }
  deriving stock (Eq, Show)

-- | OSC 133's marks: @A@, @B@, @C@ and @D@.
data PromptStep
  = -- | @A@: a prompt starts. Shells may send it again for a redrawn prompt.
    PromptStart PromptKind
  | -- | @B@: the prompt is drawn, and input starts.
    InputStart
  | -- | @C@: the input was submitted, and its output starts. The command
    -- line, if the shell sent one; empty otherwise.
    OutputStart Text
  | -- | @D@: the command finished. Its exit code, if the shell reported one,
    -- and a description of what went wrong, empty if none.
    CommandEnd (Maybe Int32) Text
  deriving stock (Eq, Show)

-- | Which prompt an @A@ starts.
data PromptKind
  = -- | the main prompt; also when the shell does not say
    PrimaryPrompt
  | -- | a prompt at the right edge of the line
    RightPrompt
  | -- | the start of an extra line of a command spanning several
    ContinuationPrompt
  | -- | another prompt for an extra line of input (bash's PS2); shells
    -- differ between this and 'ContinuationPrompt'
    SecondaryPrompt
  deriving stock (Eq, Show, Enum, Bounded)

-- | A clipboard.
data ClipboardLocation
  = -- | the system clipboard
    StandardClipboard
  | -- | the selection clipboard
    SelectionClipboard
  | -- | the primary selection
    PrimaryClipboard
  deriving stock (Eq, Show, Enum, Bounded)

-- | One representation of a clipboard value.
data ClipboardContent = ClipboardContent
  { -- | its MIME type
    mime :: Text,
    -- | its bytes, decoded from the protocol's encoding
    bytes :: ByteString
  }
  deriving stock (Eq, Show)

-- | A progress report (OSC 9;4).
data ProgressReport = ProgressReport
  { -- | what to show
    state :: ProgressState,
    -- | the percentage, 0 to 100, if the program sent one
    percent :: Maybe Word8
  }
  deriving stock (Eq, Show)

-- | What a progress report asks to show.
data ProgressState
  = -- | no progress
    ProgressRemove
  | -- | determinate progress
    ProgressSet
  | -- | a failure
    ProgressError
  | -- | indeterminate progress
    ProgressIndeterminate
  | -- | paused progress
    ProgressPause
  deriving stock (Eq, Show, Enum, Bounded)

-- | A program status report (OSC 7501). The terminal keeps no record of
-- these; a report replaces its record whole.
data ProgramReport = ProgramReport
  { -- | what the program is doing
    state :: ProgramState,
    -- | what a blocked program needs, if it said
    need :: Maybe ProgramNeed,
    -- | the percentage, 0 to 100, for working and blocked states that
    -- sent one
    percent :: Maybe Word8,
    -- | which record: empty for the program itself, @a/b@ a child of @a@
    record :: Text,
    -- | a stable name for the program, for machines; empty if not sent
    app :: Text,
    -- | a short label, for people
    title :: Text,
    -- | one line, for people
    message :: Text
  }
  deriving stock (Eq, Show)

-- | What a program says it is doing.
data ProgramState
  = -- | waiting for the user's next instruction
    StatusIdle
  | -- | running on its own
    StatusWorking
  | -- | finished, with a result to look at
    StatusDone
  | -- | waiting for the user
    StatusBlocked
  | -- | failed and stopped
    StatusError
  | -- | remove this record and those beneath it (all, for the root)
    StatusClear
  deriving stock (Eq, Show, Enum, Bounded)

-- | What a blocked program needs.
data ProgramNeed
  = -- | approval
    NeedPermission
  | -- | a typed answer
    NeedQuestion
  | -- | a credential
    NeedAuth
  deriving stock (Eq, Show, Enum, Bounded)

-- | A sequence the library does not implement.
data UnsupportedSequence = UnsupportedSequence
  { -- | APC or OSC
    kind :: SequenceKind,
    -- | whether 'content' was cut short by the byte limit
    truncated :: Bool,
    -- | the bytes between introducer and terminator (for an OSC, its number
    -- first)
    content :: ByteString
  }
  deriving stock (Eq, Show)

-- | The kind of an unsupported sequence.
data SequenceKind
  = -- | an Application Program Command
    Apc
  | -- | an Operating System Command, and how it ended: a reply should end
    -- the same way
    Osc OscTerminator
  deriving stock (Eq, Show)

-- | How an OSC ended.
data OscTerminator
  = -- | @ESC \\@
    TerminatedBySt
  | -- | BEL
    TerminatedByBel
  deriving stock (Eq, Show, Enum, Bounded)

-- | A requested window size, in cells; 'Nothing' keeps that axis.
data WindowSize = WindowSize
  { -- | columns
    cols :: Maybe Extent,
    -- | rows
    rows :: Maybe Extent
  }
  deriving stock (Eq, Show)
