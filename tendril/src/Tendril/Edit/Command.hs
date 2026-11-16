{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StrictData #-}

-- | Edit commands and the state they edit (DESIGN.md §5.4).
--
-- Every edit is a 'Command' value, indexed by the kind of mode it may be
-- bound in, so that a keymap of one mode cannot hold another's commands; and
-- by the application's own commands, which tendril carries but does not
-- interpret. 'apply' runs the commands that edit the text, the cursor, the
-- selection, the kill ring and the registers; the rest are the editor's
-- (undo, history, accepting the input, the application's), and it leaves
-- them to the editor (@Tendril.Editor@, §5.8).
--
-- __Positions.__ An t'EditState' keeps its cursor and anchor as code point
-- offsets, always on cluster boundaries: a motion moves between boundaries,
-- and after an edit each is moved to one, forward after an insertion (the
-- inserted text may have joined the cluster after it, and typing goes on
-- after what was typed) and back after a deletion (the text on each side may
-- have joined into one cluster). Code point offsets, not grapheme ones,
-- because the rope splits at a code point in time logarithmic in its
-- length, and finding the boundaries near one reads only the text near it,
-- where a grapheme offset is counted from the start of the buffer (D34).
module Tendril.Edit.Command
  ( -- * Commands
    ModeKind (..),
    Command (..),

    -- * Editing
    EditState,
    editState,
    withBuffer,
    apply,
  )
where

import Data.Text qualified as T
import GHC.Records (HasField (..))
import Tendril.Edit.KillRing (KillRing, appendKill, current, emptyKillRing, kill, prependKill, rotate)
import Tendril.Edit.Motion (Motion, Target (..), target)
import Tendril.Edit.Register (Register, Registers, emptyRegisters, recall, store)
import Tendril.Edit.Selection (Selection (..), collapsed, region)
import Tendril.Text.Buffer (Buffer, ceilingBoundary, extent, floorBoundary, replace, slice)
import Tendril.Text.Units (Offset, Range, Unit (..), fromOffset, offset, range)

-- $setup
-- >>> import Tendril.Edit.Motion (Motion (..))

-- | The kinds of mode a keymap is for. A command that only makes sense in
-- one kind (a vi operator in normal mode) is indexed by it, and one that
-- makes sense in every kind is polymorphic in it. Vi's commands arrive with
-- its keymap (M6.1), and Helix's kinds with its (M6.2).
data ModeKind = Emacs | ViInsert | ViNormal
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | An edit, or a request of the editor, that a key can be bound to, in
-- modes of kind @m@, with the application's commands of type @app@.
type role Command representational nominal

-- The mode is the guarantee, so a coercion may not change it (D30): while
-- every command is polymorphic in it, GHC would infer a phantom role.
data Command app (m :: ModeKind) where
  -- | Insert text at the cursor.
  Insert :: Text -> Command app m
  -- | Insert a line feed, whether or not the input is complete.
  ForceNewline :: Command app m
  -- | Move the cursor; an active selection keeps its anchor.
  Move :: Motion -> Command app m
  -- | Move the cursor, starting a selection at it if none is active.
  Select :: Motion -> Command app m
  -- | Start a selection at the cursor (Emacs's @set-mark-command@).
  SetMark :: Command app m
  -- | End the selection, keeping the cursor.
  Deselect :: Command app m
  -- | Delete the text between the cursor and where a motion would take it.
  Delete :: Motion -> Command app m
  -- | Delete it, onto the kill ring. A kill straight after a kill joins its
  -- entry, at the end going forward and at the start going back; a kill of
  -- nothing adds no entry and does not break the run.
  Kill :: Motion -> Command app m
  -- | Kill the selected text. Joining a kill, it goes at the start of the
  -- entry if the cursor is before the anchor.
  KillSelection :: Command app m
  -- | Copy the selected text onto the kill ring, and end the selection.
  CopySelection :: Command app m
  -- | Insert the kill ring's current entry.
  Yank :: Command app m
  -- | Straight after a yank, replace what it inserted with the kill ring's
  -- next older entry, moving its yank pointer (Emacs's @yank-pop@).
  YankPop :: Command app m
  -- | Store the selected text in a register, and end the selection.
  CopyToRegister :: Register -> Command app m
  -- | Insert what a register holds.
  InsertRegister :: Register -> Command app m
  -- | Undo the last edit (the editor's: M2.5).
  Undo :: Command app m
  -- | Redo an undone edit (the editor's: M2.5).
  Redo :: Command app m
  -- | Submit the input if the validator finds it complete, or insert a line
  -- feed (the editor's, §5.5).
  AcceptOrNewline :: Command app m
  -- | Recall the previous history entry (the editor's).
  HistoryPrev :: Command app m
  -- | Recall the next history entry (the editor's).
  HistoryNext :: Command app m
  -- | Select the syntax node around the selection (the editor's, through
  -- its @enclosing@ plug-in, §5.5).
  ExpandSelection :: Command app m
  -- | Undo an 'ExpandSelection' (the editor's).
  ShrinkSelection :: Command app m
  -- | Edit the input in @$EDITOR@ (the editor's).
  EditExternal :: Command app m
  -- | One of the application's own commands, which tendril passes on.
  App :: app -> Command app m

deriving stock instance (Eq app) => Eq (Command app m)

deriving stock instance (Show app) => Show (Command app m)

-- | The text being edited, the cursor and selection in it, and what the
-- editing commands keep between them. Read its parts as @s.buffer@,
-- @s.cursor@, @s.selection@, @s.killRing@ and @s.registers@; only 'apply'
-- changes them, which keeps the cursor and the anchor on cluster
-- boundaries.
data EditState = EditState
  { stateBuffer :: Buffer,
    stateSelection :: Selection,
    -- | The goal column a vertical motion left ("Tendril.Edit.Motion").
    stateGoal :: Maybe Word,
    stateKillRing :: KillRing,
    stateRegisters :: Registers,
    -- | What the last command was, for the commands that act differently
    -- straight after another.
    stateChain :: Chain
  }
  deriving stock (Eq, Show)

-- | What the last command left for the next one.
data Chain
  = -- | Nothing.
    Unchained
  | -- | It killed, so a kill now joins its entry.
    Killed
  | -- | It yanked this many code points from here, which a yank-pop replaces.
    Yanked (Offset 'CodePoint) Word
  deriving stock (Eq, Show)

instance HasField "buffer" EditState Buffer where
  getField = (.stateBuffer)

instance HasField "cursor" EditState (Offset 'CodePoint) where
  getField s = s.stateSelection.cursor

instance HasField "selection" EditState Selection where
  getField = (.stateSelection)

instance HasField "killRing" EditState KillRing where
  getField = (.stateKillRing)

instance HasField "registers" EditState Registers where
  getField = (.stateRegisters)

-- | A buffer to edit, with the cursor at its end, nothing selected, nothing
-- killed and no register holding anything.
editState :: Buffer -> EditState
editState b = EditState b (collapsed (extent b)) Nothing emptyKillRing emptyRegisters Unchained

-- | The state with its text replaced (a history entry recalled), the cursor
-- at the end and nothing selected; the kill ring and registers stay.
withBuffer :: Buffer -> EditState -> EditState
withBuffer b s = (editState b) {stateKillRing = s.stateKillRing, stateRegisters = s.stateRegisters}

-- | The state after an editing command; or 'Nothing' for a command that is
-- the editor's, not an edit (undo, history, accepting the input, structural
-- selection, the external editor, the application's).
--
-- >>> s = editState "ab cd"
-- >>> fmap (\s' -> (s'.buffer, s'.cursor)) (apply (Kill PrevWord) s)
-- Just ("ab ",Offset 3)
-- >>> apply (HistoryPrev :: Command () 'Emacs) s
-- Nothing
apply :: Command app m -> EditState -> Maybe EditState
apply cmd s0 = case cmd of
  Insert t -> Just (insert t s)
  ForceNewline -> Just (insert "\n" s)
  Move m -> Just (moveBy m s0.stateSelection.anchor)
  Select m -> Just (moveBy m (s0.stateSelection.anchor <|> Just x))
  SetMark -> Just s {stateSelection = Selection x (Just x)}
  Deselect -> Just s {stateSelection = collapsed x}
  Delete m -> Just (cut (motionRange m) s)
  Kill m ->
    let p = (moveTo m).position
     in Just (killRange (if p < x then Backward else Forward) (range x p))
  KillSelection ->
    -- Emacs's kill-region: joining a kill, it goes at the start of the
    -- entry when the cursor is before the anchor.
    let dir = if any (x <) s0.stateSelection.anchor then Backward else Forward
     in Just (maybe deselected (killRange dir) selected)
  CopySelection -> Just (maybe s (\r -> deselected {stateKillRing = kill (slice r b) s.stateKillRing}) selected)
  Yank -> Just (maybe s yank (current s.stateKillRing))
  YankPop -> Just $ case s0.stateChain of
    Yanked at n ->
      let ring = rotate s.stateKillRing
          t = fromMaybe "" (current ring)
       in (insertOver (range at (plus at n)) t s) {stateKillRing = ring, stateChain = Yanked at (len t)}
    _ -> s
  CopyToRegister r -> Just (maybe s (\rg -> deselected {stateRegisters = store r (slice rg b) s.stateRegisters}) selected)
  InsertRegister r -> Just (maybe s (`insert` s) (recall r s.stateRegisters))
  Undo -> Nothing
  Redo -> Nothing
  AcceptOrNewline -> Nothing
  HistoryPrev -> Nothing
  HistoryNext -> Nothing
  ExpandSelection -> Nothing
  ShrinkSelection -> Nothing
  EditExternal -> Nothing
  App _ -> Nothing
  where
    b = s0.stateBuffer
    x = s0.stateSelection.cursor
    -- Every command but a vertical motion forgets the goal column, and
    -- every one but a kill or a yank ends a chain.
    s = s0 {stateGoal = Nothing, stateChain = Unchained}
    deselected = s {stateSelection = collapsed x}
    selected = region s0.stateSelection
    moveTo = target b s0.stateGoal x
    moveBy m anchor =
      let t = moveTo m
       in s {stateSelection = Selection t.position anchor, stateGoal = t.goal}
    motionRange m = range x (moveTo m).position
    -- A kill of nothing kills nothing, but as in Emacs it is still a kill:
    -- it ends the selection, and a chain of kills goes on through it.
    killRange dir r
      | r.start == r.end = deselected {stateChain = if s0.stateChain == Killed then Killed else Unchained}
      | otherwise =
          let t = slice r b
              ring = case (s0.stateChain, dir) of
                (Killed, Forward) -> appendKill t s.stateKillRing
                (Killed, Backward) -> prependKill t s.stateKillRing
                _ -> kill t s.stateKillRing
           in (cut r s) {stateKillRing = ring, stateChain = Killed}
    yank t = (insert t s) {stateChain = Yanked x (len t)}

-- | Which way a kill went from the cursor.
data Direction = Backward | Forward

-- | Insert text at the cursor, ending the selection.
insert :: Text -> EditState -> EditState
insert t s = insertOver (range x x) t s
  where
    x = s.stateSelection.cursor

-- | Replace a range with text, and put the cursor after it, ending the
-- selection: on the boundary at or after the text's end, since the text may
-- have joined the cluster after it.
insertOver :: Range 'CodePoint -> Text -> EditState -> EditState
insertOver r t s =
  s {stateBuffer = b', stateSelection = collapsed (ceilingBoundary b' (plus r.start (len t)))}
  where
    b' = replace r t s.stateBuffer

-- | Delete a range, and put the cursor where it was, ending the selection:
-- on the boundary at or before it, since the text on each side may have
-- joined into one cluster.
cut :: Range 'CodePoint -> EditState -> EditState
cut r s = s {stateBuffer = b', stateSelection = collapsed (floorBoundary b' r.start)}
  where
    b' = replace r "" s.stateBuffer

len :: Text -> Word
len = fromIntegral . T.length

plus :: Offset 'CodePoint -> Word -> Offset 'CodePoint
plus o n = offset (fromOffset o + n)
