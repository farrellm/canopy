{-# LANGUAGE StrictData #-}

-- | Undo as a tree, not a stack (DESIGN.md §5.4), as in vim and Helix: an
-- edit made after an undo starts a branch, and the steps it leaves behind
-- stay reachable by 'earlier' and 'later', which walk every step in the
-- order it was made.
--
-- An t'Undoable' is the state being edited with its tree, and 'edit' runs a
-- command on both: it applies an editing command ("Tendril.Edit.Command"),
-- records the step if the command changed the text, and runs
-- 'Tendril.Edit.Command.Undo' and 'Tendril.Edit.Command.Redo' itself.
--
-- __Coalescing.__ A run of typing is one step, as is a run of deletions
-- one cluster back and one of deletions one cluster on: an edit joins the
-- step before it if both are of one of those kinds, nothing came between
-- them (a motion, an undo, any command that did not change the text, or
-- one of the editor's), and it came within 'coalesceWindow' of the last
-- edit of the step. Typing is the insertion of one cluster that is not a
-- line break, so a paste or a newline is a step of its own. Every other
-- edit is a step of its own.
--
-- __Where the cursor goes.__ An undo puts back the text and the cursor as
-- they were before the step's first edit; a redo, and a move by 'earlier'
-- or 'later', the text and the cursor as they were after the step's last
-- edit. Neither keeps a selection, and neither changes the kill ring or the
-- registers.
--
-- The tree keeps every step until the state is replaced: the editor starts
-- a new one for each input it reads (and for a history entry recalled into
-- the buffer). Each step holds two snapshots of the text, which share all
-- but the edited part of the rope.
module Tendril.Edit.Undo
  ( -- * Time
    Time,
    millis,
    coalesceWindow,

    -- * Undoable state
    Undoable,
    undoable,
    edit,
    interrupted,

    -- * Moving in the tree
    undo,
    redo,
    earlier,
    later,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import GHC.Records (HasField (..))
import Tendril.Edit.Command (Command (..), EditState, Snapshot, apply, interrupt, restore, snapshot)
import Tendril.Edit.Motion (Motion (..))
import Tendril.Text.Grapheme (clusters)

-- $setup
-- >>> import Tendril.Edit.Command (ModeKind (..), editState)
-- >>> import Tendril.Edit.Motion (Motion (..))
-- >>> run t cs u = foldl' (\u' c -> fromMaybe u' (edit (millis t) c u')) u (cs :: [Command () 'Emacs])
-- >>> text u = toText u.state.buffer

-- | When a command ran, in milliseconds from any origin, which must not move
-- while the editor runs: tendril has no clock, and the driver that reads
-- the host's input reads one (§5.8).
newtype Time = Time Word64
  deriving stock (Eq, Ord, Show)

-- | A time, @n@ milliseconds from the clock's origin.
millis :: Word64 -> Time
millis = Time

-- | How long after a step's last edit, in milliseconds, another may join
-- it: one second.
coalesceWindow :: Word64
coalesceWindow = 1000

-- | The state being edited, and the steps that led to it. Read the state as
-- @u.state@, how many steps have been recorded as @u.steps@, and which one
-- the state is at as @u.step@: steps are numbered from 1 in the order they
-- were made, and 0 is the state the tree started from.
data Undoable = Undoable
  { undoState :: EditState,
    -- | The steps, by number. Step 0 is the start, and is its own parent.
    undoSteps :: IntMap.IntMap Step,
    -- | The step the text is at.
    undoAt :: Int,
    -- | The kind and time of the current step's last edit, while the next
    -- edit may still join it.
    undoOpen :: Maybe (Kind, Time)
  }
  deriving stock (Eq, Show)

-- | One step: the text and the cursor before its first edit and after its
-- last, the step it was made from, and the one a redo from it goes to.
data Step = Step
  { parent :: Int,
    before :: Snapshot,
    after :: Snapshot,
    -- | The step most recently made from this one or undone back to it.
    next :: Maybe Int
  }
  deriving stock (Eq, Show)

-- | Which edits join into one step.
data Kind = Typing | DeletingBack | DeletingOn | Single
  deriving stock (Eq, Show)

instance HasField "state" Undoable EditState where
  getField = (.undoState)

instance HasField "step" Undoable Int where
  getField = (.undoAt)

instance HasField "steps" Undoable Int where
  getField u = IntMap.size u.undoSteps - 1

-- | A state with nothing to undo.
undoable :: EditState -> Undoable
undoable s = Undoable s (IntMap.singleton 0 (Step 0 p p Nothing)) 0 Nothing
  where
    p = snapshot s

-- | The state and tree after a command at a time; or 'Nothing' for a
-- command that is the editor's ('Tendril.Edit.Command.apply' returns
-- 'Nothing' for it) other than 'Undo' and 'Redo'. After running one of
-- those, the editor passes the result through 'interrupted'.
--
-- >>> u = run 0 [Insert "a", Insert "b"] (undoable (editState ""))
-- >>> (text u, u.steps)
-- ("ab",1)
-- >>> text (undo u)
-- ""
-- >>> u' = run 5000 [Insert "c"] u
-- >>> (text u', u'.steps, text (undo u'))
-- ("abc",2,"ab")
edit :: Time -> Command app m -> Undoable -> Maybe Undoable
edit t cmd u = case cmd of
  Undo -> Just (undo u)
  Redo -> Just (redo u)
  _ -> do
    s' <- apply cmd u.undoState
    pure $
      if s'.version == u.undoState.version
        then u {undoState = s', undoOpen = Nothing}
        else record t (kindOf cmd) u.undoState s' u

-- | What a command the editor ran leaves: as 'Tendril.Edit.Command.interrupt'
-- does to the state, and the next edit will not join the last step.
interrupted :: Undoable -> Undoable
interrupted u = u {undoState = interrupt u.undoState, undoOpen = Nothing}

-- | Back to before the current step, and the cursor to where it was then.
-- At the start, the state is only 'interrupted'.
undo :: Undoable -> Undoable
undo u = fromMaybe (interrupted u) $ do
  st <- IntMap.lookup u.undoAt u.undoSteps
  guard (u.undoAt /= 0)
  pure
    u
      { undoState = restore st.before u.undoState,
        undoSteps = IntMap.adjust (\p -> p {next = Just u.undoAt}) st.parent u.undoSteps,
        undoAt = st.parent,
        undoOpen = Nothing
      }

-- | On to the step most recently made from the current one or undone back to
-- it, and the cursor to where its last edit left it. With none, the state
-- is only 'interrupted'.
--
-- >>> u = run 0 [Insert "a", Move PrevCluster] (undoable (editState ""))
-- >>> (text (redo (undo u)), (redo (undo u)).state.cursor)
-- ("a",Offset 1)
redo :: Undoable -> Undoable
redo u = fromMaybe (interrupted u) $ do
  n <- (.next) =<< IntMap.lookup u.undoAt u.undoSteps
  goTo n u

-- | To the step made just before the current one, whichever branch it is
-- on (vim's @g-@): repeated, it visits every step back to the start. At the
-- start, the state is only 'interrupted'.
--
-- >>> u = run 0 [Insert "a", Undo, Insert "b"] (undoable (editState ""))
-- >>> map text (take 3 (iterate earlier u))
-- ["b","a",""]
earlier :: Undoable -> Undoable
earlier u = fromMaybe (interrupted u) (guard (u.undoAt > 0) *> goTo (u.undoAt - 1) u)

-- | To the step made just after the current one (vim's @g+@), as 'earlier'
-- goes back. At the newest step, the state is only 'interrupted'.
later :: Undoable -> Undoable
later u = fromMaybe (interrupted u) (goTo (u.undoAt + 1) u)

-- | To a step, with the text and the cursor as its last edit left them.
-- The steps on the way from it to the start each redo towards it after.
goTo :: Int -> Undoable -> Maybe Undoable
goTo n u = do
  st <- IntMap.lookup n u.undoSteps
  pure
    u
      { undoState = restore st.after u.undoState,
        undoSteps = towards n u.undoSteps,
        undoAt = n,
        undoOpen = Nothing
      }

-- | The steps with each one on the way from a step to the start redoing
-- towards it.
towards :: Int -> IntMap.IntMap Step -> IntMap.IntMap Step
towards n steps = case IntMap.lookup n steps of
  Just st | n /= 0 -> towards st.parent (IntMap.adjust (\p -> p {next = Just n}) st.parent steps)
  _ -> steps

-- | The tree after an edit from one state to another: the edit joins the
-- current step if it may, or is a new step made from it.
record :: Time -> Kind -> EditState -> EditState -> Undoable -> Undoable
record t k s s' u = case u.undoOpen of
  Just (k0, t0)
    | k /= Single && k == k0 && within t0 ->
        u
          { undoState = s',
            undoSteps = IntMap.adjust (\st -> st {after = snapshot s'}) u.undoAt u.undoSteps,
            undoOpen = Just (k, t)
          }
  _ ->
    u
      { undoState = s',
        undoSteps =
          IntMap.insert n (Step u.undoAt (snapshot s) (snapshot s') Nothing) $
            IntMap.adjust (\p -> p {next = Just n}) u.undoAt u.undoSteps,
        undoAt = n,
        undoOpen = Just (k, t)
      }
  where
    n = IntMap.size u.undoSteps
    -- A time before the last edit's (a clock that must not, did) is no gap.
    within (Time t0) = let Time t1 = t in t1 < t0 || t1 - t0 <= coalesceWindow

-- | Which edits a command's joins.
kindOf :: Command app m -> Kind
kindOf = \case
  Insert t | typed t -> Typing
  Delete PrevCluster -> DeletingBack
  Delete NextCluster -> DeletingOn
  _ -> Single
  where
    -- One cluster, and not a line break: a key typed, not a paste.
    typed t = case clusters t of
      [c] -> T.all (\ch -> ch /= '\n' && ch /= '\r') c
      _ -> False
