-- | The undo tree (DESIGN.md §5.4, §11.3): undo and redo are inverses, an
-- undo puts back the text and the cursor from before the step, every step
-- stays reachable after a branch, and runs of typing and deleting join.
module Test.Tendril.Edit.Undo (tests) where

import Tendril.Edit.Command (Command (..), ModeKind (..), editState)
import Tendril.Edit.KillRing (entries)
import Tendril.Edit.Motion (Motion (..))
import Tendril.Edit.Register (Register (..))
import Tendril.Edit.Selection (Selection (anchor))
import Tendril.Edit.Undo (Undoable, coalesceWindow, earlier, edit, interrupted, later, millis, redo, undo, undoable)
import Tendril.Text.Buffer (fromText)
import Tendril.Text.Grapheme (boundaries)
import Tendril.Text.Units (fromOffset, offset)
import Test.QuickCheck (Gen, conjoin, counterexample, cover, elements, forAll, frequency, listOf, property, (===), (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Tendril.Text.Gen (genRunText)

tests :: TestTree
tests =
  testGroup
    "Tendril.Edit.Undo"
    [ testProperty "a redo after an undo, and an undo after a redo, come back" $
        forAll genSession $ \(t, es) ->
          let us = scanl (\v (dt, c) -> after dt c v) (start t) es
              u = fromMaybe (start t) (viaNonEmpty last us)
              changes = length [() | (v, v') <- zip us (drop 1 us), text v' /= text v, steps v' == steps v]
              back f g = let u' = via (g . f) u in (text u', step u') === (text u, step u)
              canRedo = step (via redo u) /= step u
              holds c p = if c then p else property True
           in cover 30 (step u /= 0) "something to undo" $
                cover 10 canRedo "something to redo" $
                  cover 10 (changes > 0) "an edit joined a step" $
                    conjoin
                      [ counterexample "redo . undo" (holds (step u /= 0) (back undo redo)),
                        counterexample "undo . redo" (holds canRedo (back redo undo))
                      ],
      testProperty "an undo puts back the text and the cursor from before the step" $
        forAll genSession $ \(t, es) ->
          forAll genEdit $ \c ->
            let u = runAll (start t) es
                edited = after (coalesceWindow + 1) c u
                v = via undo edited
             in text edited /= text u ==>
                  (text v, cursor v, step v) === (text u, cursor u, step u),
      testProperty "every step stays reachable, in the order it was made" $
        forAll genSession $ \(t, es) ->
          let apart = [(coalesceWindow + 1, c) | (_, c) <- es]
              us = scanl (\v (dt, c) -> after dt c v) (start t) apart
              made =
                text (start t)
                  : [text v' | ((_, c), v, v') <- zip3 apart us (drop 1 us), isEdit c, text v' /= text v]
              u = fromMaybe (start t) (viaNonEmpty last us)
              walked = map text (take (steps u + 1) (iterate (via later) (via (toStart earlier) u)))
           in cover 10 (any (isUndo . snd) es && length made > 2) "a branch" $
                (steps u, walked) === (length made - 1, made),
      testProperty "the cursor stays on a boundary through undo, redo, earlier and later" $
        forAll genSession $ \(t, es) ->
          let us = scanl (\u (dt, c) -> after dt c u) (start t) es
              moves = [undo, redo, earlier, later]
           in conjoin
                [ counterexample (show (text v, cursor v)) (offset (cursor v) `elem` boundaries (text v))
                | u <- us,
                  v <- u : map (`via` u) moves
                ],
      testCase "a run of typing is one step" $ do
        let u = typed 100 "abc" (start "")
        (text u, steps u) @?= ("abc", 1)
        text (via undo u) @?= "",
      testCase "a pause splits a run" $ do
        let u = after (coalesceWindow + 1) (Insert "c") (typed 100 "ab" (start ""))
        (steps u, text (via undo u)) @?= (2, "ab"),
      testCase "the gap is from the step's last edit, not its first" $
        steps (typed (coalesceWindow `div` 2) "abcd" (start "")) @?= 1,
      testCase "a change of kind, a motion, an undo or an editor's command splits a run" $ do
        let deleted = after 10 (Delete PrevCluster) (typed 10 "ab" (start ""))
        (steps deleted, text (via undo deleted)) @?= (2, "ab")
        let moved = typed 10 "c" (after 10 (Move PrevCluster) (typed 10 "ab" (start "")))
        (text moved, steps moved, text (via undo moved)) @?= ("acb", 2, "ab")
        let redone = typed 10 "c" (via (redo . undo) (typed 10 "ab" (start "")))
        (steps redone, text (via undo redone)) @?= (2, "ab")
        steps (typed 10 "c" (after 10 HistoryPrev (typed 10 "ab" (start "")))) @?= 2,
      testCase "runs of deletions back and on join, each its own way" $ do
        let back = edits 10 [Delete PrevCluster, Delete PrevCluster] (start "abcd")
        (text back, steps back, text (via undo back)) @?= ("ab", 1, "abcd")
        let forward = edits 10 [Move BufferStart, Delete NextCluster, Delete NextCluster, Delete PrevCluster] (start "abcd")
        (text forward, steps forward, text (via undo forward)) @?= ("cd", 1, "abcd"),
      testCase "a paste, a newline and a kill are each a step of their own" $ do
        steps (edits 10 [Insert "x", Insert "pasted", Insert "y"] (start "")) @?= 3
        steps (edits 10 [Insert "x", ForceNewline, Insert "\n", Insert "y"] (start "")) @?= 4
        steps (edits 10 [Kill PrevWord, Kill PrevWord] (start "one two")) @?= 2,
      testCase "an emoji with a skin tone is one key typed" $
        steps (edits 10 [Insert "a", Insert "\x1F44D\x1F3FD", Insert "b"] (start "")) @?= 1,
      testCase "a command that changes nothing is no step" $ do
        let u =
              edits
                10
                [ Move BufferStart,
                  Delete PrevCluster,
                  Kill PrevWord,
                  Yank,
                  YankPop,
                  InsertRegister (Register 'r'),
                  Insert "",
                  Select BufferEnd,
                  CopySelection
                ]
                (start "ab")
        (text u, steps u) @?= ("ab", 0)
        -- A yank-pop round a ring of one puts back the same text.
        let popped = edits 10 [Kill PrevWord, Yank, YankPop] (start "ab")
        (text popped, steps popped) @?= ("ab", 2),
      testCase "the cursor: an undo goes back to before the step, a redo to after it" $ do
        let u = edits 10 [Move BufferStart, Insert "x", Move BufferEnd] (start "ab")
        cursor (via undo u) @?= 0
        cursor (via (redo . undo) u) @?= 1
        (tree (via undo u)).state.selection.anchor @?= Nothing,
      testCase "redo follows the branch made or undone last" $ do
        let u = edits 10 [Insert "a", Undo, Insert "b", Undo] (start "")
        text (via redo u) @?= "b"
        let v = via (later . earlier . earlier . redo) u
        (text v, text (via (redo . undo) v)) @?= ("a", "a")
        text (via (redo . toStart undo) v) @?= "a",
      testCase "nothing to undo, redo, or go to leaves the text" $ do
        text (via undo (start "ab")) @?= "ab"
        text (via redo (start "ab")) @?= "ab"
        text (via earlier (start "ab")) @?= "ab"
        text (via later (typed 10 "c" (start "ab"))) @?= "abc",
      testCase "the kill ring survives an undo" $ do
        let u = edits 10 [Kill PrevWord, Undo] (start "one two")
        (text u, entries (tree u).state.killRing) @?= ("one two", ["two"]),
      testCase "an undo ends a yank's claim on a yank-pop" $
        -- Without it, the yank-pop replaced the yank's range in the text
        -- from before the yank.
        text (edits 10 [Kill PrevWord, Insert "x", Yank, Undo, YankPop] (start "ab cd")) @?= "ab x",
      testCase "an undo ends a run of kills" $
        entries (tree (edits 10 [Kill PrevWord, Kill PrevWord, Undo, Kill PrevWord] (start "a b c"))).state.killRing
          @?= ["b ", "b c"],
      testCase "the editor's commands other than undo and redo are not edits" $
        forM_ [AcceptOrNewline, HistoryPrev, HistoryNext, ExpandSelection, ShrinkSelection, EditExternal, App ()] $ \c ->
          isJust (edit (millis 0) c (tree (start "x"))) @?= False,
      testCase "a clock that went back is no gap" $ do
        let u = tree (typed 10 "ab" (start ""))
        ((.steps) <$> edit (millis 0) (Insert "c") u) @?= Just 1,
      testCase "an undo of a deletion that joined two clusters puts the cursor back between them" $ do
        -- Hangul L and V join once the letter between them goes.
        let u = edits 10 [Move BufferStart, Move NextCluster, Delete NextCluster] (start "\x1100\&a\x1161")
        (text u, cursor u) @?= ("\x1100\x1161", 0)
        (text (via undo u), cursor (via undo u)) @?= ("\x1100\&a\x1161", 1)
    ]

-- | A state with its tree, and the time of its last command.
data Clocked = Clocked Word64 Undoable

-- | A state with its tree, at time 0, the cursor at the end.
start :: Text -> Clocked
start = Clocked 0 . undoable . editState . fromText

-- | The tree's state.
tree :: Clocked -> Undoable
tree (Clocked _ u) = u

-- | A move in the tree, at the same time.
via :: (Undoable -> Undoable) -> Clocked -> Clocked
via f (Clocked t u) = Clocked t (f u)

text :: Clocked -> Text
text c = toText (tree c).state.buffer

-- | A command, some milliseconds after the last one. The editor's commands
-- other than undo and redo leave the state 'interrupted', as the editor will.
after :: Word64 -> Command () 'Emacs -> Clocked -> Clocked
after dt c (Clocked t u) = Clocked t' (fromMaybe (interrupted u) (edit (millis t') c u))
  where
    t' = t + dt

edits :: Word64 -> [Command () 'Emacs] -> Clocked -> Clocked
edits dt cs u = foldl' (flip (after dt)) u cs

-- | Typing, a key at a time, some milliseconds apart.
typed :: Word64 -> Text -> Clocked -> Clocked
typed dt t = edits dt (map (Insert . one) (toString t))

runAll :: Clocked -> [(Word64, Command () 'Emacs)] -> Clocked
runAll = foldl' (\u (dt, c) -> after dt c u)

-- | A move repeated until the tree is at its start.
toStart :: (Undoable -> Undoable) -> Undoable -> Undoable
toStart f u
  | u.step == 0 = u
  | otherwise = toStart f (f u)

steps :: Clocked -> Int
steps c = (tree c).steps

step :: Clocked -> Int
step c = (tree c).step

cursor :: Clocked -> Word
cursor c = fromOffset (tree c).state.cursor

isUndo :: Command () 'Emacs -> Bool
isUndo = \case
  Undo -> True
  _ -> False

-- | An editing command, not undo or redo.
isEdit :: Command () 'Emacs -> Bool
isEdit = \case
  Undo -> False
  Redo -> False
  _ -> True

-- | A text, and commands with the milliseconds before each.
genSession :: Gen (Text, [(Word64, Command () 'Emacs)])
genSession = (,) <$> genRunText <*> listOf ((,) <$> genGap <*> genCommand)

-- | Gaps inside the window, at its edge, and past it.
genGap :: Gen Word64
genGap = elements [0, 100, coalesceWindow, coalesceWindow + 1, 5000]

-- | A command that changes the text, unless there is nothing for it to do.
genEdit :: Gen (Command () 'Emacs)
genEdit =
  frequency
    [ (3, Insert <$> genRunText),
      (2, Delete <$> elements [minBound .. maxBound]),
      (1, Kill <$> elements [minBound .. maxBound]),
      (1, pure Yank)
    ]

-- | Edits, keys typed and deletions that join, motions, undo and redo, and
-- now and then one of the editor's other commands.
genCommand :: Gen (Command () 'Emacs)
genCommand =
  frequency
    [ (4, genEdit),
      (3, Insert . one <$> elements "ab "),
      (2, Delete <$> elements [PrevCluster, NextCluster]),
      (2, Move <$> elements [minBound .. maxBound]),
      (1, elements [SetMark, YankPop, CopySelection, CopyToRegister (Register 'r'), InsertRegister (Register 'r')]),
      (2, pure Undo),
      (1, pure Redo),
      (1, elements [HistoryPrev, App ()])
    ]
