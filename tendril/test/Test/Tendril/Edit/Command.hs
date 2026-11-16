-- | The editing model (DESIGN.md §5.4, §11.3): commands keep the cursor and
-- the anchor on cluster boundaries and the rope's metrics right, and each
-- does what its name says.
module Test.Tendril.Edit.Command (tests) where

import Data.Text qualified as T
import Data.Text.Mixed.Rope qualified as Rope
import Tendril.Edit.Command (Command (..), EditState, ModeKind (..), apply, editState)
import Tendril.Edit.KillRing (current, entries)
import Tendril.Edit.Motion (Motion (..))
import Tendril.Edit.Register (Register (..))
import Tendril.Edit.Selection (Selection (..))
import Tendril.Text.Buffer (fromText, toRope)
import Tendril.Text.Grapheme (boundaries)
import Tendril.Text.Units (fromOffset, offset)
import Test.QuickCheck (Gen, Property, conjoin, counterexample, elements, forAll, frequency, listOf, (===), (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Tendril.Text.Gen (genRunText, genText)

tests :: TestTree
tests =
  testGroup
    "Tendril.Edit.Command"
    [ testProperty "every command keeps the cursor and anchor on boundaries, and the rope's metrics" $
        forAll genRunText $ \t ->
          forAll (listOf genCommand) $ \cs ->
            conjoin
              [ counterexample (show (c, s.buffer, s.selection)) (wellFormed s)
              | (c, s) <- zip (Nothing : map Just cs) (run (editState (fromText t)) cs)
              ],
      testProperty "NextCluster from the start visits every boundary" $
        forAll genRunText $ \t ->
          let s0 = edits (editState (fromText t)) [Move BufferStart]
              visits = map (.cursor) (take (T.length t + 1) (iterate (edits' [Move NextCluster]) s0))
           in ordNub visits === boundaries t,
      testProperty "PrevCluster from the end visits every boundary" $
        forAll genRunText $ \t ->
          let visits = map (.cursor) (take (T.length t + 1) (iterate (edits' [Move PrevCluster]) (editState (fromText t))))
           in ordNub visits === reverse (boundaries t),
      testProperty "Insert puts its text at the cursor" $
        forAll genRunText $ \t ->
          forAll genText $ \u ->
            forAll (listOf genMove) $ \ms ->
              let s = edits (editState (fromText t)) ms
                  (before, after) = T.splitAt (fromIntegral (fromOffset s.cursor)) t
               in toText (edits s [Insert u]).buffer === before <> u <> after,
      testProperty "a yank puts back what a kill took, if the cursor stayed" $
        forAll genRunText $ \t ->
          forAll (listOf genMove) $ \ms ->
            forAll (elements [minBound .. maxBound]) $ \m ->
              let s = edits (editState (fromText t)) ms
                  killed = edits s [Kill m]
               in (killed.cursor == min s.cursor (edits s [Move m]).cursor) ==>
                    toText (edits killed [Yank]).buffer === t,
      testCase "an insertion before a combining mark moves past it" $
        cursorAfter "\x0301" [Move BufferStart, Insert "e"] ("e\x0301", 2),
      testCase "a deletion that joins two clusters moves to the joined one's start" $
        -- Hangul L and V join into one cluster once the letter between them goes.
        cursorAfter "\x1100\&a\x1161" [Move BufferStart, Move NextCluster, Move NextCluster, Delete PrevCluster] ("\x1100\x1161", 0),
      testCase "word motions" $ do
        let at n = edits (editState "foo  bar.baz") [Move BufferStart] & \s -> iterateN n (Move NextWord) s
        map (fromOffset . (.cursor) . at) [1, 2, 3] @?= [3, 8, 12]
        (edits (editState "foo  bar.baz") [Move PrevWord, Move PrevWord]).cursor @?= offset 5,
      testCase "vertical motions keep their goal column" $ do
        let s = editState "abcd\nx\nabcd"
        map (fromOffset . (.cursor)) (scanl (\s' c -> edits s' [c]) s [Move LineUp, Move LineUp, Move LineDown, Move LineDown, Move PrevCluster, Move LineUp])
          @?= [11, 6, 4, 6, 11, 10, 6],
      testCase "a line ending in CR LF ends before the CR" $ do
        let s = edits (editState "ab\r\ncd") [Move BufferStart]
        (edits s [Move LineEnd]).cursor @?= offset 2
        (edits s [Move LineDown]).cursor @?= offset 4
        (edits s [Move LineDown, Move LineUp]).cursor @?= offset 0,
      testCase "consecutive kills join, forward at the end and back at the start" $ do
        let s = edits (editState "one two three four") [Move PrevWord, Move PrevWord, Kill PrevWord, Kill PrevWord, Kill NextWord]
        toText s.buffer @?= " four"
        entries s.killRing @?= ["one two three"]
        let s' = edits s [Move NextCluster, Kill NextWord]
        entries s'.killRing @?= ["four", "one two three"],
      testCase "a yank-pop cycles what the yank inserted" $ do
        let s = edits (editState "a b") [Kill PrevWord, Move BufferStart, Kill NextWord, Yank]
        (toText s.buffer, current s.killRing) @?= ("a ", Just "a")
        let s' = edits s [YankPop]
        (toText s'.buffer, s'.cursor) @?= ("b ", offset 1)
        toText (edits s' [YankPop]).buffer @?= "a ",
      testCase "a yank-pop not after a yank does nothing" $ do
        let s = edits (editState "a b") [Kill PrevWord, Insert "x"]
        toText (edits s [YankPop]).buffer @?= "a x",
      testCase "the selection: a motion keeps the anchor, an edit ends it" $ do
        let s = edits (editState "hello world") [Move BufferStart, SetMark, Move NextWord]
        s.selection @?= Selection (offset 5) (Just (offset 0))
        let k = edits s [KillSelection]
        (toText k.buffer, k.selection, current k.killRing) @?= (" world", Selection (offset 0) Nothing, Just "hello")
        (edits s [Insert "!"]).selection @?= Selection (offset 6) Nothing
        (edits (editState "ab") [Select PrevCluster, Select PrevCluster]).selection @?= Selection (offset 0) (Just (offset 2)),
      testCase "registers hold the selected text" $ do
        let s = edits (editState "ab") [Select PrevCluster, CopyToRegister (Register 'r'), Move BufferStart, InsertRegister (Register 'r')]
        (toText s.buffer, s.cursor) @?= ("bab", offset 1)
        toText (edits s [InsertRegister (Register 'q')]).buffer @?= "bab",
      -- Regression 19 (DESIGN.md §5.4): a yank-pop moved the newest entry
      -- to the back of the ring, so a kill after it went in front of a
      -- reordered ring, and the next yank-pop found the wrong entry.
      testCase "regression 19: a yank-pop moves a pointer, not the entries" $ do
        let s = edits (editState "x y z") [Kill PrevWord, Move PrevCluster, Kill PrevWord, Yank, YankPop]
        (toText s.buffer, current s.killRing) @?= ("x z ", Just "z")
        let s' = edits s [Move BufferStart, Kill NextWord]
        (entries s'.killRing, current s'.killRing) @?= (["x", "y", "z"], Just "x")
        toText (edits s' [Yank, YankPop]).buffer @?= "y z ",
      -- Regression 20 (DESIGN.md §5.4): a kill of nothing ended a run of
      -- kills, and left the selection it was given.
      testCase "regression 20: a kill of nothing keeps the run and ends the selection" $ do
        let s = edits (editState "ab\ncd") [Move BufferStart, Kill NextWord, Kill LineEnd, Kill NextWord]
        entries s.killRing @?= ["ab\ncd"]
        (edits (editState "ab") [SetMark, KillSelection]).selection @?= Selection (offset 2) Nothing,
      testCase "the editor's commands are not edits" $
        forM_ [Undo, Redo, AcceptOrNewline, HistoryPrev, HistoryNext, ExpandSelection, ShrinkSelection, EditExternal, App ()] $ \c ->
          when (isJust (apply c (editState "x"))) (assertFailure (show c))
    ]

-- | The cursor and anchor are boundaries of the text, and the rope's metrics
-- are those of a rope built from its text afresh.
wellFormed :: EditState -> Property
wellFormed s =
  let t = toText s.buffer
      bs = boundaries t
      metrics r = (Rope.charLength r, Rope.utf8Length r, Rope.utf16Length r, Rope.lengthInLines r)
   in conjoin
        [ counterexample "cursor" (s.cursor `elem` bs),
          counterexample "anchor" (all (`elem` bs) s.selection.anchor),
          metrics (toRope s.buffer) === metrics (Rope.fromText t)
        ]

-- | Every state from the first through each command's.
run :: EditState -> [Command () 'Emacs] -> [EditState]
run = scanl (\s c -> fromMaybe s (apply c s))

edits :: EditState -> [Command () 'Emacs] -> EditState
edits s cs = fromMaybe s (viaNonEmpty last (run s cs))

edits' :: [Command () 'Emacs] -> EditState -> EditState
edits' = flip edits

iterateN :: Int -> Command () 'Emacs -> EditState -> EditState
iterateN n c s = edits s (replicate n c)

cursorAfter :: Text -> [Command () 'Emacs] -> (Text, Word) -> Assertion
cursorAfter t cs expected =
  let s = edits (editState (fromText t)) cs in (toText s.buffer, fromOffset s.cursor) @?= expected

genMove :: Gen (Command () 'Emacs)
genMove = Move <$> elements [minBound .. maxBound]

-- | Any editing command, and now and then one of the editor's.
genCommand :: Gen (Command () 'Emacs)
genCommand =
  frequency
    [ (3, Insert <$> genRunText),
      (1, pure ForceNewline),
      (4, genMove),
      (2, Select <$> motion),
      (1, elements [SetMark, Deselect, KillSelection, CopySelection, Yank, YankPop]),
      (2, Delete <$> motion),
      (2, Kill <$> motion),
      (1, CopyToRegister . Register <$> elements "ab"),
      (1, InsertRegister . Register <$> elements "ab"),
      (1, elements [Undo, HistoryPrev, App ()])
    ]
  where
    motion = elements [minBound .. maxBound]
