-- | Keymap tries (DESIGN.md §5.4): a binding is found where it was made,
-- a prefix of one waits for more, and conflicts are exactly the bindings a
-- later one replaced. Chords are in normal form, and described as Emacs
-- reads them.
module Test.Tendril.Keymap (tests) where

import Data.Char (isControl)
import Data.List (partition)
import Tendril.Keymap (Conflict (..), Key (..), KeyChord, Lookup (..), Mods (..), bind, chord, conflicts, describe, emptyTrie, key, keymap, keymapWithConflicts, noMods, printableStarts, toBindings, typed, walk, withAlt, withCtrl, withShift, withSuper)
import Test.QuickCheck (Gen, choose, conjoin, counterexample, forAll, listOf, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?), (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Tendril.Keymap"
    [ testProperty "a sequence just bound is bound to its value" $
        forAll genBindings $ \bs ->
          forAll genSeq $ \ks ->
            walk (toList ks) (bind ks 'v' (keymap bs)) === Bound 'v',
      testProperty "with no conflicts, every binding is found, and only those" $
        forAll genBindings $ \bs0 ->
          let bs = withoutConflicts bs0
              t = keymap bs
           in conjoin
                [ counterexample (show ks) (walk (toList ks) t === Bound v)
                | (ks, v) <- bs
                ]
                .&&. sortOn fst (toBindings t) === sortOn fst bs,
      testProperty "every proper prefix of a binding is a prefix" $
        forAll genBindings $ \bs ->
          let t = keymap bs
           in conjoin
                [ counterexample (show p) (isPrefix (walk p t))
                | (ks, _) <- toBindings t,
                  p <- inits' (toList ks)
                ],
      testProperty "the last binding made is found, whatever came before" $
        forAll genBindings $ \bs ->
          case viaNonEmpty last bs of
            Nothing -> walk [] (keymap bs) === walk [] emptyTrie
            Just (ks, v) -> walk (toList ks) (keymap bs) === Bound v,
      testProperty "conflicts are what a list of the live bindings drops" $
        forAll genBindings $ \bs ->
          [(x.earlier, x.later) | x <- conflicts bs] === replaced bs,
      testProperty "each binding is in the keymap or in conflicts, once" $
        forAll genBindings $ \bs ->
          length (toBindings (keymap bs)) + length (conflicts bs) === length bs,
      testProperty "conflicts are empty exactly when no two sequences clash" $
        forAll genBindings $ \bs ->
          let seqs = zip [0 :: Int ..] (map fst bs)
           in null (conflicts bs) === and [not (clash a b) | (i, a) <- seqs, (j, b) <- seqs, i < j],
      testProperty "keymapWithConflicts is keymap and conflicts" $
        forAll genBindings $ \bs ->
          keymapWithConflicts bs === (keymap bs, conflicts bs),
      testCase "shift is applied to a letter, and kept on other keys" $ do
        withShift (key (Char 'A')) @?= key (Char 'A')
        withShift (key (Char 'é')) @?= key (Char 'É')
        (withShift (key (Char '1'))).mods.shift @?= True
        (withShift (key Tab)).mods.shift @?= True,
      -- Regression 29 (DESIGN.md §5.4, D36): chord dropped shift from every
      -- character key, so Ctrl+Shift+/ given as '/' with ctrl and shift was
      -- C-/ (Undo, not Redo), and Shift+a given as 'a' typed "a".
      testCase "regression 29: chord applies shift to a letter and keeps it on others" $ do
        withShift (key (Char 'a')) @?= key (Char 'A')
        typed (withShift (key (Char 'a'))) @?= Just "A"
        withCtrl (withShift (key (Char '/'))) /= withCtrl (key (Char '/')) @? "C-S-/ is not C-/"
        describe (withCtrl (withShift (key (Char '/')))) @?= "C-S-/"
        typed (withShift (key (Char '/'))) @?= Nothing
        chord shiftOnly (Char '\SOH') @?= withCtrl (key (Char 'A')),
      testCase "describe" $ do
        describe (withCtrl (key (Char ' '))) @?= "C-SPC"
        describe (withAlt (key Backspace)) @?= "M-DEL"
        describe (withCtrl (withShift (key (F 5)))) @?= "C-S-<f5>"
        describe (key Enter) @?= "RET"
        describe (withAlt (key Tab)) @?= "M-TAB",
      -- Regression 30 (DESIGN.md §5.4): describe wrote Enter, Tab, Backspace
      -- and Escape with ctrl, shift or super in their control characters'
      -- names (C-DEL, S-RET), which Emacs reads as other keys.
      testCase "regression 30: modified Enter, Tab, Backspace and Escape have their own names" $ do
        describe (withCtrl (key Backspace)) @?= "C-<backspace>"
        describe (withShift (key Enter)) @?= "S-<return>"
        describe (withCtrl (key Tab)) @?= "C-<tab>"
        describe (withSuper (key Escape)) @?= "s-<escape>"
        describe (withAlt (withCtrl (key Backspace))) @?= "C-M-<backspace>",
      -- Regression 25 (DESIGN.md §5.4): describe put super before shift,
      -- where Emacs's order (A-C-H-M-S-s) puts shift first.
      testCase "regression 25: describe puts shift before super, as Emacs does" $
        describe (withSuper (withShift (key ArrowLeft))) @?= "S-s-<left>",
      -- Regression 26 (DESIGN.md §5.4): describe wrote Ctrl-Shift-A as C-A,
      -- which Emacs reads as C-a.
      testCase "regression 26: an ASCII capital with ctrl is described with S-" $ do
        describe (withCtrl (key (Char 'A'))) @?= "C-S-a"
        describe (withAlt (withCtrl (key (Char 'Z')))) @?= "C-M-S-z"
        describe (withAlt (key (Char 'A'))) @?= "M-A"
        describe (withCtrl (key (Char 'a'))) @?= "C-a",
      testCase "describe tells every chord apart" $
        let cs = [chord ms k | ms <- allMods, k <- [Char 'a', Char 'A', Char '?', Char ' ', Tab, Backspace, ArrowLeft]]
         in length (ordNub (map describe cs)) @?= length (ordNub cs),
      -- Regression 27 (DESIGN.md §5.4): chord kept a control character, so
      -- key (Char '\t') was not key Tab, and typed gave it as text.
      testCase "regression 27: chord takes a control character to normal form" $ do
        key (Char '\t') @?= key Tab
        key (Char '\r') @?= key Enter
        key (Char '\ESC') @?= key Escape
        key (Char '\DEL') @?= key Backspace
        key (Char '\NUL') @?= withCtrl (key (Char ' '))
        key (Char '\SOH') @?= withCtrl (key (Char 'a'))
        key (Char '\b') @?= withCtrl (key (Char 'h'))
        key (Char '\US') @?= withCtrl (key (Char '_'))
        (chord shiftOnly (Char '\t')).mods.shift @?= True
        typed (key (Char '\x85')) @?= Nothing
        sequence_
          [ assertBool (show (c, ms)) (not (isControlKey ch) && maybe True (not . any isControl . toString) (typed ch))
          | c <- ['\NUL' .. '\US'] <> ['\DEL'],
            ms <- allMods,
            let ch = chord ms (Char c)
          ],
      -- Regression 31 (DESIGN.md §5.4): typed gave a surrogate as text, which
      -- T.singleton replaces with U+FFFD.
      testCase "regression 31: a surrogate types nothing" $ do
        typed (key (Char '\xD800')) @?= Nothing
        typed (key (Char '\xDFFF')) @?= Nothing,
      testCase "printableStarts: the bindings that start with a chord that types" $
        printableStarts
          ( keymap
              [ (key (Char 'a') :| [], 'x'),
                (withCtrl (key (Char 'a')) :| [], 'y'),
                (withCtrl (key (Char 'x')) :| [key (Char 'u')], 'z'),
                (withShift (key (Char '1')) :| [], 'w')
              ]
          )
          @?= [key (Char 'a') :| []],
      testCase "typed: only a character key, with no modifier" $ do
        typed (key (Char 'é')) @?= Just "é"
        typed (withShift (key (Char 'A'))) @?= Just "A"
        typed (withAlt (key (Char 'a'))) @?= Nothing
        typed (key Enter) @?= Nothing
    ]
  where
    isPrefix = \case
      Prefix _ -> True
      _ -> False
    inits' xs = [take n xs | n <- [0 .. length xs - 1]]
    shiftOnly = noMods {shift = True}
    isControlKey :: KeyChord -> Bool
    isControlKey ch = case ch.key of
      Char c -> isControl c
      _ -> False

allMods :: [Mods]
allMods = [Mods {ctrl = c, alt = a, shift = s, super = u} | c <- bools, a <- bools, s <- bools, u <- bools]
  where
    bools = [False, True]

clash :: NonEmpty Int -> NonEmpty Int -> Bool
clash a b = toList a `isPrefixOf` toList b || toList b `isPrefixOf` toList a

-- A model of conflicts over a list of the live sequences: each binding drops
-- the live ones it clashes with, smallest first, and is live itself.
replaced :: [(NonEmpty Int, Char)] -> [(NonEmpty Int, NonEmpty Int)]
replaced = go []
  where
    go _ [] = []
    go live ((ks, _) : rest) =
      let (gone, kept) = partition (clash ks) live
       in [(g, ks) | g <- sort gone] <> go (ks : kept) rest

-- Sequences of up to three keys from four, so that prefixes and conflicts
-- are common.
genSeq :: Gen (NonEmpty Int)
genSeq = do
  k <- choose (0, 3)
  n <- choose (0, 2)
  ks <- replicateM n (choose (0, 3))
  pure (k :| ks)

genBindings :: Gen [(NonEmpty Int, Char)]
genBindings = do
  ss <- listOf genSeq
  pure (zip ss ['a' ..])

-- The bindings that conflict with none before them.
withoutConflicts :: [(NonEmpty Int, Char)] -> [(NonEmpty Int, Char)]
withoutConflicts = reverse . foldl' keep []
  where
    keep acc (ks, v)
      | any (clash ks . fst) acc = acc
      | otherwise = (ks, v) : acc
