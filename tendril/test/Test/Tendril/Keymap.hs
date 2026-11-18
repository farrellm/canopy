-- | Keymap tries (DESIGN.md §5.4): a binding is found where it was made,
-- a prefix of one waits for more, and conflicts are exactly the pairs of
-- sequences where one starts the other.
module Test.Tendril.Keymap (tests) where

import Tendril.Keymap (Conflict (..), Key (..), Lookup (..), Mods (..), bind, conflicts, describe, emptyTrie, key, keymap, toBindings, typed, walk, withAlt, withCtrl, withShift)
import Test.QuickCheck (Gen, choose, conjoin, counterexample, forAll, listOf, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
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
      testProperty "conflicts are the pairs where one sequence starts the other" $
        forAll genBindings $ \bs ->
          let seqs = zip [0 :: Int ..] (map (toList . fst) bs)
              naive = [(a, b) | (i, a) <- seqs, (j, b) <- seqs, i < j, a `isPrefixOf` b || b `isPrefixOf` a]
           in [(toList x.earlier, toList x.later) | x <- conflicts bs] === naive,
      testCase "shift is dropped from a character key, kept on others" $ do
        withShift (key (Char 'a')) @?= key (Char 'a')
        (withShift (key Tab)).mods.shift @?= True,
      testCase "describe" $ do
        describe (withCtrl (key (Char ' '))) @?= "C-SPC"
        describe (withAlt (key Backspace)) @?= "M-DEL"
        describe (withCtrl (withShift (key (F 5)))) @?= "C-S-<f5>",
      testCase "typed: only a character key, with no ctrl, alt or super" $ do
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
      | any (\(ks', _) -> clash (toList ks) (toList ks')) acc = acc
      | otherwise = (ks, v) : acc
    clash a b = a `isPrefixOf` b || b `isPrefixOf` a
