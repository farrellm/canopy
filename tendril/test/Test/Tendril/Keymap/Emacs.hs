-- | The Emacs keymap (DESIGN.md §5.4): no two of its bindings conflict, none
-- starts with a chord that types a character, and its keys do what Emacs's
-- do.
module Test.Tendril.Keymap.Emacs (tests) where

import Tendril.Edit.Command (Command (..))
import Tendril.Edit.Motion (Motion (..))
import Tendril.Keymap (Key (..), KeyChord, Lookup (..), conflicts, describe, key, toBindings, typed, walk, withAlt, withCtrl, withShift)
import Tendril.Keymap.Emacs (bindings, emacs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Tendril.Keymap.Emacs"
    [ testCase "no two bindings conflict" $
        conflicts (bindings @()) @?= [],
      testCase "every binding is in the keymap" $
        sortOn fst (toBindings (emacs @())) @?= sortOn fst (bindings @()),
      testCase "no binding starts with a chord that types a character" $
        sequence_
          [ assertBool (toString (describe k)) (isNothing (typed k))
          | (k :| _, _) <- bindings @()
          ],
      testCase "keys" $
        sequence_
          [ (describeAll ks, walk ks (emacs @())) @?= (describeAll ks, Bound cmd)
          | (ks, cmd) <-
              [ ([c 'a'], Move LineStart),
                ([c 'e'], Move LineEnd),
                ([m 'f'], Move NextWord),
                ([withCtrl (key ArrowLeft)], Move PrevWord),
                ([m '<'], Move BufferStart),
                ([m '>'], Move BufferEnd),
                ([withShift (key ArrowRight)], Select NextCluster),
                ([c ' '], SetMark),
                ([key Backspace], Delete PrevCluster),
                ([c 'k'], Kill LineEnd),
                ([withAlt (key Backspace)], Kill PrevWord),
                ([c 'y'], Yank),
                ([m 'y'], YankPop),
                ([c '_'], Undo),
                ([c 'x', key (Char 'u')], Undo),
                ([withAlt (c '_')], Redo),
                ([key Enter], AcceptOrNewline),
                ([withAlt (key Enter)], ForceNewline),
                ([c 'x', c 'e'], EditExternal)
              ]
          ],
      testCase "C-x waits for the next chord" $
        case walk [c 'x'] (emacs @()) of
          Prefix _ -> pass
          other -> assertFailure (show other),
      testCase "a printable character is not bound" $
        walk [key (Char 'a')] (emacs @()) @?= Unbound
    ]
  where
    describeAll = map describe

c, m :: Char -> KeyChord
c = withCtrl . key . Char
m = withAlt . key . Char
