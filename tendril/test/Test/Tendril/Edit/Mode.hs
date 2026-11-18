{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | Commands' modes (DESIGN.md §5.4, §10.1, D30): a command's mode cannot
-- be changed by 'coerce', nor a keymap's, and a command of one mode cannot
-- be bound in another's keymap. Each value here must not type-check;
-- deferred, the type error is thrown when it is evaluated. Every command so
-- far is polymorphic in its mode, so without the role annotation GHC would
-- infer a phantom role, and the coercions would.
module Test.Tendril.Edit.Mode (tests) where

import Control.Exception (TypeError (..), evaluate, try)
import Data.Type.Coercion (Coercion (..))
import Tendril.Edit.Command (Command (..), ModeKind (..))
import Tendril.Edit.Motion (Motion (..))
import Tendril.Keymap (Key (..), Keymap, bind, key)
import Tendril.Keymap.Emacs (emacs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Tendril.Edit.Command modes"
    [ testCase "coerce cannot change a command's mode" $ do
        r <- try (evaluate emacsToVi)
        case r of
          Left (TypeError _) -> pass
          Right (_ :: Coercion (Command () 'Emacs) (Command () 'ViNormal)) -> assertFailure "a Command () 'Emacs coerces to a Command () 'ViNormal",
      testCase "coerce cannot change a keymap's mode" $ do
        r <- try (evaluate emacsKeymapToVi)
        case r of
          Left (TypeError _) -> pass
          Right (_ :: Coercion (Keymap () 'Emacs) (Keymap () 'ViNormal)) -> assertFailure "a Keymap () 'Emacs coerces to a Keymap () 'ViNormal",
      testCase "a vi normal-mode command cannot be bound in the Emacs keymap" $ do
        r <- try (evaluate (length (show @String viInEmacs)))
        case r of
          Left (TypeError _) -> pass
          Right _ -> assertFailure "a Command () 'ViNormal was bound in a Keymap () 'Emacs"
    ]

-- Its own binding, so its deferred error is a thunk the test forces.
emacsToVi :: Coercion (Command () 'Emacs) (Command () 'ViNormal)
emacsToVi = Coercion

emacsKeymapToVi :: Coercion (Keymap () 'Emacs) (Keymap () 'ViNormal)
emacsKeymapToVi = Coercion

viNormal :: Command () 'ViNormal
viNormal = Move NextCluster

viInEmacs :: Keymap () 'Emacs
viInEmacs = bind (key (Char 'l') :| []) viNormal emacs
