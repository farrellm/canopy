{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | Commands' modes (DESIGN.md §5.4, §10.1, D30): a command's mode cannot
-- be changed by 'coerce'. The 'Coercion' here, from an Emacs command to a vi
-- normal-mode one, must not type-check; deferred, the type error is thrown
-- when it is evaluated. Every command so far is polymorphic in its mode, so
-- without the role annotation GHC would infer a phantom role, and it would.
module Test.Tendril.Edit.Mode (tests) where

import Control.Exception (TypeError (..), evaluate, try)
import Data.Type.Coercion (Coercion (..))
import Tendril.Edit.Command (Command, ModeKind (..))
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
          Right (_ :: Coercion (Command () 'Emacs) (Command () 'ViNormal)) -> assertFailure "a Command () 'Emacs coerces to a Command () 'ViNormal"
    ]

-- Its own binding, so its deferred error is a thunk the test forces.
emacsToVi :: Coercion (Command () 'Emacs) (Command () 'ViNormal)
emacsToVi = Coercion
