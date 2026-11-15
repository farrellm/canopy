{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | Units (DESIGN.md §10.1, D30): an offset's unit cannot be changed by
-- 'coerce'. The 'Coercion' here, from bytes to UTF-16 code units, must not
-- type-check; deferred, the type error is thrown when it is evaluated. This
-- module imports 'Offset' without its constructor, as every user does: with
-- it in scope, the newtype would unwrap and the coercion type-check whatever
-- the role.
module Test.Tendril.Text.Units (tests) where

import Control.Exception (TypeError (..), evaluate, try)
import Data.Type.Coercion (Coercion (..))
import Tendril.Text.Units (Offset, Unit (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Tendril.Text.Units"
    [ testCase "coerce cannot change an offset's unit" $ do
        r <- try (evaluate byteToUtf16)
        case r of
          Left (TypeError _) -> pass
          Right Coercion -> assertFailure "an Offset 'Byte coerces to an Offset 'Utf16"
    ]

-- Its own binding, so its deferred error is a thunk the test forces.
byteToUtf16 :: Coercion (Offset 'Byte) (Offset 'Utf16)
byteToUtf16 = Coercion
