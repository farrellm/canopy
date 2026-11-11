{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | Regions (DESIGN.md §4.2, §4.3): a handle's region cannot be changed by
-- 'coerce'. Each 'Coercion' here, from a handle in region @Int@ to one in
-- @()@, must not type-check; deferred, the type error is thrown when it is
-- evaluated. One that did type-check would evaluate without one.
module Test.Ghostty.Effectful.Region (tests) where

import Control.Exception (TypeError (..), evaluate, try)
import Data.Type.Coercion (Coercion (..))
import Ghostty.Effectful.Encode (KeyEncoder, MouseEncoder)
import Ghostty.Effectful.Render (Frame, RenderState, Row)
import Ghostty.Effectful.Terminal (Terminal, TrackedRef)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Region"
    [ -- Regression 14 (DESIGN.md §4.2): every region parameter was phantom,
      -- so 'coerce' took a handle out of its region without its
      -- constructor, as `withTerminal cfg (pure . coerce)` did a terminal
      -- the region then freed. Each is nominal now, as ST's is.
      testGroup
        "regression 14: coerce cannot take a handle out of its region"
        [ refused "Terminal" terminal,
          refused "TrackedRef" trackedRef,
          refused "RenderState" renderState,
          refused "Frame's render state" frameState,
          refused "Frame's frame" frameTag,
          refused "Row's render state" rowState,
          refused "Row's frame" rowTag,
          refused "KeyEncoder" keyEncoder,
          refused "MouseEncoder" mouseEncoder
        ]
    ]

-- Each escape is its own binding, so its deferred error is a thunk that
-- 'refused' forces. In the list, every one would be forced as the tree is
-- built, before any test ran. The regions are 'Int' and '()', not a type
-- variable: most of these types are poly-kinded, so a variable would be of
-- another kind and refused whatever the role.

terminal :: Coercion (Terminal Int) (Terminal ())
terminal = Coercion

trackedRef :: Coercion (TrackedRef Int) (TrackedRef ())
trackedRef = Coercion

renderState :: Coercion (RenderState Int) (RenderState ())
renderState = Coercion

frameState :: Coercion (Frame Int ()) (Frame () ())
frameState = Coercion

frameTag :: Coercion (Frame () Int) (Frame () ())
frameTag = Coercion

rowState :: Coercion (Row Int ()) (Row () ())
rowState = Coercion

rowTag :: Coercion (Row () Int) (Row () ())
rowTag = Coercion

keyEncoder :: Coercion (KeyEncoder Int) (KeyEncoder ())
keyEncoder = Coercion

mouseEncoder :: Coercion (MouseEncoder Int) (MouseEncoder ())
mouseEncoder = Coercion

-- | The coercion did not type-check: evaluating it throws its deferred
-- type error.
refused :: String -> Coercion a b -> TestTree
refused name c = testCase name $ do
  r <- try (evaluate c)
  case r of
    Left (TypeError _) -> pass
    Right Coercion -> assertFailure (name <> "'s region can be coerced")
