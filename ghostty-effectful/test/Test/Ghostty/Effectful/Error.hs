{-# LANGUAGE PatternSynonyms #-}

-- | The shared result handling (DESIGN.md §4.7): every refused call goes
-- through "Ghostty.Effectful.Internal.Error", so each is checked once here.
module Test.Ghostty.Effectful.Error (tests) where

import Control.Exception (try)
import Foreign.Storable (poke)
import Ghostty.Effectful.Error (CallSite (..), GhosttyError (..), ResultCode (..))
import Ghostty.Effectful.Internal.Error (bytesVia, expectSuccess, optionalVia, outVia, succeedsUnless)
import Ghostty.FFI.Types (pattern GhosttyInvalidValue, pattern GhosttyNoValue, pattern GhosttyOutOfMemory, pattern GhosttyOutOfSpace, pattern GhosttySuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Error"
    [ -- Regression 13 (DESIGN.md §4.7): seven calls with a documented refusal
      -- switched on their result by hand, and five of them threw
      -- 'Invariant' for running out of memory. Each now goes through the
      -- helper checked here.
      testCase "regression 13: running out of memory is OutOfMemory, whatever else a call may refuse" $ do
        a <- try (optionalVia @Word8 ResultNoValue site (const (pure GhosttyOutOfMemory)))
        b <- try (succeedsUnless ResultInvalidValue site GhosttyOutOfMemory)
        c <- try (outVia @Word8 site (const (pure GhosttyOutOfMemory)))
        d <- try (bytesVia site (\_ _ -> pure (GhosttyOutOfMemory, 0)))
        (void a, void b, void c, void d)
          @?= (Left (OutOfMemory site), Left (OutOfMemory site), Left (OutOfMemory site), Left (OutOfMemory site)),
      testCase "a documented refusal is a value, and anything else an invariant" $ do
        refused <- optionalVia @Word8 ResultNoValue site (const (pure GhosttyNoValue))
        refused @?= Nothing
        other <- try (succeedsUnless ResultNoValue site GhosttyInvalidValue)
        other @?= Left (Invariant site ResultInvalidValue),
      testCase "success is the value written" $ do
        ok <- outVia @Word8 site (\p -> GhosttySuccess <$ poke p 7)
        ok @?= 7
        expectSuccess site GhosttySuccess,
      testCase "a buffer too small is retried at the size asked for" $ do
        bs <- bytesVia site $ \_ cap -> pure (if cap < 100 then (GhosttyOutOfSpace, 100) else (GhosttySuccess, 0))
        bs @?= ""
    ]
  where
    site = CallSite "test"
