{-# LANGUAGE PatternSynonyms #-}

-- | Turning a @GhosttyResult@ into the high level's errors (DESIGN.md §4.7).
module Ghostty.Effectful.Internal.Error
  ( resultCode,
    expectSuccess,
    newHandle,
  )
where

import Control.Exception (throwIO)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable, peek)
import Ghostty.Effectful.Error (CallSite, GhosttyError (..), ResultCode (..))
import Ghostty.FFI.Types
  ( GhosttyResult (..),
    pattern GhosttyInvalidValue,
    pattern GhosttyIoError,
    pattern GhosttyLimitExceeded,
    pattern GhosttyNoValue,
    pattern GhosttyOutOfMemory,
    pattern GhosttyOutOfSpace,
    pattern GhosttyRejected,
    pattern GhosttyResultUnknown,
    pattern GhosttySuccess,
  )

-- | The closed form of a result.
resultCode :: GhosttyResult -> ResultCode
resultCode = \case
  GhosttySuccess -> ResultSuccess
  GhosttyOutOfMemory -> ResultOutOfMemory
  GhosttyInvalidValue -> ResultInvalidValue
  GhosttyOutOfSpace -> ResultOutOfSpace
  GhosttyNoValue -> ResultNoValue
  GhosttyIoError -> ResultIoError
  GhosttyLimitExceeded -> ResultLimitExceeded
  GhosttyRejected -> ResultRejected
  GhosttyResultUnknown n -> ResultUnknown (fromIntegral n)

-- | For a call whose only documented failure is running out of memory:
-- success returns, out of memory throws 'OutOfMemory', and anything else
-- throws 'Invariant'.
expectSuccess :: CallSite -> GhosttyResult -> IO ()
expectSuccess site r = case resultCode r of
  ResultSuccess -> pass
  ResultOutOfMemory -> throwIO (OutOfMemory site)
  other -> throwIO (Invariant site other)

-- | A handle from a @…_new(allocator, &out)@ call, with the default
-- allocator (DESIGN.md §3.3); its only documented failure is running out of
-- memory ('expectSuccess').
newHandle :: (Storable h) => CallSite -> (Ptr allocator -> Ptr h -> IO GhosttyResult) -> IO h
newHandle site new = alloca $ \out -> do
  expectSuccess site =<< new nullPtr out
  peek out
