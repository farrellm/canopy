{-# LANGUAGE PatternSynonyms #-}

-- | Turning a @GhosttyResult@ into the high level's errors (DESIGN.md §4.7).
module Ghostty.Effectful.Internal.Error
  ( resultCode,
    expectSuccess,
  )
where

import Control.Exception (throwIO)
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
