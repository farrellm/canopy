{-# LANGUAGE PatternSynonyms #-}

-- | Turning a @GhosttyResult@ into the high level's errors (DESIGN.md §4.7).
module Ghostty.Effectful.Internal.Error
  ( resultCode,
    resultError,
    expectSuccess,
    succeedsUnless,
    orInvariant,
    newHandle,
    configured,
    outVia,
    outAt,
    optionalVia,
    optionalAtWith,
    bytesVia,
    bytesAt,
    bytesAtWith,
    firstTry,
  )
where

import Control.Exception (onException, throwIO)
import Data.ByteString qualified as BS
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
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
    pattern GhosttyUnsupported,
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
  GhosttyUnsupported -> ResultUnsupported
  GhosttyResultUnknown n -> ResultUnknown (fromIntegral n)

-- | What a refused call throws: 'OutOfMemory' for running out of memory,
-- and 'Invariant' for anything else, which the header says cannot happen.
resultError :: CallSite -> ResultCode -> GhosttyError
resultError site = \case
  ResultOutOfMemory -> OutOfMemory site
  other -> Invariant site other

-- | For a call whose only documented failure is running out of memory:
-- success returns, and anything else throws its 'resultError'.
expectSuccess :: CallSite -> GhosttyResult -> IO ()
expectSuccess site r = case resultCode r of
  ResultSuccess -> pass
  other -> throwIO (resultError site other)

-- | For a call with one documented refusal besides running out of memory:
-- 'True' on success, 'False' on that refusal, and anything else throws its
-- 'resultError'.
succeedsUnless :: ResultCode -> CallSite -> GhosttyResult -> IO Bool
succeedsUnless refusal site r = case resultCode r of
  ResultSuccess -> pure True
  c | c == refusal -> pure False
  other -> throwIO (resultError site other)
-- Inlined, as 'outVia' is: out of line, a caller builds its call site's
-- thunk on every call, where inlined it is built only on the error path.
{-# INLINE succeedsUnless #-}

-- | A value the header says is there, or 'Invariant' with the result that
-- stands for its absence.
orInvariant :: CallSite -> ResultCode -> Maybe a -> IO a
orInvariant site code = maybe (throwIO (Invariant site code)) pure

-- | A handle from a @…_new(allocator, &out)@ call, with the default
-- allocator (DESIGN.md §3.3); its only documented failure is running out of
-- memory ('expectSuccess').
newHandle :: (Storable h) => CallSite -> (Ptr allocator -> Ptr h -> IO GhosttyResult) -> IO h
newHandle site new = outVia site (new nullPtr)

-- | A handle with its configuration set, or freed if setting it throws, so a
-- handle whose options are refused is never left behind (regression 16).
configured :: (h -> IO ()) -> (h -> IO ()) -> h -> IO h
configured set free h = h <$ set h `onException` free h

-- | The value a call writes to its out-parameter, for a call whose only
-- documented failure is running out of memory ('expectSuccess').
outVia :: (Storable b) => CallSite -> (Ptr b -> IO GhosttyResult) -> IO b
outVia site call = alloca $ \out -> outAt out site call
-- Inlined, as 'optionalVia' is: a read is a few loads around a C call, and
-- out of line it would take the 'Storable' dictionary and a closure for the
-- call on every read.
{-# INLINE outVia #-}

-- | 'outVia' into memory the caller provides, which a run of reads reuses:
-- each 'alloca' is a pinned allocation, which costs more than the read.
outAt :: (Storable b) => Ptr b -> CallSite -> (Ptr b -> IO GhosttyResult) -> IO b
outAt out site call = do
  expectSuccess site =<< call out
  peek out
{-# INLINE outAt #-}

-- | 'outVia' for a call with one documented refusal besides running out of
-- memory ('succeedsUnless'): 'Nothing' on that refusal.
optionalVia :: (Storable b) => ResultCode -> CallSite -> (Ptr b -> IO GhosttyResult) -> IO (Maybe b)
optionalVia refusal site call = alloca $ \out -> optionalAtWith out id refusal site call
{-# INLINE optionalVia #-}

-- | 'optionalVia' into memory the caller provides, as 'outAt' is, the value
-- converted as it is read. (An 'fmap' over the result would build the
-- 'Just' twice, which costs the per-cell colour reads.)
optionalAtWith :: (Storable b) => Ptr b -> (b -> c) -> ResultCode -> CallSite -> (Ptr b -> IO GhosttyResult) -> IO (Maybe c)
optionalAtWith out convert refusal site call = do
  r <- call out
  case resultCode r of
    ResultSuccess -> Just . convert <$> peek out
    c
      | c == refusal -> pure Nothing
      | otherwise -> throwIO (resultError site c)
{-# INLINE optionalAtWith #-}

-- | The bytes a call writes into a buffer: given the buffer and its
-- capacity, the call returns its result and the length it wrote, or, with
-- @GHOSTTY_OUT_OF_SPACE@, the length it needs. The first try's buffer holds
-- nearly everything; a second, of the size asked for, cannot be too small
-- unless the call is not the same twice, so a short buffer never escapes
-- (DESIGN.md §4.7). Any other result throws its 'resultError'.
bytesVia :: CallSite -> (Ptr Word8 -> Int -> IO (GhosttyResult, Int)) -> IO ByteString
bytesVia site call = allocaBytes firstTry $ \buf -> bytesAt buf firstTry site call
{-# INLINE bytesVia #-}

-- | The capacity of 'bytesVia''s first try, which nearly every string fits.
firstTry :: Int
firstTry = 64

-- | 'bytesVia', its first try into a buffer of the given capacity the
-- caller provides, as 'outAt''s is.
bytesAt :: Ptr Word8 -> Int -> CallSite -> (Ptr Word8 -> Int -> IO (GhosttyResult, Int)) -> IO ByteString
bytesAt = bytesAtWith (\buf n -> BS.packCStringLen (castPtr buf, n))
{-# INLINE bytesAt #-}

-- | 'bytesAt', the bytes converted by the given reader while they are in
-- the buffer, which may be the caller's or one freed on return: the
-- reader must not keep the pointer (it decodes or copies).
bytesAtWith :: (Ptr Word8 -> Int -> IO a) -> Ptr Word8 -> Int -> CallSite -> (Ptr Word8 -> Int -> IO (GhosttyResult, Int)) -> IO a
bytesAtWith readBytes buf0 cap0 site call = attempt buf0 cap0 >>= either (retry . max 1) pure
  where
    retry cap = allocaBytes cap (`attempt` cap) >>= either (const outOfSpace) pure
    attempt buf cap = do
      (r, n) <- call buf cap
      case resultCode r of
        ResultSuccess -> Right <$> readBytes buf n
        ResultOutOfSpace -> pure (Left n)
        other -> throwIO (resultError site other)
    outOfSpace = throwIO (Invariant site ResultOutOfSpace)
{-# INLINE bytesAtWith #-}
