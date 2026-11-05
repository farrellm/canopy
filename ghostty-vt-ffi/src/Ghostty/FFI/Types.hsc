{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Types shared by every libghostty-vt header: the result code and the
-- borrowed string (@include/ghostty/vt/types.h@, DESIGN.md §3.2).
--
-- C enums are open, so each is a newtype over the C integer with pattern
-- synonyms and an @Unknown@ view (§3.3).
module Ghostty.FFI.Types
  ( -- * Results
    GhosttyResult (..),
    pattern GhosttySuccess,
    pattern GhosttyOutOfMemory,
    pattern GhosttyInvalidValue,
    pattern GhosttyOutOfSpace,
    pattern GhosttyNoValue,
    pattern GhosttyIoError,
    pattern GhosttyLimitExceeded,
    pattern GhosttyRejected,
    pattern GhosttyResultUnknown,

    -- * Strings
    GhosttyString (..),
  )
where

import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))

#include <ghostty/vt.h>

-- | @GhosttyResult@: the return value of every fallible call.
newtype GhosttyResult = GhosttyResult CInt
  deriving stock (Eq, Show)
  deriving newtype (Storable)

-- | @GHOSTTY_SUCCESS@.
pattern GhosttySuccess :: GhosttyResult
pattern GhosttySuccess = GhosttyResult (#const GHOSTTY_SUCCESS)

-- | @GHOSTTY_OUT_OF_MEMORY@.
pattern GhosttyOutOfMemory :: GhosttyResult
pattern GhosttyOutOfMemory = GhosttyResult (#const GHOSTTY_OUT_OF_MEMORY)

-- | @GHOSTTY_INVALID_VALUE@.
pattern GhosttyInvalidValue :: GhosttyResult
pattern GhosttyInvalidValue = GhosttyResult (#const GHOSTTY_INVALID_VALUE)

-- | @GHOSTTY_OUT_OF_SPACE@.
pattern GhosttyOutOfSpace :: GhosttyResult
pattern GhosttyOutOfSpace = GhosttyResult (#const GHOSTTY_OUT_OF_SPACE)

-- | @GHOSTTY_NO_VALUE@.
pattern GhosttyNoValue :: GhosttyResult
pattern GhosttyNoValue = GhosttyResult (#const GHOSTTY_NO_VALUE)

-- | @GHOSTTY_IO_ERROR@.
pattern GhosttyIoError :: GhosttyResult
pattern GhosttyIoError = GhosttyResult (#const GHOSTTY_IO_ERROR)

-- | @GHOSTTY_LIMIT_EXCEEDED@.
pattern GhosttyLimitExceeded :: GhosttyResult
pattern GhosttyLimitExceeded = GhosttyResult (#const GHOSTTY_LIMIT_EXCEEDED)

-- | @GHOSTTY_REJECTED@.
pattern GhosttyRejected :: GhosttyResult
pattern GhosttyRejected = GhosttyResult (#const GHOSTTY_REJECTED)

-- | Any value this binding does not name: a newer library's result code.
pattern GhosttyResultUnknown :: CInt -> GhosttyResult
pattern GhosttyResultUnknown n <- (unknownResult -> Just n)
  where
    GhosttyResultUnknown n = GhosttyResult n

unknownResult :: GhosttyResult -> Maybe CInt
unknownResult r@(GhosttyResult n)
  | r `elem` known = Nothing
  | otherwise = Just n
  where
    known =
      [ GhosttySuccess,
        GhosttyOutOfMemory,
        GhosttyInvalidValue,
        GhosttyOutOfSpace,
        GhosttyNoValue,
        GhosttyIoError,
        GhosttyLimitExceeded,
        GhosttyRejected
      ]

{-# COMPLETE
  GhosttySuccess,
  GhosttyOutOfMemory,
  GhosttyInvalidValue,
  GhosttyOutOfSpace,
  GhosttyNoValue,
  GhosttyIoError,
  GhosttyLimitExceeded,
  GhosttyRejected,
  GhosttyResultUnknown
  #-}

-- | @GhosttyString@: a borrowed byte string. The pointer's lifetime is the
-- producing call's to document.
data GhosttyString = GhosttyString
  { -- | @ptr@: the bytes.
    ptr :: Ptr Word8,
    -- | @len@: their count.
    len :: CSize
  }
  deriving stock (Eq, Show)

instance Storable GhosttyString where
  sizeOf _ = #size GhosttyString
  alignment _ = #alignment GhosttyString
  peek p =
    GhosttyString
      <$> (#peek GhosttyString, ptr) p
      <*> (#peek GhosttyString, len) p
  poke p s = do
    (#poke GhosttyString, ptr) p s.ptr
    (#poke GhosttyString, len) p s.len
