{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
    ghosttyResultEnum,

    -- * Strings
    GhosttyString (..),
    ghosttyStringBytes,
    ghosttyStringLayout,

    -- * Type manifest
    ghosttyTypeJson,
  )
where

import Data.ByteString qualified as BS
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CChar, CInt (..), CSize (..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Ghostty.FFI.Layout (CEnum (..), CField (..), CStruct (..))

#include <ghostty/vt.h>

#define FIELD_SIZE(t, f) sizeof(((t *)0)->f)

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
unknownResult (GhosttyResult n)
  | toInteger n `elem` map snd ghosttyResultEnum.values = Nothing
  | otherwise = Just n

-- | Every value of @GhosttyResult@ this binding names, for the layout test
-- (§3.4). It is also what 'GhosttyResultUnknown' excludes, so a pattern above
-- that is missing here would be seen as unknown, which the layout test
-- catches.
ghosttyResultEnum :: CEnum
ghosttyResultEnum =
  CEnum
    { name = "GhosttyResult",
      size = (#size GhosttyResult),
      values =
        [ ("GHOSTTY_SUCCESS", (#const GHOSTTY_SUCCESS)),
          ("GHOSTTY_OUT_OF_MEMORY", (#const GHOSTTY_OUT_OF_MEMORY)),
          ("GHOSTTY_INVALID_VALUE", (#const GHOSTTY_INVALID_VALUE)),
          ("GHOSTTY_OUT_OF_SPACE", (#const GHOSTTY_OUT_OF_SPACE)),
          ("GHOSTTY_NO_VALUE", (#const GHOSTTY_NO_VALUE)),
          ("GHOSTTY_IO_ERROR", (#const GHOSTTY_IO_ERROR)),
          ("GHOSTTY_LIMIT_EXCEEDED", (#const GHOSTTY_LIMIT_EXCEEDED)),
          ("GHOSTTY_REJECTED", (#const GHOSTTY_REJECTED))
        ]
    }

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
  sizeOf _ = (#size GhosttyString)
  alignment _ = (#alignment GhosttyString)
  peek p =
    GhosttyString
      <$> (#peek GhosttyString, ptr) p
      <*> (#peek GhosttyString, len) p
  poke p s = do
    (#poke GhosttyString, ptr) p s.ptr
    (#poke GhosttyString, len) p s.len

-- | Copy a borrowed string's bytes out, while the pointer is still valid. A
-- null pointer is the empty string, whatever its length says. The one
-- marshalling helper this package has (DESIGN.md §3.3): every reader of a
-- 'GhosttyString' goes through it.
ghosttyStringBytes :: GhosttyString -> IO ByteString
ghosttyStringBytes s
  | s.ptr == nullPtr = pure BS.empty
  | otherwise = BS.packCStringLen (castPtr s.ptr, fromIntegral s.len)

-- | The layout of @GhosttyString@ as compiled, for the layout test (§3.4).
ghosttyStringLayout :: CStruct
ghosttyStringLayout =
  CStruct
    { name = "GhosttyString",
      size = (#size GhosttyString),
      align = (#alignment GhosttyString),
      fields =
        [ CField "ptr" (#offset GhosttyString, ptr) (#const FIELD_SIZE(GhosttyString, ptr)),
          CField "len" (#offset GhosttyString, len) (#const FIELD_SIZE(GhosttyString, len))
        ]
    }

-- | @ghostty_type_json@: the linked library's description of every public
-- type (sizes, alignments, field offsets, enum values), as a static
-- NUL-terminated JSON string valid for the life of the process. What the
-- layout test checks the binding against (§3.4). @unsafe@: it does not call
-- back into Haskell.
foreign import capi unsafe "ghostty/vt.h ghostty_type_json"
  ghosttyTypeJson :: IO (ConstPtr CChar)
