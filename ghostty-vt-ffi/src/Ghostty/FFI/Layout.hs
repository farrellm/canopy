{-# LANGUAGE DuplicateRecordFields #-}

-- | Descriptions of the C types this binding marshals, as the binding was
-- compiled: every number comes from @hsc2hs@ in the module that binds the
-- type (DESIGN.md §3.3), and is exported beside it.
--
-- @ghostty-vt-ffi-test@ compares these with @ghostty_type_json()@ from the
-- library actually linked (§3.4). The two come from different places, the
-- headers and the library, so a disagreement is a stale or foreign library
-- that would otherwise corrupt memory without a sound.
module Ghostty.FFI.Layout
  ( CStruct (..),
    CField (..),
    CEnum (..),
    unnamed,
  )
where

import Foreign.C.Types (CInt)

-- | A C struct as the binding sees it.
data CStruct = CStruct
  { -- | The C type name, as @ghostty_type_json()@ keys it.
    name :: Text,
    -- | @sizeof@, in bytes.
    size :: Int,
    -- | @_Alignof@, in bytes.
    align :: Int,
    -- | Every field of the struct, in declaration order.
    fields :: [CField]
  }
  deriving stock (Eq, Show)

-- | One field of a 'CStruct'.
data CField = CField
  { -- | The field's C name.
    name :: Text,
    -- | @offsetof@, in bytes.
    offset :: Int,
    -- | @sizeof@ of the field, in bytes.
    size :: Int
  }
  deriving stock (Eq, Show)

-- | A C enum as the binding sees it: the values it names. The layout test
-- requires every value the linked library has, so the binding is exhaustive at
-- the pin; the @Unknown@ view (§3.3) is still there, because C enums are open.
data CEnum = CEnum
  { -- | The C type name, as @ghostty_type_json()@ keys it.
    name :: Text,
    -- | @sizeof@ of the enum type, in bytes.
    size :: Int,
    -- | Each named value: its full C name (@GHOSTTY_SUCCESS@) and its value.
    values :: [(Text, Integer)]
  }
  deriving stock (Eq, Show)

-- | A value the enum does not name, or 'Nothing' for one it does. Every enum's
-- @Unknown@ view (§3.3) is this over its descriptor, so the view and the
-- descriptor cannot disagree.
unnamed :: CEnum -> CInt -> Maybe CInt
unnamed e n
  | toInteger n `elem` map snd e.values = Nothing
  | otherwise = Just n
