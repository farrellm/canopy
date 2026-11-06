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
  )
where

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

-- | A C enum as the binding sees it: the values it names, not necessarily all
-- the header has. C enums are open (§3.3), so naming a subset is sound.
data CEnum = CEnum
  { -- | The C type name, as @ghostty_type_json()@ keys it.
    name :: Text,
    -- | @sizeof@ of the enum type, in bytes.
    size :: Int,
    -- | Each named value: its full C name (@GHOSTTY_SUCCESS@) and its value.
    values :: [(Text, Integer)]
  }
  deriving stock (Eq, Show)
