{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StrictData #-}

-- | Positions that carry their unit in their type (DESIGN.md §5.3, §10.1).
--
-- Seven position systems meet in canopy, and confusing two is the commonest
-- bug in editors. An 'Offset' says which one it is in: the cursor is an
-- @'Offset' ''Grapheme'@, tree-sitter talks in @'Offset' ''Byte'@, LSP in
-- @'Offset' ''Utf16'@ and the renderer in @'Offset' ''Column'@. There is no
-- 'Num' instance, so offsets in two units cannot be added, and the
-- constructor is not exported and the unit's role is nominal, so 'coerce'
-- cannot change the unit either (D30). Conversions are functions of the text
-- the offsets are into ("Tendril.Text.Buffer").
module Tendril.Text.Units
  ( Unit (..),
    Offset,
    offset,
    fromOffset,
  )
where

-- | What an 'Offset' counts.
data Unit
  = -- | UTF-8 code units: tree-sitter's and the terminal's.
    Byte
  | -- | UTF-16 code units: LSP's.
    Utf16
  | -- | Unicode code points: what @Data.Text@ counts.
    CodePoint
  | -- | Extended grapheme clusters (UAX #29): what the user sees as one
    -- character, and where the cursor can be.
    Grapheme
  | -- | Terminal cells: the renderer's.
    Column
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A count of 'Unit' @u@ from the start of some text, or a length in them.
newtype Offset (u :: Unit) = Offset Word
  deriving stock (Eq, Ord, Show)

-- The unit is the guarantee, so a coercion may not change it (D30). Without
-- this GHC infers a phantom role, and 'coerce' turns a byte offset into a
-- UTF-16 one.
type role Offset nominal

-- | An offset of @n@ units. The unit is the caller's claim, made once, where a
-- count enters from outside (a parser's byte offset, an LSP position).
--
-- >>> fromOffset (offset @'Grapheme 3)
-- 3
offset :: Word -> Offset u
offset = Offset

-- | The count, for a caller that hands it to something outside, which has
-- its own idea of the unit.
fromOffset :: Offset u -> Word
fromOffset (Offset n) = n
