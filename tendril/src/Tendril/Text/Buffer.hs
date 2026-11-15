{-# LANGUAGE StrictData #-}

-- | The editor's text (DESIGN.md §5.3): a @text-rope@ rope, which keeps its
-- length in code points, UTF-8 and UTF-16 code units, and the conversions
-- between those and grapheme clusters.
--
-- Every conversion is total: an offset past the end, or inside a unit of the
-- target (half a code point's bytes, half a surrogate pair, the middle of a
-- cluster), has no counterpart, and converts to 'Nothing'.
module Tendril.Text.Buffer
  ( Buffer,
    fromText,
    fromRope,
    toRope,
    isEmpty,

    -- * Units
    BufferUnit (..),
    convert,
    splitAtOffset,
  )
where

import Data.List (elemIndex)
import Data.Text.Mixed.Rope (Rope)
import Data.Text.Mixed.Rope qualified as Rope
import Tendril.Text.Grapheme (boundaries)
import Tendril.Text.Units (Offset, Unit (..), fromOffset, offset)

-- | A text being edited.
newtype Buffer = Buffer Rope
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, IsString)

instance ToText Buffer where
  toText (Buffer r) = Rope.toText r

-- | A buffer holding a text.
fromText :: Text -> Buffer
fromText = Buffer . Rope.fromText

-- | A buffer holding a rope's text.
fromRope :: Rope -> Buffer
fromRope = Buffer

-- | The buffer's rope, for a consumer that reads one (tree-sitter's input,
-- through canopy's adapter).
toRope :: Buffer -> Rope
toRope (Buffer r) = r

-- | Whether the buffer holds no text.
isEmpty :: Buffer -> Bool
isEmpty (Buffer r) = Rope.null r

-- | A unit that offsets into a buffer can be counted in. 'Column' is not
-- one: a column is a place on a row, and depends on the host (see
-- "Tendril.Text.Width").
class BufferUnit (u :: Unit) where
  -- | The buffer's length in this unit.
  extent :: Buffer -> Offset u

  -- | An offset as code points; 'Nothing' past the end or inside a code
  -- point.
  toCodePoint :: Buffer -> Offset u -> Maybe (Offset 'CodePoint)

  -- | A code point offset in this unit; 'Nothing' past the end or inside a
  -- unit.
  fromCodePoint :: Buffer -> Offset 'CodePoint -> Maybe (Offset u)

instance BufferUnit 'CodePoint where
  extent (Buffer r) = offset (Rope.charLength r)
  toCodePoint b c = c <$ guard (c <= extent b)
  fromCodePoint b c = c <$ guard (c <= extent b)

instance BufferUnit 'Byte where
  extent (Buffer r) = offset (Rope.utf8Length r)
  toCodePoint b@(Buffer r) n = do
    guard (n <= extent b)
    (before, _) <- Rope.utf8SplitAt (fromOffset n) r
    pure (offset (Rope.charLength before))
  fromCodePoint b c = offset . Rope.utf8Length . toRope . fst <$> splitCodePoints c b

instance BufferUnit 'Utf16 where
  extent (Buffer r) = offset (Rope.utf16Length r)
  toCodePoint b@(Buffer r) n = do
    guard (n <= extent b)
    (before, _) <- Rope.utf16SplitAt (fromOffset n) r
    pure (offset (Rope.charLength before))
  fromCodePoint b c = offset . Rope.utf16Length . toRope . fst <$> splitCodePoints c b

-- | Graphemes walk the text: the rope keeps no count of them.
instance BufferUnit 'Grapheme where
  extent b = offset (fromIntegral (length (boundaries (toText b))) - 1)
  toCodePoint b g = boundaries (toText b) !!? fromIntegral (fromOffset g)
  fromCodePoint b c = offset . fromIntegral <$> elemIndex c (takeWhile (<= c) (boundaries (toText b)))

-- | An offset in another unit.
--
-- >>> convert @'Grapheme @'Byte "e\x0301\&a" (offset 1)
-- Just (Offset 3)
-- >>> convert @'Byte @'Grapheme "e\x0301\&a" (offset 1)
-- Nothing
convert :: forall a b. (BufferUnit a, BufferUnit b) => Buffer -> Offset a -> Maybe (Offset b)
convert b = toCodePoint b >=> fromCodePoint b

-- | The text before an offset and the text from it.
--
-- >>> splitAtOffset @'Grapheme (offset 1) "e\x0301\&a"
-- Just ("e\769","a")
splitAtOffset :: (BufferUnit u) => Offset u -> Buffer -> Maybe (Buffer, Buffer)
splitAtOffset o b = toCodePoint b o >>= (`splitCodePoints` b)

splitCodePoints :: Offset 'CodePoint -> Buffer -> Maybe (Buffer, Buffer)
splitCodePoints c b@(Buffer r) = do
  guard (c <= extent b)
  let (before, after) = Rope.charSplitAt (fromOffset c) r
  pure (Buffer before, Buffer after)
