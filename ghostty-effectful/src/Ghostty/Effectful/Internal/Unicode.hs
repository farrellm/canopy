-- | Widths as the terminal lays text out (DESIGN.md §4.8): the raw calls.
-- The public API is "Ghostty.Effectful.Unicode".
module Ghostty.Effectful.Internal.Unicode
  ( Grapheme (..),
    widthOf,
    graphemesOf,
  )
where

import Data.Text qualified as T
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, sizeOf)
import Ghostty.FFI.Unicode (ghosttyUnicodeCodepointWidth, ghosttyUnicodeGraphemeWidth)

-- | One grapheme cluster, as the terminal segments text with grapheme
-- clustering on (mode 2027), and the cells it takes.
data Grapheme = Grapheme
  { -- | the cluster's characters
    cluster :: Text,
    -- | its width in cells: 0, 1 or 2
    width :: Word8
  }
  deriving stock (Eq, Show)

-- | @ghostty_unicode_codepoint_width@. It is pure in C (it reads a table),
-- and bound without 'IO'.
widthOf :: Char -> Word8
widthOf = ghosttyUnicodeCodepointWidth . fromIntegral . ord

-- | @ghostty_unicode_grapheme_width@, in a loop over the whole text: each
-- call consumes the codepoints of the first cluster of what is left, and at
-- least one while any is.
graphemesOf :: Text -> IO [Grapheme]
graphemesOf text =
  withArrayLen (map (fromIntegral . ord) (T.unpack text)) $ \n cps ->
    alloca $ \wp -> go wp cps n text []
  where
    go :: Ptr Word8 -> Ptr Word32 -> Int -> Text -> [Grapheme] -> IO [Grapheme]
    go wp p left rest acc
      | left <= 0 = pure (reverse acc)
      | otherwise = do
          used <- max 1 . fromIntegral <$> ghosttyUnicodeGraphemeWidth p (fromIntegral left) wp
          w <- peek wp
          let (c, rest') = T.splitAt used rest
          go wp (p `plusPtr` (used * sizeOf (0 :: Word32))) (left - used) rest' (Grapheme {cluster = c, width = w} : acc)
