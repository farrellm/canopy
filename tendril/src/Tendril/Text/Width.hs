{-# LANGUAGE StrictData #-}

-- | Display width, as libghostty-vt decides it (DESIGN.md §5.3): width is the
-- terminal's decision, not tendril's.
--
-- A terminal in grapheme-cluster mode (2027) gives each cluster the width of
-- its first code point, made two cells by any later code point that is not
-- 'zeroWidthInCluster', and chosen by a VS15 or VS16 after an emoji
-- variation base. Without the mode, each code point takes its own width.
-- @Tendril.Term.Caps@ probes the host for the mode (§5.7).
module Tendril.Text.Width
  ( WidthMode (..),
    textWidth,
    clusterWidth,
  )
where

import Data.Text qualified as T
import Tendril.Text.Grapheme (clusters)
import Tendril.Text.Unicode.Tables (emojiVariationBase, width, zeroWidthInCluster)
import Tendril.Text.Units (Offset, Unit (..), offset)

-- | How the host measures text.
data WidthMode
  = -- | Grapheme-cluster mode (2027) is on: a cluster is measured whole.
    Clusters
  | -- | It is off: each code point is measured alone, and the widths add.
    CodePoints
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The cells a text takes on one row.
--
-- >>> textWidth Clusters "a\x2764\xFE0F"
-- Offset 3
-- >>> textWidth CodePoints "a\x2764\xFE0F"
-- Offset 2
textWidth :: WidthMode -> Text -> Offset 'Column
textWidth mode = offset . fromIntegral . sum . measure
  where
    measure = case mode of
      Clusters -> map clusterWidth . clusters
      CodePoints -> map width . toString

-- | The cells one grapheme cluster takes in grapheme-cluster mode:
-- libghostty-vt's @graphemeWidth@, on a text that is one cluster.
--
-- >>> map clusterWidth ["e\x0301", "\x1F44D\x1F3FD", "\x2764", "\x2764\xFE0F", "a\xFE0F"]
-- [1,2,1,2,1]
clusterWidth :: Text -> Int
clusterWidth t = case T.uncons t of
  Nothing -> 0
  Just (c, rest) -> go (width c) c (toString rest)
  where
    go w prev = \case
      [] -> w
      c : cs
        -- A variation selector chooses the width after a base, and is
        -- ignored after anything else, which stays the one it follows.
        | c == '\xFE0F' || c == '\xFE0E' ->
            if emojiVariationBase prev
              then go (if c == '\xFE0F' then 2 else 1) c cs
              else go w prev cs
        | zeroWidthInCluster c -> go w c cs
        | otherwise -> go 2 c cs
