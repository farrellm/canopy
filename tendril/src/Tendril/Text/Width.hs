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
    peakWidth,
    pieces,
  )
where

import Data.Text qualified as T
import Tendril.Text.Grapheme (clusters)
import Tendril.Text.Unicode.Tables (emojiVariationBase, width, zeroWidthInCluster)
import Tendril.Text.Units (Offset, Unit (..), fromOffset, offset)

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
textWidth mode = offset . sum . measure
  where
    measure = case mode of
      Clusters -> map (fromOffset . clusterWidth) . clusters
      CodePoints -> map (fromIntegral . width) . toString

-- | The cells one grapheme cluster takes in grapheme-cluster mode:
-- libghostty-vt's @graphemeWidth@, on a text that is one cluster.
--
-- >>> map (fromOffset . clusterWidth) ["e\x0301", "\x1F44D\x1F3FD", "\x2764", "\x2764\xFE0F", "a\xFE0F"]
-- [1,2,1,2,1]
clusterWidth :: Text -> Offset 'Column
clusterWidth t = offset . fromIntegral $ case T.uncons t of
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

-- | The most cells a cluster takes in grapheme-cluster mode while the host
-- reads it, one code point at a time: a VS15 can make a cluster narrow after
-- its first code points made it wide, and the host places it by the width it
-- had then.
--
-- >>> map (fromOffset . peakWidth) ["\x231A\xFE0E", "\x0600\x231A\xFE0E", "e\x0301"]
-- [2,2,1]
peakWidth :: Text -> Offset 'Column
peakWidth t = foldl' max (offset 0) [clusterWidth (T.take n t) | n <- [1 .. T.length t]]

-- | The pieces the host draws a text in, each in cells of its own, with
-- their widths, in order. In grapheme-cluster mode they are its clusters,
-- except that a code point below U+0100 always starts a piece: libghostty-vt
-- 1.0 prints those without asking whether they join the cell before, so an
-- Arabic number sign (a @Prepend@) and the @x@ after it, one cluster, are two
-- cells; and the zero-width code points a cluster starts with are a piece
-- of their own, of width 0, since the host has no cell to put them in, and
-- reads what follows them afresh. Otherwise each code point that takes a cell is a piece, with the
-- zero-width ones after it, which the host puts in the same cell. A piece of
-- width 0 is a run of zero-width code points the host has no cell for (a
-- combining mark at the start). Concatenated, the pieces are the text.
--
-- >>> map (fmap fromOffset) (pieces Clusters "e\x0301\x1F469\x200D\x1F4BB\x0600x\x11A8\x0903")
-- [("e\769",1),("\128105\8205\128187",2),("\1536",1),("x",1),("\4520",0),("\2307",1)]
-- >>> map (fmap fromOffset) (pieces CodePoints "\x0301\&e\x0301\x1F469\x200D\x1F4BB")
-- [("\769",0),("e\769",1),("\128105\8205",2),("\128187",2)]
pieces :: WidthMode -> Text -> [(Text, Offset 'Column)]
pieces mode t = case mode of
  Clusters -> concatMap (zeroApart . (\c -> (c, clusterWidth c))) (concatMap latin1Apart (clusters t))
  CodePoints -> go t
  where
    zeroApart (c, w) =
      let (zeros, rest) = T.span ((== 0) . width) c
       in if T.null zeros || T.null rest then [(c, if T.null rest then offset 0 else w)] else [(zeros, offset 0), (rest, clusterWidth rest)]
    latin1Apart c = case T.uncons c of
      Nothing -> []
      Just (c0, rest) ->
        let (same, more) = T.break (< '\x100') rest
         in T.cons c0 same : latin1Apart more
    go rest = case T.uncons rest of
      Nothing -> []
      Just (c, after) ->
        -- A code point of width 0 starts a piece only at the start.
        let (zeros, rest') = T.span ((== 0) . width) after
         in (T.cons c zeros, offset (fromIntegral (width c))) : go rest'
