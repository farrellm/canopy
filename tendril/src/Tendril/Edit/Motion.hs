{-# LANGUAGE StrictData #-}

-- | Cursor motions (DESIGN.md §5.4): where each takes the cursor in a
-- buffer. A motion moves between cluster boundaries, and reads only the
-- text it moves over and the line it is on (D34).
module Tendril.Edit.Motion
  ( Motion (..),
    Target (..),
    target,
  )
where

import Data.Char (isAlphaNum)
import Data.Text qualified as T
import Tendril.Text.Buffer
  ( Buffer,
    clusterEnd,
    clustersBefore,
    clustersFrom,
    extent,
    floorBoundary,
    lineEnd,
    lineStart,
  )
import Tendril.Text.Units (Offset, Unit (..), fromOffset, offset)

-- | A place to move the cursor to, from where it is.
data Motion
  = -- | One cluster back.
    PrevCluster
  | -- | One cluster on.
    NextCluster
  | -- | Back to the start of a word: over any clusters that are not a word's,
    -- then over a word's. A word's cluster starts with a letter or a digit.
    PrevWord
  | -- | On to the end of a word, as 'PrevWord' goes back.
    NextWord
  | -- | The start of the cursor's line.
    LineStart
  | -- | The end of the cursor's line, before its line feed.
    LineEnd
  | -- | The line above, as many clusters into it as the goal column, or its
    -- end if it is shorter. On the first line, nowhere.
    LineUp
  | -- | The line below, as 'LineUp' goes above. On the last line, nowhere.
    LineDown
  | -- | The start of the buffer.
    BufferStart
  | -- | The end of the buffer.
    BufferEnd
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Where a motion lands, and the goal column it leaves.
data Target = Target
  { -- | The cursor's new place, a cluster boundary.
    position :: Offset 'CodePoint,
    -- | For 'LineUp' and 'LineDown', the column they aimed at, in clusters
    -- from the start of a line, which the next vertical motion aims at too:
    -- so a run of them passes a short line and comes back to the column it
    -- started from. 'Nothing' after any other motion.
    goal :: Maybe Word
  }
  deriving stock (Eq, Show)

-- | Where a motion takes a cursor at a cluster boundary, given the goal
-- column the last motion left.
--
-- >>> (target "ab cd" Nothing (offset 5) PrevWord).position
-- Offset 3
-- >>> (target "abc\nd\nefg" Nothing (offset 2) LineDown).position
-- Offset 5
-- >>> (target "abc\nd\nefg" (Just 2) (offset 5) LineDown).position
-- Offset 8
target :: Buffer -> Maybe Word -> Offset 'CodePoint -> Motion -> Target
target b goal x = \case
  PrevCluster -> Target (maybe x fst (listToMaybe (clustersBefore b x))) Nothing
  NextCluster -> Target (maybe x clusterEnd (listToMaybe (clustersFrom b x))) Nothing
  PrevWord ->
    let rest = dropWhile (not . isWord) (clustersBefore b x)
     in Target (maybe (offset 0) fst (viaNonEmpty last (takeWhile isWord rest))) Nothing
  NextWord ->
    let rest = dropWhile isWord (dropWhile (not . isWord) (clustersFrom b x))
     in Target (maybe (extent b) fst (listToMaybe rest)) Nothing
  LineStart -> Target (lineStart b x) Nothing
  LineEnd -> Target (floorBoundary b (lineEnd b x)) Nothing
  LineUp
    | start == offset 0 -> Target x (Just column)
    | otherwise -> Target (atColumn (lineStart b (before start))) (Just column)
  LineDown
    | here == extent b -> Target x (Just column)
    | otherwise -> Target (atColumn (after here)) (Just column)
  BufferStart -> Target (offset 0) Nothing
  BufferEnd -> Target (extent b) Nothing
  where
    start = lineStart b x
    here = lineEnd b x
    column = fromMaybe (fromIntegral (length (takeWhile ((< x) . fst) (clustersFrom b start)))) goal
    -- The cluster @column@ clusters into the line that starts here, or the
    -- line's end.
    atColumn s =
      let e = floorBoundary b (lineEnd b s)
          starts = takeWhile (< e) (map fst (clustersFrom b s))
       in fromMaybe e (starts !!? fromIntegral column)
    before o = offset (fromOffset o - 1)
    after o = offset (fromOffset o + 1)

-- | Whether a cluster is part of a word: it starts with a letter or a digit.
isWord :: (Offset 'CodePoint, Text) -> Bool
isWord = maybe False (isAlphaNum . fst) . T.uncons . snd
