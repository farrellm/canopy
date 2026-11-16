{-# LANGUAGE StrictData #-}

-- | The editor's text (DESIGN.md §5.3): a @text-rope@ rope, which keeps its
-- length in code points, UTF-8 and UTF-16 code units, and the conversions
-- between those and grapheme clusters.
--
-- Every conversion is total: an offset past the end, or inside a unit of the
-- target (half a code point's bytes, half a surrogate pair, the middle of a
-- cluster), has no counterpart, and converts to 'Nothing'.
--
-- The rope counts code points, bytes and UTF-16 code units, but not
-- clusters, so a conversion to or from ''Grapheme' segments the buffer from
-- its start. The editor keeps its positions as code point offsets on cluster
-- boundaries instead, and finds the boundaries near one with 'clustersFrom'
-- and 'clustersBefore', which segment from a nearby place where segmenting
-- can resume ("Tendril.Text.Grapheme".'Tendril.Text.Grapheme.resume'): their
-- cost is the text they read, not the buffer (D34).
module Tendril.Text.Buffer
  ( Buffer,
    fromText,
    toRope,
    isEmpty,

    -- * Units
    BufferUnit (..),
    convert,
    splitAtOffset,

    -- * Editing
    replace,
    slice,

    -- * Clusters near a position
    clustersFrom,
    clustersBefore,
    clusterEnd,
    isBoundary,
    floorBoundary,
    ceilingBoundary,

    -- * Lines
    lineStart,
    lineEnd,
  )
where

import Data.List (elemIndex)
import Data.Text qualified as T
import Data.Text.Lines (Position (..))
import Data.Text.Mixed.Rope (Rope)
import Data.Text.Mixed.Rope qualified as Rope
import Tendril.Text.Grapheme (Segmenter, boundaries, breakBefore, resume, segmenter)
import Tendril.Text.Units (Offset, Range, Unit (..), fromOffset, offset)

-- $setup
-- >>> import Tendril.Text.Units (range)

-- | A text being edited.
newtype Buffer = Buffer Rope
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, NFData)

-- | Through 'fromText', so a literal is chunked like any other text (D34).
instance IsString Buffer where
  fromString = fromText . fromString

instance ToText Buffer where
  toText (Buffer r) = Rope.toText r

-- | A buffer holding a text.
fromText :: Text -> Buffer
fromText = Buffer . chunked

-- | A rope of a text in chunks of at most 'chunkSize' code points, joined
-- as a balanced tree. The rope splits a chunk by walking it, so one chunk of
-- a whole pasted text would make every split near its end cost the paste's
-- length (D34).
chunked :: Text -> Rope
chunked t
  | T.length (T.take (chunkSize + 1) t) <= chunkSize = Rope.fromText t
  | otherwise = balanced (map Rope.fromText (T.chunksOf chunkSize t))
  where
    balanced = \case
      [] -> mempty
      [r] -> r
      rs -> let (a, b) = splitAt (length rs `div` 2) rs in balanced a <> balanced b

-- | The most code points 'chunked' puts in a chunk.
chunkSize :: Int
chunkSize = 4096

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

-- | The buffer with a range's text replaced. An offset past the end is the
-- end.
--
-- >>> replace (range (offset 1) (offset 3)) "XY" "abcd"
-- "aXYd"
replace :: Range 'CodePoint -> Text -> Buffer -> Buffer
replace r t (Buffer rope) = Buffer (before <> chunked t <> after)
  where
    (before, rest) = Rope.charSplitAt (fromOffset r.start) rope
    (_, after) = Rope.charSplitAt (fromOffset r.end - fromOffset r.start) rest

-- | A range's text. An offset past the end is the end.
--
-- >>> slice (range (offset 1) (offset 3)) "abcd"
-- "bc"
slice :: Range 'CodePoint -> Buffer -> Text
slice r (Buffer rope) = Rope.toText (sliceRope (fromOffset r.start) (fromOffset r.end) rope)

sliceRope :: Word -> Word -> Rope -> Rope
sliceRope a b = fst . Rope.charSplitAt (b - min a b) . snd . Rope.charSplitAt a

-- | The clusters that start at or after an offset, each with its start, in
-- order, produced lazily: reading the first few reads only the text near
-- the offset.
--
-- >>> take 2 (clustersFrom "ae\x0301\&b" (offset 1))
-- [(Offset 1,"e\769"),(Offset 3,"b")]
-- >>> take 1 (clustersFrom "ae\x0301\&b" (offset 2))
-- [(Offset 3,"b")]
clustersFrom :: Buffer -> Offset 'CodePoint -> [(Offset 'CodePoint, Text)]
clustersFrom (Buffer rope) x = dropWhile ((< x) . fst) (segmentFrom r s (charsFrom r rope))
  where
    (r, s) = restart rope (fromOffset x)

-- | The clusters that end at or before an offset, each with its start,
-- nearest first, produced lazily.
--
-- >>> take 2 (clustersBefore "ae\x0301\&b" (offset 3))
-- [(Offset 1,"e\769"),(Offset 0,"a")]
-- >>> take 1 (clustersBefore "ae\x0301\&b" (offset 2))
-- [(Offset 0,"a")]
clustersBefore :: Buffer -> Offset 'CodePoint -> [(Offset 'CodePoint, Text)]
clustersBefore (Buffer rope) x0 = go 1 (min (fromOffset x0) (Rope.charLength rope))
  where
    go k y
      | y == 0 = []
      | otherwise = search (restart rope (y - min y k))
      where
        -- Every cluster from the restart on that ends by @y@, nearest first,
        -- then the ones before the first of them. If there is none, the
        -- restart is inside the cluster that ends by @y@, so look again from
        -- twice as far back: a cluster of many code points that each resume
        -- (a run of Hangul L, of spacing marks) would otherwise be segmented
        -- again for every one of them.
        search (r, s) =
          case filter ((<= offset y) . clusterEnd) (takeWhile ((< y) . fromOffset . fst) (segmentFrom r s (charsFrom r rope))) of
            c : cs -> reverse (c : cs) <> go window (fromOffset (fst c))
            []
              | r == 0 -> []
              | otherwise -> search (restart rope (r - min r (y - r)))
    -- How far back each batch of clusters after the first starts: one
    -- segmentation yields every cluster in it, where restarting at each
    -- cluster would split the rope and segment again for every one. The
    -- first starts as near as it can, since most callers want one cluster.
    window = 32

-- | Whether a cluster starts or ends at an offset: the start and the end of
-- the buffer are boundaries, and an offset past the end is not.
--
-- >>> map (isBoundary "e\x0301\&a" . offset) [0 .. 4]
-- [True,False,True,True,False]
isBoundary :: Buffer -> Offset 'CodePoint -> Bool
isBoundary b x
  | x == offset 0 || x == extent b = True
  | x > extent b = False
  | otherwise = maybe False ((== x) . fst) (listToMaybe (clustersFrom b x))

-- | The boundary at or before an offset: the start of the cluster it is in.
-- An offset past the end is the end.
--
-- >>> map (floorBoundary "e\x0301\&a" . offset) [0 .. 4]
-- [Offset 0,Offset 0,Offset 2,Offset 3,Offset 3]
floorBoundary :: Buffer -> Offset 'CodePoint -> Offset 'CodePoint
floorBoundary b x0
  | isBoundary b x = x
  | otherwise = maybe (offset 0) clusterEnd (listToMaybe (clustersBefore b x))
  where
    -- Most offsets asked about are boundaries already, which 'isBoundary'
    -- confirms more cheaply than finding the cluster before (and at either
    -- end, for nothing).
    x = min x0 (extent b)

-- | Where a cluster from 'clustersFrom' or 'clustersBefore' ends.
--
-- >>> map clusterEnd (clustersFrom "ae\x0301" (offset 0))
-- [Offset 1,Offset 3]
clusterEnd :: (Offset 'CodePoint, Text) -> Offset 'CodePoint
clusterEnd (st, t) = offset (fromOffset st + fromIntegral (T.length t))

-- | The boundary at or after an offset: the end of the cluster it is in. An
-- offset past the end is the end.
--
-- >>> map (ceilingBoundary "e\x0301\&a" . offset) [0 .. 4]
-- [Offset 0,Offset 2,Offset 2,Offset 3,Offset 3]
ceilingBoundary :: Buffer -> Offset 'CodePoint -> Offset 'CodePoint
ceilingBoundary b x0
  | x == extent b = x
  | otherwise = maybe (extent b) fst (listToMaybe (clustersFrom b x))
  where
    x = min x0 (extent b)

-- | Where segmenting can start at or before a code point offset, and the
-- segmenter's state there: just after the nearest code point that resumes
-- it, or the start of the text. The window searched doubles until it finds
-- one, so ordinary text costs a few code points. Each doubling reads only
-- the part of its window that the last one did not.
restart :: Rope -> Word -> (Word, Segmenter)
restart rope x = go x 64
  where
    go hi k = case lastResume (Rope.toText (sliceRope w hi rope)) of
      Just (j, s) -> (w + j + 1, s)
      Nothing
        | w == 0 -> (0, segmenter)
        | otherwise -> go w (2 * k)
      where
        w = x - min x k

-- | The last code point of a text that resumes segmenting, by its index,
-- with the state after it.
lastResume :: Text -> Maybe (Word, Segmenter)
lastResume t = go (fromIntegral (T.length t)) t
  where
    go !n u = case T.unsnoc u of
      Nothing -> Nothing
      Just (u', c) -> case resume c of
        Just s -> Just (n - 1, s)
        Nothing -> go (n - 1) u'

-- | The rope's code points from an offset, read lazily a chunk at a time.
charsFrom :: Word -> Rope -> [Char]
charsFrom r = go . snd . Rope.charSplitAt r
  where
    go rest
      | Rope.null rest = []
      | otherwise = let (a, b) = Rope.charSplitAt 256 rest in toString (Rope.toText a) <> go b

-- | The clusters of the code points from an offset, read from a state that
-- is exact there; the code points before the first boundary end a cluster
-- that started earlier, and are skipped.
segmentFrom :: Word -> Segmenter -> [Char] -> [(Offset 'CodePoint, Text)]
segmentFrom = skip
  where
    skip !p s = \case
      [] -> []
      c : cs -> case breakBefore c s of
        (True, s') -> cluster p s' [c] (p + 1) cs
        (False, s') -> skip (p + 1) s' cs
    cluster start s acc !p = \case
      [] -> [(offset start, T.pack (reverse acc))]
      c : cs -> case breakBefore c s of
        (True, s') -> (offset start, T.pack (reverse acc)) : cluster p s' [c] (p + 1) cs
        (False, s') -> cluster start s' (c : acc) (p + 1) cs

-- | The start of the line an offset is on: just after the line feed before
-- it, or the start of the buffer. An offset past the end is taken as the
-- end.
--
-- >>> map (lineStart "ab\ncd" . offset) [0, 2, 3, 5]
-- [Offset 0,Offset 0,Offset 3,Offset 3]
lineStart :: Buffer -> Offset 'CodePoint -> Offset 'CodePoint
lineStart (Buffer rope) x = offset (Rope.charLength before - (Rope.charLengthAsPosition before).posColumn)
  where
    (before, _) = Rope.charSplitAt (fromOffset x) rope

-- | The end of the line an offset is on: the line feed at or after it, or
-- the end of the buffer. In a line that ends in CR LF, that is inside the
-- cluster; the caller wants 'floorBoundary' of it. An offset past the end is
-- taken as the end.
--
-- >>> map (lineEnd "ab\ncd" . offset) [0, 2, 3, 5]
-- [Offset 2,Offset 2,Offset 5,Offset 5]
lineEnd :: Buffer -> Offset 'CodePoint -> Offset 'CodePoint
lineEnd (Buffer rope) x = offset (x' + n - (if newline then 1 else 0))
  where
    x' = min (fromOffset x) (Rope.charLength rope)
    (line, _) = Rope.splitAtLine 1 (snd (Rope.charSplitAt x' rope))
    n = Rope.charLength line
    newline = n > 0 && Rope.toText (snd (Rope.charSplitAt (n - 1) line)) == "\n"
