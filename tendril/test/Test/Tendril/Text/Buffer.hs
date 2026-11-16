{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The buffer's unit conversions (DESIGN.md §5.3): each one round-trips
-- where it is defined, and is undefined past the end and inside a unit.
module Test.Tendril.Text.Buffer (tests) where

import Data.Text qualified as T
import Data.Text.Foreign (lengthWord8)
import Tendril.Text.Buffer
  ( Buffer,
    BufferUnit (..),
    ceilingBoundary,
    clustersBefore,
    clustersFrom,
    convert,
    floorBoundary,
    fromText,
    isBoundary,
    lineEnd,
    lineStart,
    replace,
    slice,
    splitAtOffset,
  )
import Tendril.Text.Grapheme (boundaries, clusters)
import Tendril.Text.Units (Offset, Unit (..), fromOffset, offset, range)
import Test.QuickCheck (Gen, Property, choose, conjoin, counterexample, forAll, property, vectorOf, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Tendril.Text.Gen (genRunText, genText)

tests :: TestTree
tests =
  testGroup
    "Tendril.Text.Buffer"
    [ testCase "an astral letter, by unit" $ do
        let b = fromText "a\x10400\&b"
        (fromOffset (extent @'Byte b), fromOffset (extent @'Utf16 b), fromOffset (extent @'CodePoint b))
          @?= (6, 4, 3)
        map (convert @'Byte @'CodePoint b . offset) [0 .. 7] @?= map (fmap offset) [Just 0, Just 1, Nothing, Nothing, Nothing, Just 2, Just 3, Nothing]
        map (convert @'Utf16 @'CodePoint b . offset) [0 .. 5] @?= map (fmap offset) [Just 0, Just 1, Nothing, Just 2, Just 3, Nothing],
      testProperty "lengths agree with Data.Text's" $
        forAll genText $ \t ->
          let b = fromText t
           in ( fromOffset (extent @'CodePoint b),
                fromOffset (extent @'Byte b),
                fromOffset (extent @'Grapheme b),
                toText b
              )
                === ( fromIntegral (T.length t),
                      fromIntegral (lengthWord8 t),
                      fromIntegral (length (clusters t)),
                      t
                    ),
      testProperty "each unit's extent converts to each other's" $
        forAll genText $ \t ->
          let b = fromText t
           in conjoin
                [ convert @'Grapheme @'Byte b (extent b) === Just (extent b),
                  convert @'Byte @'Utf16 b (extent b) === Just (extent b),
                  convert @'Utf16 @'Grapheme b (extent b) === Just (extent b)
                ],
      testProperty "Grapheme round-trips through each unit" $
        forAll genText (roundTrips @'Grapheme . fromText),
      testProperty "CodePoint round-trips through each unit" $
        forAll genText (roundTrips @'CodePoint . fromText),
      testProperty "Byte round-trips through each unit" $
        forAll genText (roundTrips @'Byte . fromText),
      testProperty "Utf16 round-trips through each unit" $
        forAll genText (roundTrips @'Utf16 . fromText),
      -- So that the round trips above are not vacuous: a cluster or code point
      -- boundary is one in every finer unit.
      testProperty "every grapheme and code point offset converts to each finer unit" $
        forAll genText $ \t ->
          let b = fromText t
              finer :: forall a. (BufferUnit a) => [Offset a] -> Bool
              finer = all $ \o ->
                isJust (convert @a @'CodePoint b o)
                  && isJust (convert @a @'Byte b o)
                  && isJust (convert @a @'Utf16 b o)
           in (finer (map (offset @'Grapheme) [0 .. fromOffset (extent @'Grapheme b)]), finer (map (offset @'CodePoint) [0 .. fromOffset (extent @'CodePoint b)]))
                === (True, True),
      testProperty "a code point offset is a grapheme one exactly at a boundary" $
        forAll genText $ \t ->
          let b = fromText t
              cs = map (offset @'CodePoint) [0 .. fromIntegral (T.length t)]
           in map (isJust . convert @'CodePoint @'Grapheme b) cs === map (`elem` boundaries t) cs,
      testProperty "splitting at a grapheme gives the clusters before it" $
        forAll genText $ \t ->
          let b = fromText t
           in conjoin
                [ fmap (bimap toText toText) (splitAtOffset (offset @'Grapheme (fromIntegral g)) b)
                    === Just (T.concat (take g (clusters t)), T.concat (drop g (clusters t)))
                | g <- [0 .. length (clusters t)]
                ],
      -- D34: the boundaries near an offset, found by segmenting from a
      -- place nearby, are the ones segmenting the whole text finds.
      testCase "a run longer than the first window" $ do
        let b = fromText ("a" <> T.replicate 300 "\x0301" <> "b")
        take 2 (clustersBefore b (extent b)) @?= [(offset 301, "b"), (offset 0, "a" <> T.replicate 300 "\x0301")]
        take 1 (clustersFrom b (offset 150)) @?= [(offset 301, "b")]
        floorBoundary b (offset 150) @?= offset 0,
      -- Regression 23 (D34): inside a cluster of code points that each
      -- resume (a run of Hangul L), clustersBefore stepped back one code
      -- point at a time and segmented to the end again for each: quadratic.
      testCase "regression 23: a long cluster of resuming code points" $ do
        let run = T.replicate 20000 "\x1100"
            b = fromText ("a" <> run)
        take 2 (clustersBefore b (extent b)) @?= [(offset 1, run), (offset 0, "a")],
      testProperty "clustersFrom is whole-text segmentation from there on" $
        nearby $
          \t x -> clustersFrom (fromText t) x === filter ((>= x) . fst) (segmented t),
      testProperty "clustersBefore is whole-text segmentation up to there, backwards" $
        nearby $
          \t x -> clustersBefore (fromText t) x === reverse (filter ((<= x) . end) (segmented t)),
      testProperty "isBoundary, floorBoundary and ceilingBoundary agree with the boundaries" $
        nearby $ \t x ->
          let b = fromText t
              bs = boundaries t
              x' = min x (extent b)
           in (isBoundary b x, floorBoundary b x, ceilingBoundary b x)
                === ( x `elem` bs,
                      fromMaybe (offset 0) (viaNonEmpty last (filter (<= x') bs)),
                      fromMaybe (extent b) (find (>= x') bs)
                    ),
      testProperty "lineStart and lineEnd are the line feeds around an offset" $
        nearby $ \t x ->
          let n = fromOffset x
              (before, after) = T.splitAt (fromIntegral n) t
           in (lineStart (fromText t) x, lineEnd (fromText t) x)
                === ( offset (fromIntegral (T.length before - T.length (T.takeWhileEnd (/= '\n') before))),
                      offset (fromIntegral (T.length before + T.length (T.takeWhile (/= '\n') after)))
                    ),
      testProperty "replace and slice are Data.Text's" $
        forAll genText $ \t ->
          forAll genText $ \u ->
            forAll (positions t) $ \(x, y) ->
              let r = range x y
                  at o = fromIntegral (fromOffset o)
                  (before, rest) = T.splitAt (at r.start) t
                  (mid, after) = T.splitAt (at r.end - at r.start) rest
               in (toText (replace r u (fromText t)), slice r (fromText t)) === (before <> u <> after, mid)
    ]

-- | A text and offsets into it, one past the end among them.
nearby :: (Text -> Offset 'CodePoint -> Property) -> Property
nearby p = forAll genRunText $ \t ->
  forAll (vectorOf 8 (choose (0, T.length t + 1))) $ \xs ->
    conjoin [counterexample (show x) (p t (offset (fromIntegral x))) | x <- xs]

-- | Two offsets into a text, in either order.
positions :: Text -> Gen (Offset 'CodePoint, Offset 'CodePoint)
positions t = do
  let o = offset . fromIntegral
  x <- choose (0, T.length t)
  y <- choose (0, T.length t)
  pure (o x, o y)

-- | Each cluster of a whole text, with its start.
segmented :: Text -> [(Offset 'CodePoint, Text)]
segmented t = zip (boundaries t) (clusters t)

end :: (Offset 'CodePoint, Text) -> Offset 'CodePoint
end (o, c) = offset (fromOffset o + fromIntegral (T.length c))

-- | Every offset of unit @a@ from 0 to one past the end: each that converts
-- to another unit converts back to itself, and the one past the end converts
-- to nothing.
roundTrips :: forall a. (BufferUnit a) => Buffer -> Property
roundTrips b =
  conjoin
    [ counterexample (show o) $
        conjoin
          [ back (convert @a @'Byte b o) o,
            back (convert @a @'Utf16 b o) o,
            back (convert @a @'CodePoint b o) o,
            back (convert @a @'Grapheme b o) o
          ]
    | n <- [0 .. fromOffset (extent @a b)],
      let o = offset @a n
    ]
    .&&. convert @a @'CodePoint b (offset (fromOffset (extent @a b) + 1)) === Nothing
  where
    back :: forall u. (BufferUnit u) => Maybe (Offset u) -> Offset a -> Property
    back converted o = case converted of
      Nothing -> property True
      Just x -> convert @u @a b x === Just o
