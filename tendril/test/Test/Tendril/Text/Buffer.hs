{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The buffer's unit conversions (DESIGN.md §5.3): each one round-trips
-- where it is defined, and is undefined past the end and inside a unit.
module Test.Tendril.Text.Buffer (tests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Tendril.Text.Buffer (Buffer, BufferUnit (..), convert, fromText, splitAtOffset)
import Tendril.Text.Grapheme (boundaries, clusters)
import Tendril.Text.Units (Offset, Unit (..), fromOffset, offset)
import Test.QuickCheck (Property, conjoin, counterexample, forAll, property, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Tendril.Text.Gen (genText)

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
                      fromIntegral (lengthUtf8 t),
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
                ]
    ]

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

lengthUtf8 :: Text -> Int
lengthUtf8 = BS.length . TE.encodeUtf8
