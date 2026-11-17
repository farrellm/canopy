-- | Generators for T0's properties: text weighted toward the code points
-- whose clusters and widths are hard, astral and multi-code-point ones among
-- them.
module Test.Tendril.Text.Gen (genText, genRunText) where

import Data.Text qualified as T
import Test.QuickCheck (Gen, choose, elements, frequency, listOf, oneof, vectorOf)

-- | A text of up to a few dozen code points.
genText :: Gen Text
genText = T.pack <$> listOf genChar

genChar :: Gen Char
genChar =
  frequency
    [ (6, elements interesting),
      (1, elements ['a' .. 'z']),
      (1, choose ('\x0000', '\xD7FF')),
      (1, choose ('\xE000', '\x10FFFF'))
    ]

-- | CR and LF; a combining acute; ZWJ; an emoji, two emoji modifier bases, a
-- skin-tone modifier, a text-default heart and a wide watch with both
-- variation selectors; two regional indicators; Devanagari
-- KA, virama and nukta; Hangul L, V, T, LV and LVT; a Han character; an
-- astral letter; an Arabic prepend; a spacing mark; a Vedic linker that is
-- not Extend.
interesting :: [Char]
interesting =
  "\r\n\x0301\x200D\x1F600\x1F44D\x1F468\x1F3FB\x2764\x231A\xFE0F\xFE0E\x1F1FA\x1F1F8\x0915\x094D\x093C\x1100\x1161\x11A8\xAC00\xAC01\x6F22\x10400\x0600\x0903\x1CF5"

-- | A text with runs of code points after which segmenting cannot resume
-- (combining marks, ZWJ, skin tones, variation selectors, regional
-- indicators), some longer than the window "Tendril.Text.Buffer" first
-- searches for a place to resume from, among line feeds and ordinary text.
genRunText :: Gen Text
genRunText = T.concat <$> listOf (oneof [genText, run, pure "\n", pure "ab "])
  where
    run = do
      n <- frequency [(3, choose (1, 8)), (1, choose (60, 200))]
      T.pack <$> vectorOf n (elements "\x0301\x200D\x1F3FB\xFE0F\x1F1FA\x094D")
