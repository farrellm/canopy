{-# LANGUAGE StrictData #-}

-- | Extended grapheme clusters as libghostty-vt segments them (DESIGN.md
-- §5.3): UAX #29's rules at the tables' Unicode version, which pass the UCD's
-- @GraphemeBreakTest.txt@, with libghostty-vt's two tailorings. The terminal
-- decides what it draws, so tendril segments as it does. The conformance
-- test expects the first tailoring's breaks, as uucode's own does, and no
-- case has a variation selector.
--
-- __Emoji modifiers.__ UAX #29 makes an emoji modifier (a skin tone) @Extend@,
-- so it continues any cluster. libghostty-vt's uucode continues one with it
-- only straight after an @Emoji_Modifier_Base@ (or a prepend, by GB9b), as
-- UTS #51's ED-13 defines a modifier sequence, and otherwise starts a new
-- cluster: @q@ and a skin tone are two clusters, one cell and two.
--
-- __Stray variation selectors.__ A VS15 or VS16 that does not follow an
-- emoji variation base is part of the cluster it follows, but the rules do
-- not see it: libghostty-vt drops it, and reads the next code point against
-- the one before it. So a Hangul LVT syllable, a VS16 and a T jamo are one
-- cluster, where UAX #29 alone would break before the jamo.
--
-- At 18.0.0, GB9c no longer needs a consonant before the linker: it is
-- @\\p{InCB=Linker} [\\p{InCB=Extend}\\p{InCB=Linker}]* × \\p{InCB=Consonant}@,
-- as the conformance cases show (a virama at the start of a text joins the
-- consonant after it).
--
-- A 'Segmenter' reads code points one at a time and says before each whether
-- a cluster starts there; 'clusters' and 'boundaries' run one over a text.
module Tendril.Text.Grapheme
  ( -- * Segmenting text
    clusters,
    boundaries,

    -- * Incrementally
    Segmenter,
    segmenter,
    breakBefore,
  )
where

import Data.Text qualified as T
import Tendril.Text.Unicode.Tables
  ( GraphemeBreak (..),
    IndicConjunctBreak (..),
    emojiModifier,
    emojiModifierBase,
    emojiVariationBase,
    extendedPictographic,
    graphemeBreak,
    indicConjunctBreak,
  )
import Tendril.Text.Units (Offset, Unit (..), offset)

-- | What the rules need to know of the code points read so far.
data Segmenter = Segmenter
  { -- | The last code point's break property; 'Nothing' at the start.
    previous :: Maybe GraphemeBreak,
    -- | The last code point is an @Emoji_Modifier_Base@, which a modifier
    -- may continue (the tailoring).
    afterModifierBase :: Bool,
    -- | The last code point is an emoji variation base, which a variation
    -- selector may follow (the second tailoring).
    afterVariationBase :: Bool,
    -- | The run of regional indicators the last code point ends has an odd
    -- length (GB12, GB13).
    oddIndicators :: Bool,
    -- | Where the text stands in GB11's @ExtPict Extend* ZWJ@.
    pictographic :: Pictographic,
    -- | The text ends in GB9c's @Linker [Extend Linker]*@, so a consonant
    -- next continues the cluster.
    linked :: Bool
  }
  deriving stock (Eq, Show)

-- | GB11's prefix: @ExtPict Extend*@, then the @ZWJ@ after it.
data Pictographic = NoPictographic | Pictographic | PictographicZwj
  deriving stock (Eq, Show)

-- | The state at the start of a text.
segmenter :: Segmenter
segmenter = Segmenter Nothing False False False NoPictographic False

-- | Whether a cluster starts at this code point, after what the segmenter
-- has read, and the segmenter with it read. One always starts at the first.
--
-- >>> fst (breakBefore '\x0301' (snd (breakBefore 'e' segmenter)))
-- False
breakBefore :: Char -> Segmenter -> (Bool, Segmenter)
breakBefore c s
  | not breaks && (c == '\xFE0E' || c == '\xFE0F') && not s.afterVariationBase = (False, s)
  | otherwise = (breaks, s')
  where
    gb = graphemeBreak c
    incb = indicConjunctBreak c
    pict = extendedPictographic c
    breaks = case s.previous of
      Nothing -> True -- GB1
      Just p -> rules p
    rules p
      | p == CR && gb == LF = False -- GB3
      | p `elem` [CR, LF, Control] = True -- GB4
      | gb `elem` [CR, LF, Control] = True -- GB5
      | p == HangulL && gb `elem` [HangulL, HangulV, HangulLV, HangulLVT] = False -- GB6
      | p `elem` [HangulLV, HangulV] && gb `elem` [HangulV, HangulT] = False -- GB7
      | p `elem` [HangulLVT, HangulT] && gb == HangulT = False -- GB8
      | emojiModifier c && not s.afterModifierBase && p /= Prepend = True -- the tailoring
      | gb `elem` [Extend, ZWJ] = False -- GB9
      | gb == SpacingMark = False -- GB9a
      | p == Prepend = False -- GB9b
      | s.linked && incb == IncbConsonant = False -- GB9c
      | s.pictographic == PictographicZwj && pict = False -- GB11
      | p == RegionalIndicator && gb == RegionalIndicator && s.oddIndicators = False -- GB12, GB13
      | otherwise = True -- GB999
    s' =
      Segmenter
        { previous = Just gb,
          afterModifierBase = emojiModifierBase c,
          afterVariationBase = emojiVariationBase c,
          oddIndicators =
            gb == RegionalIndicator
              && not (s.previous == Just RegionalIndicator && s.oddIndicators),
          -- A break ends the prefix: under UAX #29 alone only a control
          -- breaks before an Extend or ZWJ, and it has ended it already, but
          -- the tailoring breaks before a modifier, which is Extend.
          pictographic = case (pict, breaks, s.pictographic, gb) of
            (True, _, _, _) -> Pictographic
            (_, False, Pictographic, Extend) -> Pictographic
            (_, False, Pictographic, ZWJ) -> PictographicZwj
            _ -> NoPictographic,
          -- uucode reads a modifier as no @InCB=Extend@, though the UCD
          -- makes it one, so it ends GB9c's prefix there too.
          linked = case incb of
            IncbLinker -> True
            IncbExtend | not (emojiModifier c) -> s.linked
            _ -> False
        }

-- | A text's grapheme clusters, in order.
--
-- >>> clusters "e\x0301\&a\x1F1FA\x1F1F8\r\n"
-- ["e\769","a","\127482\127480","\r\n"]
clusters :: Text -> [Text]
clusters t = go t (lengths t)
  where
    go rest = \case
      [] -> []
      n : ns -> let (c, rest') = T.splitAt n rest in c : go rest' ns

-- | Every cluster boundary in a text, as a code point offset: the start, the
-- end, and each place between two clusters, in order. An empty text has one.
--
-- >>> boundaries "e\x0301\&a"
-- [Offset 0,Offset 2,Offset 3]
boundaries :: Text -> [Offset 'CodePoint]
boundaries t = offset 0 : snd (mapAccumL step 0 (lengths t))
  where
    step acc n = let acc' = acc + fromIntegral n in (acc', offset acc')

-- | Each cluster's length in code points, produced lazily: a consumer that
-- stops early (a conversion near the start of a buffer) segments only the
-- prefix it reads.
lengths :: Text -> [Int]
lengths = go segmenter 0
  where
    go !s !n rest = case T.uncons rest of
      Nothing -> [n | n > 0]
      Just (c, rest') -> case breakBefore c s of
        (True, s') | n > 0 -> n : go s' 1 rest'
        (_, s') -> go s' (n + 1) rest'
