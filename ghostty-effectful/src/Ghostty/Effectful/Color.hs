-- | Colours as a terminal holds them (DESIGN.md §4.4): an RGB triple, and the
-- 256-colour palette.
module Ghostty.Effectful.Color
  ( -- * Colours
    Rgb (..),

    -- * Palettes
    Palette,
    paletteFromList,
    paletteColors,
    paletteColor,
    setPaletteColor,
  )
where

import Data.Array (Array, elems, listArray, (!), (//))

-- | A colour, eight bits a channel.
data Rgb = Rgb
  { -- | red
    red :: Word8,
    -- | green
    green :: Word8,
    -- | blue
    blue :: Word8
  }
  deriving stock (Eq, Ord, Show)

-- | The 256-colour palette: a colour for every index, so looking one up
-- cannot fail.
newtype Palette = Palette (Array Word8 Rgb)
  deriving stock (Eq, Show)

-- | A palette of exactly 256 colours, index 0 first; 'Nothing' for any other
-- count.
--
-- >>> isJust (paletteFromList (replicate 256 (Rgb 0 0 0)))
-- True
-- >>> isJust (paletteFromList [Rgb 0 0 0])
-- False
paletteFromList :: [Rgb] -> Maybe Palette
paletteFromList cs
  | length cs == 256 = Just (Palette (listArray (minBound, maxBound) cs))
  | otherwise = Nothing

-- | The 256 colours, index 0 first.
paletteColors :: Palette -> [Rgb]
paletteColors (Palette a) = elems a

-- | The colour at an index. Total: the palette has a colour at every 'Word8'.
paletteColor :: Palette -> Word8 -> Rgb
paletteColor (Palette a) i = a ! i

-- | The palette with one index's colour replaced.
setPaletteColor :: Word8 -> Rgb -> Palette -> Palette
setPaletteColor i c (Palette a) = Palette (a // [(i, c)])
