{-# LANGUAGE StrictData #-}

-- | The cursor and the selection (DESIGN.md §5.4): Emacs's point and mark.
-- A selection is active while it has an anchor; it is the text between the
-- anchor and the cursor, and moving the cursor keeps the anchor where it is.
--
-- The editor keeps both ends on cluster boundaries ("Tendril.Edit.Command"),
-- as code point offsets, since a boundary near a code point offset is found
-- by reading the text near it, where a grapheme offset would be counted from
-- the start of the buffer (D34).
module Tendril.Edit.Selection
  ( Selection (..),
    collapsed,
    region,
  )
where

import Tendril.Text.Units (Offset, Range, Unit (..), range)

-- $setup
-- >>> import Tendril.Text.Units (offset)

-- | Where the cursor is, and the anchor if a selection is active.
data Selection = Selection
  { -- | Where text is inserted, and the end of the selection that moves.
    cursor :: Offset 'CodePoint,
    -- | The end of the selection that stays, if one is active.
    anchor :: Maybe (Offset 'CodePoint)
  }
  deriving stock (Eq, Show)

-- | A cursor with no selection.
collapsed :: Offset 'CodePoint -> Selection
collapsed x = Selection x Nothing

-- | The selected text's range, if a selection is active.
--
-- >>> region (Selection (offset 1) (Just (offset 4)))
-- Just (Range (Offset 1) (Offset 4))
region :: Selection -> Maybe (Range 'CodePoint)
region s = range s.cursor <$> s.anchor
