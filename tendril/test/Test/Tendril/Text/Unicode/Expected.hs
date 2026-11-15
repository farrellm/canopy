-- | __Generated__ by @scripts/gen-unicode-tables.py@ with
-- "Tendril.Text.Unicode.Tables": what the tables say, digested, so that the
-- test suite checks what GHC decoded from the table literals, not only their
-- text.
module Test.Tendril.Text.Unicode.Expected (digests) where

-- | Each lookup's 64-bit FNV-1a digest over code points 0 to 10FFFF, one
-- byte per code point: a constructor's index, a width, or 0 or 1.
digests :: [(Text, Word64)]
digests =
  [ ("graphemeBreak", 0x7f466b8603148caa),
    ("indicConjunctBreak", 0x05400450a9b80ca6),
    ("extendedPictographic", 0x5fc4fdef1b6d16a5),
    ("width", 0x0f6ecb8a3f093d8e),
    ("emojiVariationBase", 0x60bd89eaae8d1ce8)
  ]
