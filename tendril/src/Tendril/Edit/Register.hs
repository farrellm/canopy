{-# LANGUAGE StrictData #-}

-- | Named registers (DESIGN.md §5.4): text stored under a character, as
-- Emacs's @copy-to-register@ and @insert-register@ and vi's @"a@ keep it.
-- Unlike the kill ring, a register holds one text until it is stored again.
module Tendril.Edit.Register
  ( Register (..),
    Registers,
    emptyRegisters,
    store,
    appendTo,
    recall,
  )
where

import Data.Map.Strict qualified as Map

-- | A register's name.
newtype Register = Register Char
  deriving stock (Eq, Ord, Show)

-- | Every register's text.
newtype Registers = Registers (Map Register Text)
  deriving stock (Eq, Show)

-- | No register holds anything.
emptyRegisters :: Registers
emptyRegisters = Registers Map.empty

-- | The registers with one holding a text, in place of what it held.
--
-- >>> recall (Register 'a') (store (Register 'a') "x" emptyRegisters)
-- Just "x"
store :: Register -> Text -> Registers -> Registers
store r t (Registers m) = Registers (Map.insert r t m)

-- | The registers with a text added to the end of what one holds, as vi's
-- upper-case register names do.
--
-- >>> recall (Register 'a') (appendTo (Register 'a') "y" (store (Register 'a') "x" emptyRegisters))
-- Just "xy"
appendTo :: Register -> Text -> Registers -> Registers
appendTo r t (Registers m) = Registers (Map.insertWith (flip (<>)) r t m)

-- | What a register holds, if anything.
recall :: Register -> Registers -> Maybe Text
recall r (Registers m) = Map.lookup r m
