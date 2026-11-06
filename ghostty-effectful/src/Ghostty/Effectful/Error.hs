{-# LANGUAGE DeriveAnyClass #-}

-- | What can go wrong in a call to libghostty-vt (DESIGN.md §4.7).
--
-- Errors a caller can act on are values in the result of the one operation
-- that can produce them. What no caller can act on is a 'GhosttyError',
-- thrown: running out of memory, using the effect from a thread that does not
-- own it, and the library returning what its header says it cannot. A
-- version mismatch is neither: it is the 'Left' of
-- 'Ghostty.Effectful.runGhostty', before any handle exists.
module Ghostty.Effectful.Error
  ( GhosttyError (..),
    CallSite (..),
    ResultCode (..),
    VersionMismatch (..),
  )
where

-- | The call that failed: the C function's name for a result the library
-- returned, or the operation's for a check the binding made.
newtype CallSite = CallSite Text
  deriving stock (Eq, Show)

-- | A @GhosttyResult@, closed, with the open C enum's other values in
-- 'ResultUnknown' (DESIGN.md §3.3).
data ResultCode
  = ResultSuccess
  | ResultOutOfMemory
  | ResultInvalidValue
  | ResultOutOfSpace
  | ResultNoValue
  | ResultIoError
  | ResultLimitExceeded
  | ResultRejected
  | -- | a value this binding does not name
    ResultUnknown Int
  deriving stock (Eq, Show)

-- | The failures no caller can do anything useful with, thrown as exceptions
-- rather than threaded through every signature.
data GhosttyError
  = -- | The library could not allocate.
    OutOfMemory CallSite
  | -- | An operation ran on a thread other than the one that ran
    -- 'Ghostty.Effectful.runGhostty' (DESIGN.md §4.6). libghostty-vt's
    -- handles are not thread-safe, so this is refused before the C call.
    WrongThread CallSite
  | -- | The library returned what its header says it cannot, for the
    -- arguments the binding's types allow.
    Invariant CallSite ResultCode
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- | The linked libghostty-vt is not the build this binding was generated
-- against (DESIGN.md §3.4).
data VersionMismatch = VersionMismatch
  { -- | the pinned commit, @GHOSTTY_COMMIT@
    pinned :: Text,
    -- | the linked library's build metadata, or 'Nothing' if it would not say
    linked :: Maybe Text
  }
  deriving stock (Eq, Show)
