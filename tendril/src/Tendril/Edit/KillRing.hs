{-# LANGUAGE StrictData #-}

-- | Emacs's kill ring (DESIGN.md §5.4): text that was killed, newest first,
-- for 'Tendril.Edit.Command.Yank' to put back and
-- 'Tendril.Edit.Command.YankPop' to cycle through. Consecutive kills join
-- into one entry, which the editor arranges with 'appendKill' and
-- 'prependKill'.
module Tendril.Edit.KillRing
  ( KillRing,
    emptyKillRing,
    capacity,
    kill,
    appendKill,
    prependKill,
    current,
    rotate,
    entries,
  )
where

-- | The killed texts, newest first, at most 'capacity' of them.
newtype KillRing = KillRing [Text]
  deriving stock (Eq, Show)

-- | A ring with nothing killed.
emptyKillRing :: KillRing
emptyKillRing = KillRing []

-- | How many entries a ring keeps; a kill beyond it forgets the oldest.
-- Emacs's default @kill-ring-max@.
capacity :: Int
capacity = 120

-- | The ring with a new newest entry.
--
-- >>> entries (kill "b" (kill "a" emptyKillRing))
-- ["b","a"]
kill :: Text -> KillRing -> KillRing
kill t (KillRing ts) = KillRing (take capacity (t : ts))

-- | The ring with text added to the end of its newest entry, as a kill
-- forward straight after another kill does; or a new entry, if it is empty.
--
-- >>> entries (appendKill "b" (kill "a" emptyKillRing))
-- ["ab"]
appendKill :: Text -> KillRing -> KillRing
appendKill t (KillRing ts) = case ts of
  newest : rest -> KillRing (newest <> t : rest)
  [] -> KillRing [t]

-- | The ring with text added to the start of its newest entry, as a kill
-- backward straight after another kill does; or a new entry, if it is
-- empty.
--
-- >>> entries (prependKill "b" (kill "a" emptyKillRing))
-- ["ba"]
prependKill :: Text -> KillRing -> KillRing
prependKill t (KillRing ts) = case ts of
  newest : rest -> KillRing (t <> newest : rest)
  [] -> KillRing [t]

-- | The entry a yank inserts: the newest, until the ring is rotated.
current :: KillRing -> Maybe Text
current (KillRing ts) = listToMaybe ts

-- | The ring turned by one, so that the next older entry is current and the
-- current one is the oldest: what a yank-pop does before it inserts.
--
-- >>> entries (rotate (kill "c" (kill "b" (kill "a" emptyKillRing))))
-- ["b","a","c"]
rotate :: KillRing -> KillRing
rotate (KillRing ts) = case ts of
  newest : rest -> KillRing (rest <> [newest])
  [] -> KillRing []

-- | The entries, the current one first.
entries :: KillRing -> [Text]
entries (KillRing ts) = ts
