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

-- | The killed texts, newest first, at most 'capacity' of them, and how far
-- yank-pops have moved the yank pointer from the newest. As in Emacs, a
-- yank-pop moves the pointer and leaves the entries in their order, and a
-- kill puts its entry in front and the pointer back on it.
data KillRing = KillRing [Text] Int
  deriving stock (Eq, Show)

-- | A ring with nothing killed.
emptyKillRing :: KillRing
emptyKillRing = KillRing [] 0

-- | How many entries a ring keeps; a kill beyond it forgets the oldest.
-- Emacs's default @kill-ring-max@.
capacity :: Int
capacity = 120

-- | The ring with a new newest entry, which is current.
--
-- >>> entries (kill "b" (kill "a" emptyKillRing))
-- ["b","a"]
kill :: Text -> KillRing -> KillRing
kill t (KillRing ts _) = KillRing (take capacity (t : ts)) 0

-- | The ring with text added to the end of its newest entry, as a kill
-- forward straight after another kill does; or a new entry, if it is empty.
-- The newest entry is current.
--
-- >>> entries (appendKill "b" (kill "a" emptyKillRing))
-- ["ab"]
appendKill :: Text -> KillRing -> KillRing
appendKill t (KillRing ts _) = case ts of
  newest : rest -> KillRing (newest <> t : rest) 0
  [] -> KillRing [t] 0

-- | The ring with text added to the start of its newest entry, as a kill
-- backward straight after another kill does; or a new entry, if it is
-- empty. The newest entry is current.
--
-- >>> entries (prependKill "b" (kill "a" emptyKillRing))
-- ["ba"]
prependKill :: Text -> KillRing -> KillRing
prependKill t (KillRing ts _) = case ts of
  newest : rest -> KillRing (t <> newest : rest) 0
  [] -> KillRing [t] 0

-- | The entry a yank inserts: the one the yank pointer is on, which is the
-- newest until a yank-pop moves it.
current :: KillRing -> Maybe Text
current (KillRing ts i) = listToMaybe (drop i ts)

-- | The ring with its yank pointer moved to the next older entry, or from
-- the oldest back to the newest: what a yank-pop does before it inserts.
-- The entries keep their order.
--
-- >>> let ring = rotate (kill "c" (kill "b" (kill "a" emptyKillRing)))
-- >>> (current ring, entries ring)
-- (Just "b",["c","b","a"])
-- >>> current (rotate (rotate ring))
-- Just "c"
rotate :: KillRing -> KillRing
rotate (KillRing ts i)
  | i + 1 < length ts = KillRing ts (i + 1)
  | otherwise = KillRing ts 0

-- | The entries, newest first, wherever the yank pointer is.
entries :: KillRing -> [Text]
entries (KillRing ts _) = ts
