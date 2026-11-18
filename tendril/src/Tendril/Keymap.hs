{-# LANGUAGE StrictData #-}

-- | Keymaps (DESIGN.md §5.4): tries from sequences of key chords to
-- commands.
--
-- __Chords.__ A t'KeyChord' is a key and the modifiers held with it, in one
-- normal form, which the input decoder (@Tendril.Term.Input@, M2.7) reaches
-- from every encoding it reads:
--
-- * A key that types a character is 'Char' of the character it types with
--   shift applied, and its chord never holds shift: @M-<@ is @\'<\'@ with
--   alt, not @\',\'@ with shift and alt, and Ctrl-Shift-A is @\'A\'@ with
--   ctrl, apart from @\'a\'@ with ctrl (which only the kitty keyboard
--   protocol can tell apart). A control character is its letter with ctrl:
--   C-a is @\'a\'@ with ctrl, and C-Space is @\' \'@ with ctrl.
-- * 'Enter', 'Tab', 'Backspace' and 'Escape' are keys of their own, never
--   C-m, C-i, C-? or C-[. A legacy terminal sends one byte for each pair, and
--   the decoder reads it as the named key; a keymap that wants C-h binds it
--   apart from 'Backspace'.
-- * Alt is a modifier, never an 'Escape' before the key: the decoder folds a
--   legacy ESC prefix into it.
--
-- __Tries.__ A sequence of chords is bound to a command, or is a prefix of
-- longer bound sequences, never both: a chord either completes a binding or
-- waits for the next one, as in Emacs, so a prefix needs no timeout. Binding
-- a sequence replaces whatever was in its way.
module Tendril.Keymap
  ( -- * Chords
    Key (..),
    Mods (..),
    noMods,
    KeyChord,
    chord,
    key,
    withCtrl,
    withAlt,
    withShift,
    withSuper,
    typed,
    describe,

    -- * Tries
    Trie,
    emptyTrie,
    bind,
    keymap,
    toBindings,
    Lookup (..),
    follow,
    walk,
    Conflict (..),
    conflicts,

    -- * Keymaps
    Keymap,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Records (HasField (..))
import Tendril.Edit.Command (Command)

-- | A key, by what it does rather than where it is: the logical key a
-- layout gives, as the W3C UI Events @key@ values name them.
data Key
  = -- | A key that types this character, with shift applied.
    Char Char
  | Enter
  | Tab
  | Backspace
  | Escape
  | -- | Insert, named apart from the 'Tendril.Edit.Command.Insert' command
    InsertKey
  | -- | Delete (forward), named apart from the 'Tendril.Edit.Command.Delete'
    -- command
    DeleteKey
  | Home
  | End
  | PageUp
  | PageDown
  | ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | -- | A function key, F1 as 1.
    F Word8
  deriving stock (Eq, Ord, Show)

-- | The modifiers held with a key.
data Mods = Mods
  { -- | Control
    ctrl :: Bool,
    -- | Alt, Option or Meta
    alt :: Bool,
    -- | Shift; never set on a 'Char' key's chord
    shift :: Bool,
    -- | Super, Command or Windows
    super :: Bool
  }
  deriving stock (Eq, Ord, Show)

-- | No modifier held.
noMods :: Mods
noMods = Mods {ctrl = False, alt = False, shift = False, super = False}

-- | A key with its modifiers, in the normal form above. Read its parts as
-- @c.key@ and @c.mods@; 'chord' builds one.
data KeyChord = KeyChord Key Mods
  deriving stock (Eq, Ord, Show)

instance HasField "key" KeyChord Key where
  getField (KeyChord k _) = k

instance HasField "mods" KeyChord Mods where
  getField (KeyChord _ m) = m

-- | A key with modifiers, in normal form: shift is dropped from a 'Char'
-- key, whose character already says it.
--
-- >>> chord noMods {shift = True} (Char '<') == key (Char '<')
-- True
chord :: Mods -> Key -> KeyChord
chord m k = case k of
  Char _ -> KeyChord k m {shift = False}
  _ -> KeyChord k m

-- | A key with no modifier.
key :: Key -> KeyChord
key = chord noMods

-- | The chord with ctrl held too.
withCtrl :: KeyChord -> KeyChord
withCtrl (KeyChord k m) = chord m {ctrl = True} k

-- | The chord with alt held too.
withAlt :: KeyChord -> KeyChord
withAlt (KeyChord k m) = chord m {alt = True} k

-- | The chord with shift held too; a 'Char' key's is unchanged, since its
-- character already says whether shift was held.
withShift :: KeyChord -> KeyChord
withShift (KeyChord k m) = chord m {shift = True} k

-- | The chord with super held too.
withSuper :: KeyChord -> KeyChord
withSuper (KeyChord k m) = chord m {super = True} k

-- | The text a chord types when no keymap binds it and no prefix is
-- pending: a 'Char' key's character, with neither ctrl, alt nor super held.
-- The editor inserts it (§5.8), so no binding starts with a printable
-- character; after a prefix one may be bound (@C-x u@).
--
-- >>> typed (key (Char 'a'))
-- Just "a"
-- >>> typed (withCtrl (key (Char 'a')))
-- Nothing
typed :: KeyChord -> Maybe Text
typed (KeyChord k m) = case k of
  Char c | not (m.ctrl || m.alt || m.super) -> Just (T.singleton c)
  _ -> Nothing

-- | A chord in Emacs's notation: @C-@ for ctrl, @M-@ for alt, @s-@ for
-- super and @S-@ for shift, then the key.
--
-- >>> describe (withAlt (withCtrl (key (Char '_'))))
-- "C-M-_"
-- >>> describe (withShift (key ArrowLeft))
-- "S-<left>"
describe :: KeyChord -> Text
describe (KeyChord k m) = foldMap prefix [(m.ctrl, "C-"), (m.alt, "M-"), (m.super, "s-"), (m.shift, "S-")] <> name
  where
    prefix (held, p) = if held then p else ""
    name = case k of
      Char ' ' -> "SPC"
      Char c -> T.singleton c
      Enter -> "RET"
      Tab -> "TAB"
      Backspace -> "DEL"
      Escape -> "ESC"
      InsertKey -> "<insert>"
      DeleteKey -> "<deletechar>"
      Home -> "<home>"
      End -> "<end>"
      PageUp -> "<prior>"
      PageDown -> "<next>"
      ArrowUp -> "<up>"
      ArrowDown -> "<down>"
      ArrowLeft -> "<left>"
      ArrowRight -> "<right>"
      F n -> "<f" <> show n <> ">"

-- | Sequences of keys of type @k@ bound to values of type @v@, where no
-- bound sequence is a prefix of another.
newtype Trie k v = Trie (Map k (Node k v))
  deriving stock (Eq, Show)

-- | What one key leads to.
data Node k v = Leaf v | Branch (Trie k v)
  deriving stock (Eq, Show)

-- | Nothing bound.
emptyTrie :: Trie k v
emptyTrie = Trie Map.empty

-- | The trie with a sequence bound to a value. What was in its way goes: a
-- binding of the sequence, of a prefix of it, or of sequences it is a
-- prefix of.
--
-- >>> walk "ab" (bind ('a' :| "b") 'y' (bind ('a' :| "") 'x' emptyTrie))
-- Bound 'y'
-- >>> walk "a" (bind ('a' :| "") 'x' (bind ('a' :| "b") 'y' emptyTrie))
-- Bound 'x'
bind :: (Ord k) => NonEmpty k -> v -> Trie k v -> Trie k v
bind (k :| ks) v (Trie m) = Trie (Map.insert k node m)
  where
    node = case nonEmpty ks of
      Nothing -> Leaf v
      Just rest -> Branch (bind rest v (below (Map.lookup k m)))
    below = \case
      Just (Branch t) -> t
      _ -> emptyTrie

-- | A trie of bindings, each made in turn by 'bind', so a later one replaces
-- an earlier one in its way; 'conflicts' says whether any did.
keymap :: (Ord k) => [(NonEmpty k, v)] -> Trie k v
keymap = foldl' (\t (ks, v) -> bind ks v t) emptyTrie

-- | Every bound sequence with its value, in the keys' order.
--
-- >>> toBindings (keymap [('b' :| "", 'x'), ('a' :| "c", 'y')])
-- [('a' :| "c",'y'),('b' :| "",'x')]
toBindings :: Trie k v -> [(NonEmpty k, v)]
toBindings (Trie m) =
  concat
    [ case n of
        Leaf v -> [(k :| [], v)]
        Branch t -> [(k :| toList ks, v) | (ks, v) <- toBindings t]
    | (k, n) <- Map.toAscList m
    ]

-- | What a sequence of keys leads to.
data Lookup k v
  = -- | A binding: the sequence is complete.
    Bound v
  | -- | The start of longer bindings, which these continue.
    Prefix (Trie k v)
  | -- | Nothing.
    Unbound
  deriving stock (Eq, Show)

-- | What one key leads to: the editor keeps a 'Prefix' and follows the next
-- key in it.
follow :: (Ord k) => k -> Trie k v -> Lookup k v
follow k (Trie m) = case Map.lookup k m of
  Just (Leaf v) -> Bound v
  Just (Branch t) -> Prefix t
  Nothing -> Unbound

-- | What a sequence of keys leads to; no keys lead to the whole trie.
--
-- >>> walk "a" (keymap [('a' :| "b", 'x')])
-- Prefix (Trie (fromList [('b',Leaf 'x')]))
-- >>> walk "ac" (keymap [('a' :| "b", 'x')])
-- Unbound
walk :: (Ord k) => [k] -> Trie k v -> Lookup k v
walk [] t = Prefix t
walk (k : ks) t = case follow k t of
  Bound v | null ks -> Bound v
  Prefix t' -> walk ks t'
  _ -> Unbound

-- | Two bindings that cannot both stand, since one sequence is the other or
-- a prefix of it: 'keymap' keeps the later.
data Conflict k = Conflict
  { -- | The sequence bound first
    earlier :: NonEmpty k,
    -- | The sequence bound after it
    later :: NonEmpty k
  }
  deriving stock (Eq, Show)

-- | Every pair of bindings that conflict, in the order they were bound.
--
-- >>> conflicts [('a' :| "", 'x'), ('b' :| "", 'y'), ('a' :| "b", 'z')]
-- [Conflict {earlier = 'a' :| "", later = 'a' :| "b"}]
conflicts :: (Ord k) => [(NonEmpty k, v)] -> [Conflict k]
conflicts bs =
  [ Conflict a b
  | (i, a) <- seqs,
    (j, b) <- seqs,
    i < j,
    toList a `isPrefixOf` toList b || toList b `isPrefixOf` toList a
  ]
  where
    seqs = zip [0 :: Int ..] (map fst bs)

-- | A keymap for modes of kind @m@: the mode is 'Command''s, whose role is
-- nominal (D30), so no coercion moves a keymap between modes either.
type Keymap app m = Trie KeyChord (Command app m)
