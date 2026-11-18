{-# LANGUAGE StrictData #-}

-- | Keymaps (DESIGN.md §5.4): tries from sequences of key chords to
-- commands.
--
-- __Chords.__ A t'KeyChord' is a key and the modifiers held with it, in one
-- normal form, which the input decoder (@Tendril.Term.Input@, M2.7) reaches
-- from every encoding it reads:
--
-- * A key that types a character is 'Char' of the character it types with
--   shift applied: @M-<@ is @\'<\'@ with alt, not @\',\'@ with shift and
--   alt, and Ctrl-Shift-A is @\'A\'@ with ctrl, apart from @\'a\'@ with
--   ctrl (which only the kitty keyboard protocol can tell apart). Which
--   character shift gives depends on the layout, so 'chord' applies it only
--   where no layout is needed: a letter with a case becomes its capital and
--   drops shift. Any other character keeps shift, so a decoder that is given
--   the unshifted key (kitty without its alternate keys) makes @C-S-/@, never
--   @C-/@; the decoder gives the shifted character wherever the terminal
--   reports it. A control character is its letter with ctrl: C-a is
--   @\'a\'@ with ctrl, and C-Space is @\' \'@ with ctrl. 'chord' reaches
--   this from a control character too, so no chord holds one.
-- * 'Enter', 'Tab', 'Backspace' and 'Escape' are keys of their own, never
--   C-m, C-i, C-? or C-[. A legacy terminal sends one byte for each pair, and
--   the decoder reads it as the named key; a keymap that wants C-h binds it
--   apart from 'Backspace'. Many terminals send C-Backspace as C-h's byte,
--   which reads as C-h: only a terminal that tells them apart (the kitty
--   keyboard protocol) delivers C-Backspace.
-- * Alt is a modifier, never an 'Escape' before the key: the decoder folds a
--   legacy ESC prefix into it.
--
-- __Tries.__ A sequence of chords is bound to a command, or is a prefix of
-- longer bound sequences, never both: a chord either completes a binding or
-- waits for the next one, as in Emacs, so a prefix needs no timeout. Binding
-- a sequence replaces whatever was in its way, and 'conflicts' reports what it
-- replaced.
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
    keymapWithConflicts,
    toBindings,
    Lookup (..),
    follow,
    walk,
    Conflict (..),
    conflicts,

    -- * Keymaps
    Keymap,
    printableStarts,
  )
where

import Data.Char (GeneralCategory (Surrogate), generalCategory, isAsciiUpper, isControl, isUpper, toLower, toUpper)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Records (HasField (..))
import Tendril.Edit.Command (Command)

-- | A key, by what it does rather than where it is: the logical key a
-- layout gives, as the W3C UI Events @key@ values name them.
data Key
  = -- | A key that types this character, with shift applied.
    Char Char
  | -- | Enter or Return
    Enter
  | -- | Tab
    Tab
  | -- | Backspace, which deletes back
    Backspace
  | -- | Escape
    Escape
  | -- | Insert, named apart from the 'Tendril.Edit.Command.Insert' command
    InsertKey
  | -- | Delete (forward), named apart from the 'Tendril.Edit.Command.Delete'
    -- command
    DeleteKey
  | -- | Home
    Home
  | -- | End
    End
  | -- | Page Up
    PageUp
  | -- | Page Down
    PageDown
  | -- | The up arrow
    ArrowUp
  | -- | The down arrow
    ArrowDown
  | -- | The left arrow
    ArrowLeft
  | -- | The right arrow
    ArrowRight
  | -- | A function key, F1 as 1.
    F Word8
  deriving stock (Eq, Ord, Show)

-- | The modifiers held with a key.
data Mods = Mods
  { -- | Control
    ctrl :: Bool,
    -- | Alt, Option or Meta
    alt :: Bool,
    -- | Shift; on a 'Char' key's chord, only with a character that has no
    -- case, whose shifted form tendril cannot know
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

-- | A key with modifiers, in normal form: shift on a 'Char' key with a case
-- is applied (the capital) and dropped, and kept on any other, and a control
-- character is its named key ('Tab', 'Enter', 'Escape', 'Backspace') or its
-- letter with ctrl.
--
-- >>> chord noMods {shift = True} (Char 'a') == key (Char 'A')
-- True
-- >>> describe (chord noMods {shift = True, ctrl = True} (Char '/'))
-- "C-S-/"
-- >>> chord noMods (Char '\t') == key Tab
-- True
-- >>> chord noMods (Char '\SOH') == withCtrl (key (Char 'a'))
-- True
chord :: Mods -> Key -> KeyChord
chord m = \case
  Char '\t' -> KeyChord Tab m
  Char '\r' -> KeyChord Enter m
  Char '\ESC' -> KeyChord Escape m
  Char '\DEL' -> KeyChord Backspace m
  Char c
    | c == '\NUL' -> ctrlChar ' '
    | c < '\ESC' -> ctrlChar (toEnum (fromEnum c + 0x60))
    | c < ' ' -> ctrlChar (toEnum (fromEnum c + 0x40))
    | m.shift && (isUpper c || toUpper c /= c) -> KeyChord (Char (toUpper c)) m {shift = False}
    | otherwise -> KeyChord (Char c) m
  k -> KeyChord k m
  where
    -- A printable character, so this is the case above.
    ctrlChar c = chord m {ctrl = True} (Char c)

-- | A key with no modifier.
key :: Key -> KeyChord
key = chord noMods

-- | The chord with ctrl held too.
withCtrl :: KeyChord -> KeyChord
withCtrl (KeyChord k m) = chord m {ctrl = True} k

-- | The chord with alt held too.
withAlt :: KeyChord -> KeyChord
withAlt (KeyChord k m) = chord m {alt = True} k

-- | The chord with shift held too; a letter's is its capital, as 'chord'
-- applies shift.
withShift :: KeyChord -> KeyChord
withShift (KeyChord k m) = chord m {shift = True} k

-- | The chord with super held too.
withSuper :: KeyChord -> KeyChord
withSuper (KeyChord k m) = chord m {super = True} k

-- | The text a chord types when no keymap binds it and no prefix is
-- pending: a 'Char' key's character, with no modifier held, unless it is a
-- control character (a C1 one, since 'chord' leaves no other) or a surrogate.
-- A chord that still holds shift types nothing: its character is the key's
-- unshifted one, not the one shift gives.
-- The editor inserts it (§5.8), so no binding starts with a printable
-- character; after a prefix one may be bound (@C-x u@).
--
-- >>> typed (key (Char 'a'))
-- Just "a"
-- >>> typed (withCtrl (key (Char 'a')))
-- Nothing
typed :: KeyChord -> Maybe Text
typed (KeyChord k m) = case k of
  Char c
    | not (m.ctrl || m.alt || m.shift || m.super || isControl c || generalCategory c == Surrogate) ->
        Just (T.singleton c)
  _ -> Nothing

-- | A chord in Emacs's notation: @C-@ for ctrl, @M-@ for alt, @S-@ for
-- shift and @s-@ for super, in Emacs's order, then the key. Emacs reads
-- @C-A@ as @C-a@, so an ASCII capital with ctrl is written with @S-@ and its
-- small letter. Enter, Tab, Backspace and Escape are @RET@, @TAB@, @DEL@
-- and @ESC@ alone or with alt, which Emacs takes them to, and @\<return\>@,
-- @\<tab\>@, @\<backspace\>@ and @\<escape\>@ with ctrl, shift or super,
-- which it does not.
--
-- >>> describe (withAlt (withCtrl (key (Char '_'))))
-- "C-M-_"
-- >>> describe (withShift (key ArrowLeft))
-- "S-<left>"
-- >>> describe (withCtrl (key (Char 'A')))
-- "C-S-a"
-- >>> map describe [withAlt (key Backspace), withCtrl (key Backspace)]
-- ["M-DEL","C-<backspace>"]
describe :: KeyChord -> Text
describe (KeyChord k m) = foldMap prefix [(m.ctrl, "C-"), (m.alt, "M-"), (m.shift || folded, "S-"), (m.super, "s-")] <> name
  where
    prefix (held, p) = if held then p else ""
    special plain named = if m.ctrl || m.shift || m.super then named else plain
    folded = case k of
      Char c -> m.ctrl && isAsciiUpper c
      _ -> False
    name = case k of
      Char c | folded -> T.singleton (toLower c)
      Char ' ' -> "SPC"
      Char c -> T.singleton c
      Enter -> special "RET" "<return>"
      Tab -> special "TAB" "<tab>"
      Backspace -> special "DEL" "<backspace>"
      Escape -> special "ESC" "<escape>"
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
bind ks v = fst . insert ks v

-- | 'bind', with the bound sequences it replaced, in the keys' order.
insert :: (Ord k) => NonEmpty k -> v -> Trie k v -> (Trie k v, [NonEmpty k])
insert (k :| ks) v (Trie m) = (Trie (Map.insert k node m), map (k :|) gone)
  where
    old = Map.lookup k m
    (node, gone) = case (nonEmpty ks, old) of
      (Nothing, Nothing) -> (Leaf v, [])
      (Nothing, Just (Leaf _)) -> (Leaf v, [[]])
      (Nothing, Just (Branch t)) -> (Leaf v, [toList r | (r, _) <- toBindings t])
      (Just rest, Just (Branch t)) -> bimap Branch (map toList) (insert rest v t)
      (Just rest, _) -> (Branch (bind rest v emptyTrie), [[] | isJust old])

-- | A trie of bindings, each made in turn by 'bind', so a later one replaces
-- an earlier one in its way; 'conflicts' says whether any did.
keymap :: (Ord k) => [(NonEmpty k, v)] -> Trie k v
keymap = fst . keymapWithConflicts

-- | 'keymap' and 'conflicts' from one fold of the bindings, for a caller
-- that wants both: they cannot disagree on what a binding replaces.
--
-- >>> keymapWithConflicts [('a' :| "", 'x'), ('a' :| "b", 'y')]
-- (Trie (fromList [('a',Branch (Trie (fromList [('b',Leaf 'y')])))]),[Conflict {earlier = 'a' :| "", later = 'a' :| "b"}])
keymapWithConflicts :: (Ord k) => [(NonEmpty k, v)] -> (Trie k v, [Conflict k])
keymapWithConflicts = second (concat . reverse) . foldl' step (emptyTrie, [])
  where
    -- The trie is forced at each step, so the fold leaves no chain of thunks.
    step (t, cs) (ks, v) = case insert ks v t of
      (t', gone) -> t' `seq` (t', [Conflict g ks | g <- gone] : cs)

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

-- | A binding that a later one replaced, since one sequence is the other or
-- a prefix of it: 'keymap' keeps the later.
data Conflict k = Conflict
  { -- | The sequence replaced
    earlier :: NonEmpty k,
    -- | The sequence bound after it, which replaced it
    later :: NonEmpty k
  }
  deriving stock (Eq, Show)

-- | Each binding that 'keymap' replaces with a later one: in the order the
-- later ones are bound, and for one, in the keys' order. A binding already
-- replaced is not replaced again, so this is empty exactly when no two of
-- the bindings conflict.
--
-- >>> conflicts [('a' :| "", 'x'), ('b' :| "", 'y'), ('a' :| "b", 'z')]
-- [Conflict {earlier = 'a' :| "", later = 'a' :| "b"}]
-- >>> map (.earlier) (conflicts [('a' :| "b", 'x'), ('a' :| "c", 'y'), ('a' :| "", 'z')])
-- ['a' :| "b",'a' :| "c"]
conflicts :: (Ord k) => [(NonEmpty k, v)] -> [Conflict k]
conflicts = snd . keymapWithConflicts

-- | The bound sequences that start with a chord that types ('typed'), which
-- D36 rules out: with no prefix pending, the binding would take the chord,
-- and typing its character would run the command. The built-in keymaps have
-- none, which a test holds; the user-file decoder (M6.2) reports them as it
-- does conflicts.
--
-- >>> map (map describe . toList) (printableStarts (keymap [(key (Char 'a') :| [], ()), (withCtrl (key (Char 'a')) :| [], ())]))
-- [["a"]]
printableStarts :: Trie KeyChord v -> [NonEmpty KeyChord]
printableStarts t = [ks | (ks@(k :| _), _) <- toBindings t, isJust (typed k)]

-- | A keymap for modes of kind @m@: the mode is 'Command''s, whose role is
-- nominal (D30), so no coercion moves a keymap between modes either.
type Keymap app m = Trie KeyChord (Command app m)
