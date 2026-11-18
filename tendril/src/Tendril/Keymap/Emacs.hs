-- | The Emacs keymap (DESIGN.md §5.4): Emacs's bindings, and readline's
-- where a line editor's need differs, for every command tendril has so far.
--
-- Its type is the compile-time mode check: every binding is a
-- @'Command' app ''Emacs'@, so a command of another mode's kind cannot be
-- bound in it.
--
-- What it leaves to others:
--
-- * A printable character types itself: the editor inserts what
--   'Tendril.Keymap.typed' gives for a chord no binding takes (§5.8), so
--   no binding starts with one.
-- * C-d deletes on, and on an empty input the editor ends input instead,
--   as readline does; C-c interrupts, and is the editor's, not a command
--   (M2.11).
-- * 'ArrowUp' and C-p move a line up, and 'ArrowDown' and C-n a line down.
--   On the first line, or the last, where they do not move, the editor
--   recalls history instead (M2.11), as prompt_toolkit and reedline do; M-p
--   and M-n always recall it.
-- * The registers' commands (@C-x r s@, @C-x r i@) name the register with
--   the next key, which a trie of commands cannot say, and neither can a
--   quoted insert (@C-q@); the editor's key-reading commands come with the
--   editor. Tab's completion menu and C-r's history search come with their
--   widgets (M2.12).
module Tendril.Keymap.Emacs
  ( emacs,
    bindings,
  )
where

import Tendril.Edit.Command (Command (..), ModeKind (..))
import Tendril.Edit.Motion (Motion (..))
import Tendril.Keymap (Key (..), KeyChord, Keymap, key, keymap, withAlt, withCtrl, withShift)

-- | The Emacs keymap.
emacs :: Keymap app 'Emacs
emacs = keymap bindings

-- | Its bindings, in the order they are made. No two conflict, which a test
-- holds.
bindings :: [(NonEmpty KeyChord, Command app 'Emacs)]
bindings =
  map
    (first (:| []))
    ( -- Motions
      [ (c 'f', Move NextCluster),
        (key ArrowRight, Move NextCluster),
        (c 'b', Move PrevCluster),
        (key ArrowLeft, Move PrevCluster),
        (m 'f', Move NextWord),
        (withCtrl (key ArrowRight), Move NextWord),
        (withAlt (key ArrowRight), Move NextWord),
        (m 'b', Move PrevWord),
        (withCtrl (key ArrowLeft), Move PrevWord),
        (withAlt (key ArrowLeft), Move PrevWord),
        (c 'a', Move LineStart),
        (key Home, Move LineStart),
        (c 'e', Move LineEnd),
        (key End, Move LineEnd),
        (c 'p', Move LineUp),
        (key ArrowUp, Move LineUp),
        (c 'n', Move LineDown),
        (key ArrowDown, Move LineDown),
        (m '<', Move BufferStart),
        (withCtrl (key Home), Move BufferStart),
        (m '>', Move BufferEnd),
        (withCtrl (key End), Move BufferEnd)
      ]
        -- The mark and the region, and Emacs's shift-select-mode
        <> [ (c ' ', SetMark),
             (c '@', SetMark),
             (c 'g', Deselect),
             (c 'w', KillSelection),
             (m 'w', CopySelection)
           ]
        <> [ (withShift k, Select motion)
           | (k, motion) <-
               [ (key ArrowRight, NextCluster),
                 (key ArrowLeft, PrevCluster),
                 (withCtrl (key ArrowRight), NextWord),
                 (withCtrl (key ArrowLeft), PrevWord),
                 (key Home, LineStart),
                 (key End, LineEnd),
                 (key ArrowUp, LineUp),
                 (key ArrowDown, LineDown),
                 (withCtrl (key Home), BufferStart),
                 (withCtrl (key End), BufferEnd)
               ]
           ]
        -- Deleting and killing. C-u is readline's unix-line-discard, since
        -- tendril has no prefix argument.
        <> [ (c 'd', Delete NextCluster),
             (key DeleteKey, Delete NextCluster),
             (key Backspace, Delete PrevCluster),
             (c 'h', Delete PrevCluster),
             (c 'k', Kill LineEnd),
             (c 'u', Kill LineStart),
             (m 'd', Kill NextWord),
             (withCtrl (key DeleteKey), Kill NextWord),
             (withAlt (key Backspace), Kill PrevWord),
             (withCtrl (key Backspace), Kill PrevWord),
             (c 'y', Yank),
             (m 'y', YankPop)
           ]
        -- Undo, and Emacs 28's undo-redo on C-M-_; and on M-_ for its C-?,
        -- which a legacy terminal sends as DEL. A legacy terminal sends C-/
        -- as C-_.
        <> [ (c '/', Undo),
             (c '_', Undo),
             (withAlt (c '_'), Redo),
             (m '_', Redo)
           ]
        -- The input, and history
        <> [ (key Enter, AcceptOrNewline),
             (withAlt (key Enter), ForceNewline),
             (withShift (key Enter), ForceNewline),
             (c 'j', ForceNewline),
             (m 'p', HistoryPrev),
             (m 'n', HistoryNext)
           ]
        -- Structural selection, on Helix's keys
        <> [ (withAlt (key ArrowUp), ExpandSelection),
             (withAlt (key ArrowDown), ShrinkSelection)
           ]
    )
    -- Behind C-x
    <> [ (c 'x' :| [key (Char 'u')], Undo),
         (c 'x' :| [c 'e'], EditExternal)
       ]
  where
    c = withCtrl . key . Char
    m = withAlt . key . Char
