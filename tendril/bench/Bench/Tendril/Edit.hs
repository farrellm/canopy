-- | One keystroke's edit in a large pasted buffer (PLAN.md M2.4, DESIGN.md
-- §12.3, D34): about 1 MiB of mixed text (words, accents, Han, emoji with
-- skin tones), once as lines of about 80 code points and once as one line.
--
-- @to grapheme@ is what D-deferred-13 was about: a conversion of the end
-- of the buffer to a grapheme offset segments all of it. The rest are
-- commands at the end or in the middle, which find their boundaries by
-- segmenting from nearby (D34), and must not grow with the buffer. A
-- vertical motion counts the clusters into its line, so its cost is the
-- line's length; on the one long line there is no line above, and
-- @LineUp@ only finds that out.
module Bench.Tendril.Edit (benchmarks) where

import Data.Text qualified as T
import Tendril.Edit.Command (Command (..), EditState, ModeKind (..), apply, editState)
import Tendril.Edit.Motion (Motion (..))
import Tendril.Text.Buffer (BufferUnit (..), convert, fromText)
import Tendril.Text.Units (Unit (..), fromOffset)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnf)

-- | The benchmark tree.
benchmarks :: Benchmark
benchmarks =
  bgroup
    "Edit"
    [ shape "lines" (T.intercalate "\n" (T.chunksOf 80 pasted)),
      shape "one line" pasted
    ]

shape :: String -> Text -> Benchmark
shape name t = env (pure (let m0 = middle t in (fromText t, Strict m0, Strict (typedIn m0)))) $ \(~(b, Strict m, Strict w)) ->
  bgroup
    name
    [ bench "to grapheme" (whnf (\b' -> convert @'CodePoint @'Grapheme b' (extent b')) b),
      bench "PrevCluster at the end" (whnf (cursorAfter [Move PrevCluster] . editState) b),
      bench "PrevWord at the end" (whnf (cursorAfter [Move PrevWord] . editState) b),
      bench "LineEnd at the end" (whnf (cursorAfter [Move LineEnd] . editState) b),
      bench "LineUp at the end" (whnf (cursorAfter [Move LineUp] . editState) b),
      bench "Insert in the middle" (whnf (cursorAfter [Insert "x"]) m),
      bench "Delete PrevCluster in the middle" (whnf (cursorAfter [Delete PrevCluster]) m),
      bench "NextCluster in the middle" (whnf (cursorAfter [Move NextCluster]) m),
      bench "Insert after 5,000 typed" (whnf (cursorAfter [Insert "x"]) w),
      bench "PrevCluster after 5,000 typed" (whnf (cursorAfter [Move PrevCluster]) w),
      bench "PrevWord after 5,000 typed" (whnf (cursorAfter [Move PrevWord]) w)
    ]

-- | The state after 5,000 characters typed one at a time: the rope as it is
-- after a long session of edits, not freshly balanced.
typedIn :: EditState -> EditState
typedIn = run (map (Insert . T.singleton) (take 5000 (cycle "typing some words, ")))

-- | A state with the cursor halfway through the text: the first half, with
-- the second inserted at its end.
middle :: Text -> EditState
middle t = run [Insert after] (editState (fromText before))
  where
    (before, after) = T.splitAt (T.length t `div` 2) t

-- | A state forced when the environment is: its fields are strict, so
-- weak head normal form is all of it.
newtype Strict = Strict EditState

instance NFData Strict where
  rnf (Strict s) = s `seq` ()

-- | The cursor after the commands, as a count: forcing it forces the
-- buffer the commands built.
cursorAfter :: [Command () 'Emacs] -> EditState -> Word
cursorAfter cs s = fromOffset (run cs s).cursor

run :: [Command () 'Emacs] -> EditState -> EditState
run cs s = foldl' (\s' c -> fromMaybe s' (apply c s')) s cs

-- | About 1 MiB of text, in code points.
pasted :: Text
pasted = T.take (1024 * 1024) (T.replicate 30000 "the quick brown fox jumps over the lazy dog, caf\x00E9 na\x0131\x0308ve \x6F22\x5B57 \x1F44D\x1F3FD ok. ")
