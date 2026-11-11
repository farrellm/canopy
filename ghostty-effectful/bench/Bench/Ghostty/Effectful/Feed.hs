-- | @feed@ throughput (PLAN.md M1.10, DESIGN.md §12.3): canned VT streams of
-- about 1 MiB written to a fresh 80×24 terminal, each the shape of output a
-- child sends. One iteration is a terminal's whole life: 'runGhostty', open,
-- feed, free. @open and close@ is that life with nothing fed, so the fixed
-- cost can be told apart from the per-byte one.
--
-- CI gates every benchmark here on allocation (§2.8 step 8). Allocation is
-- the Haskell side's only: what libghostty allocates is not counted, so a
-- regression in the gate is one in the binding's own work per write, per
-- OSC stop and per event.
module Bench.Ghostty.Effectful.Feed (benchmarks) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Effectful (Eff, IOE, runEff)
import Ghostty.Effectful (Ghostty, runGhostty)
import Ghostty.Effectful.Terminal (GridSize (..), extent, feed, terminalConfig, withTerminal)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnfIO)

-- | The benchmark tree.
benchmarks :: Benchmark
benchmarks =
  bgroup
    "Feed"
    [ bench "open and close" (whnfIO (feedFresh [])),
      stream "plain" plain,
      stream "sgr" sgr,
      stream "unicode" unicode,
      stream "prompts" prompts,
      env (pure (chunksOf 4096 plain)) $ \cs ->
        bench "plain, 4 KiB writes" (whnfIO (feedFresh cs))
    ]
  where
    stream name bytes = env (pure bytes) $ \b -> bench name (whnfIO (feedFresh [b]))

-- | Feed the chunks, in order, to a fresh 80×24 terminal, and count the
-- events, which forces them.
feedFresh :: [ByteString] -> IO Int
feedFresh chunks = ghostty $ do
  g <- maybe (liftIO (die "80×24 is empty")) pure (GridSize <$> extent 80 <*> extent 24)
  withTerminal (terminalConfig g) $ \t -> sum <$> traverse (fmap length . feed t) chunks

-- | Run under the effect; a version mismatch ends the run.
ghostty :: Eff '[Ghostty, IOE] a -> IO a
ghostty act = runEff (runGhostty act) >>= either (die . show) pure

-- | Lines of 78 printable ASCII characters, scrolling: the parser's fast
-- path, and the plain case of a child's output.
plain :: ByteString
plain = toMiB [encodeUtf8 (T.take 78 (T.drop (i `mod` 95) printable)) <> "\r\n" | i <- [0 :: Int ..]]
  where
    printable = T.pack (concat (replicate 2 [' ' .. '~']))

-- | Coloured words, an SGR change before each, as from a compiler or
-- @ls --color@: the CSI path, and a style per cell.
sgr :: ByteString
sgr = toMiB [line i | i <- [0 :: Int ..]]
  where
    line i = mconcat [colour (i + w) <> "word" <> show w <> " " | w <- [0 .. 9 :: Int]] <> "\ESC[0m\r\n"
    colour n = "\ESC[" <> show (31 + n `mod` 7) <> ";" <> show (1 + n `mod` 4) <> "m"

-- | Wide characters, emoji (one a ZWJ sequence), and combining marks: the
-- UTF-8 decoder, the width tables and grapheme clustering.
unicode :: ByteString
unicode = toMiB (cycle [encodeUtf8 l <> "\r\n" | l <- samples])
  where
    samples :: [Text]
    samples =
      [ "漢字かなカナ한국어 全角の行 " <> T.replicate 4 "终端模拟器",
        "e\x0301 a\x0308 n\x0303 " <> T.replicate 6 "\x1F600\x1F469\x200D\x1F4BB ",
        "Ελληνικά Кириллица עברית " <> T.replicate 3 "\x1F1EF\x1F1F5 \x1F44D\x1F3FD "
      ]

-- | A REPL session: each input and its output between OSC 133 marks, so
-- that every mark's cursor is read (in its callback, D20).
-- The OSC path, and the binding's cost per stop and per event.
prompts :: ByteString
prompts = toMiB [block i | i <- [0 :: Int ..]]
  where
    block i =
      "\ESC]133;A\a>>> \ESC]133;B\ax"
        <> show i
        <> " = "
        <> show i
        <> "\r\n\ESC]133;C\a"
        <> show i
        <> "\r\n\ESC]133;D;0\a"

-- | The pieces, concatenated, up to the first that reaches 1 MiB.
toMiB :: [ByteString] -> ByteString
toMiB = go 0 []
  where
    go :: Int -> [ByteString] -> [ByteString] -> ByteString
    go n acc (x : xs) | n < mib = go (n + BS.length x) (x : acc) xs
    go _ acc _ = BS.concat (reverse acc)
    mib = 1024 * 1024

-- | Split into pieces of @n@ bytes, the last possibly shorter, as a PTY's
-- reads would.
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n b
  | BS.null b = []
  | otherwise = let (h, t) = BS.splitAt n b in h : chunksOf n t
