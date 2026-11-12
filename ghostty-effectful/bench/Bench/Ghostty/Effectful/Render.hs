-- | Rendering cost (DESIGN.md §4.3, §12.3): reading frames of an 80×24
-- terminal through a render state, as a view does each time it draws. One
-- iteration is a terminal's whole life, as in "Bench.Ghostty.Effectful.Feed":
-- 'runGhostty', open, feed a screen, open a render state, read, free. @no
-- frame@ is that life without the read, so the per-frame cost can be told
-- apart from the fixed one.
--
-- The frames copy every cell out ('cells'), which is the per-cell path: a
-- cell's raw value, width, style, graphemes, colours and hyperlink, each a
-- call. CI gates allocation here as it does for the feed (§2.8 step 8).
module Bench.Ghostty.Effectful.Render (benchmarks) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful (Eff, IOE, runEff)
import Ghostty.Effectful (Ghostty, runGhostty)
import Ghostty.Effectful.Cell (Cell)
import Ghostty.Effectful.Render (FrameData (..), RenderState, Row, allRows, cells, dirtyRows, getF, withFrame, withRenderState)
import Ghostty.Effectful.Terminal (GridSize (..), Terminal, extent, feed, terminalConfig, withTerminal)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnfIO)

-- | The benchmark tree.
benchmarks :: Benchmark
benchmarks =
  bgroup
    "Render"
    [ screen "no frame" styled (\_ _ -> pure 0),
      screen "plain, every cell" plain everyCell,
      screen "styled, every cell" styled everyCell,
      screen "unicode, every cell" unicode everyCell,
      screen "cursor and colours" styled cursorAndColours,
      env (pure (styled, lines24)) $ \ ~(s, ls) ->
        bench "24 frames of one new line" (whnfIO (rendered s (oneLineAtATime ls)))
    ]
  where
    screen :: String -> ByteString -> Reader -> Benchmark
    screen name bytes k = env (pure bytes) $ \b -> bench name (whnfIO (rendered b k))

-- | What a benchmark reads from a terminal and its render state, counted.
type Reader = forall (s :: Type) (t :: Type). Terminal t -> RenderState s -> Eff '[Ghostty, IOE] Int

-- | Feed a fresh 80×24 terminal the screen, open a render state on it, and
-- run the reader, whose count forces what it read.
rendered :: ByteString -> Reader -> IO Int
rendered bytes k = ghostty $ do
  g <- maybe (liftIO (die "80×24 is empty")) pure (GridSize <$> extent 80 <*> extent 24)
  withTerminal (terminalConfig g) $ \t -> do
    _ <- feed t bytes
    withRenderState t (k t)

-- | One frame, every cell of every row copied out.
everyCell :: Reader
everyCell _ rs = withFrame rs (countRows <=< allRows)

-- | One frame's cursor and colours (the palette, 256 entries, included).
cursorAndColours :: Reader
cursorAndColours _ rs = withFrame rs $ \f -> do
  c <- getF f RenderCursor
  cs <- getF f RenderColors
  pure (fromEnum (c == c) + fromEnum (cs == cs))

-- | The steady state of a REPL: a frame of the whole screen, then, 24
-- times, one more line fed and a frame of only the rows that changed.
oneLineAtATime :: [ByteString] -> Reader
oneLineAtATime ls t rs = do
  whole <- everyCell t rs
  -- Counted as it goes: a lazy sum would keep every frame's cells alive
  -- until the end, and the benchmark would measure the collector copying them.
  foldlM (\ !acc l -> feed t l >> withFrame rs (fmap (acc +) . countRows <=< dirtyRows)) whole ls

-- | The rows' cells copied out and counted, each row's forced before the
-- next is read.
countRows :: [Row s f] -> Eff '[Ghostty, IOE] Int
countRows = foldlM (\ !acc r -> cells r <&> \v -> acc + forced v) 0

-- | How many of the cells are equal to themselves: all of them, but
-- comparing forces every field of each, which is the copy a view would
-- read, without allocating as 'show' would.
forced :: V.Vector Cell -> Int
forced = V.length . V.filter (\c -> c == c)

-- | Run under the effect; a version mismatch ends the run.
ghostty :: Eff '[Ghostty, IOE] a -> IO a
ghostty act = runEff (runGhostty act) >>= either (die . show) pure

-- | 24 lines of 78 printable ASCII characters.
plain :: ByteString
plain = mconcat (take 24 [encodeUtf8 (T.take 78 (T.drop (i `mod` 95) printable)) <> "\r\n" | i <- [0 :: Int ..]])
  where
    printable = T.pack (concat (replicate 2 [' ' .. '~']))

-- | 24 lines of coloured words, an SGR change before each: a style and
-- colours per cell.
styled :: ByteString
styled = mconcat lines24

-- | The styled screen's lines.
lines24 :: [ByteString]
lines24 = [line i | i <- [0 :: Int .. 23]]
  where
    line i = mconcat [colour (i + w) <> "word" <> show w <> " " | w <- [0 .. 9 :: Int]] <> "\ESC[0m\r\n"
    colour n = "\ESC[" <> show (31 + n `mod` 7) <> ";" <> show (1 + n `mod` 4) <> "m"

-- | 24 lines of wide characters, emoji (one a ZWJ sequence) and combining
-- marks: multi-codepoint graphemes and wide cells.
unicode :: ByteString
unicode = mconcat (take 24 (cycle [encodeUtf8 l <> "\r\n" | l <- samples]))
  where
    samples :: [Text]
    samples =
      [ "漢字かなカナ한국어 全角の行 " <> T.replicate 4 "终端模拟器",
        "e\x0301 a\x0308 n\x0303 " <> T.replicate 6 "\x1F600\x1F469\x200D\x1F4BB ",
        "Ελληνικά Кириллица עברית " <> T.replicate 3 "\x1F1EF\x1F1F5 \x1F44D\x1F3FD "
      ]
