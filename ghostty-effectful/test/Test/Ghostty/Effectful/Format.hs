-- | The formatter (PLAN.md M1.8, DESIGN.md §4.8): goldens of canned VT
-- streams in every format, and the ranges and options around them.
--
-- Each golden is a stream fed to a 20×5 terminal and formatted, in
-- @test/golden/format/<stream>[-<variant>].<plain|vt|html>@. Control bytes
-- are written visibly, so a golden can be read: ESC as @\\e@, BEL as @\\a@,
-- CR as @\\r@, and any other C0 byte but LF as @\\xNN@. Goldens are read
-- before they are accepted (@--accept@), and the commit says why the output
-- is right (§11.1).
module Test.Ghostty.Effectful.Format (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.List (lookup)
import Effectful (Eff, IOE)
import Ghostty.Effectful (Ghostty)
import Ghostty.Effectful.Format
import Ghostty.Effectful.Point (Coord (..), Point (..))
import Ghostty.Effectful.Terminal
import Numeric (showHex)
import Test.Ghostty.Effectful.Support (ghostty, grid)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Format"
    [ testGroup "goldens" goldens,
      testGroup "ranges" ranges,
      testGroup "options" options
    ]

-- * Goldens

-- | The canned streams, by name.
streams :: [(String, Text)]
streams =
  [ -- The 16 colours, the 256, direct colour, and attributes, on and off.
    ( "colours",
      "plain \ESC[31mred\ESC[0m \ESC[1;38;2;1;2;3mrgb\ESC[0m\r\n"
        <> "\ESC[42mgreen bg\ESC[0m \ESC[4mund\ESC[24m\r\n"
        <> "\ESC[38;5;208m208\ESC[m \ESC[3;9mit-s\ESC[m \ESC[7minv\ESC[m"
    ),
    -- Wide characters take two cells, and a combining mark joins its base.
    -- Outside grapheme-cluster mode (2027) an emoji ZWJ sequence is two wide
    -- cells, the joiner kept with the first, as the block golden shows.
    ( "wide",
      "宽字符 a😀b\r\n"
        <> "e\x0301 n\x0303 x\r\n"
        <> "👩\x200D💻 ok"
    ),
    -- A line longer than the 20 columns wraps softly; CR LF is a hard
    -- break. A wide character that does not fit at the end of a row moves
    -- to the next.
    ( "wraps",
      "0123456789abcdefghijKLMNO\r\n"
        <> "short\r\n"
        <> "0123456789abcdefghi宽z"
    ),
    -- OSC 8 links, ended by ST and by BEL, one with an id.
    ( "hyperlinks",
      "\ESC]8;;https://example.com/\ESC\\link\ESC]8;;\ESC\\ after\r\n"
        <> "\ESC]8;id=x;file:///tmp/a\atwo\ESC]8;;\a."
    ),
    -- Eight lines into five rows: three scroll into the scrollback, which a
    -- format of the screen includes.
    ( "scrollback",
      mconcat (intersperse "\r\n" ["line " <> show i | i <- [1 .. 8 :: Int]])
    )
  ]

-- | Every stream in every format, and the variants that change the output.
goldens :: [TestTree]
goldens =
  [ golden name "" f (`format` formatOptions f)
  | (name, _) <- streams,
    f <- [minBound .. maxBound]
  ]
    <> [ golden "wraps" "unwrap" Plain (`format` (formatOptions Plain) {unwrap = True}),
         golden "colours" "notrim" Vt (`format` (formatOptions Vt) {trim = False}),
         golden "colours" "rows" Vt $ \t -> fromMaybe "no range" <$> formatRange t (formatOptions Vt) (WholeRows @'Active 1 2),
         golden "wide" "block" Plain $ \t ->
           fromMaybe "no range" <$> formatRange t (formatOptions Plain) (Block @'Active (Point 2 0) (Point 6 2)),
         golden "hyperlinks" "span" Html $ \t ->
           fromMaybe "no range" <$> formatRange t (formatOptions Html) (Span @'Active (Point 2 0) (Point 1 1))
       ]

-- | One golden: the named stream fed to a fresh terminal, then formatted.
golden :: String -> String -> Format -> (forall s. Terminal s -> Eff '[Ghostty, IOE] ByteString) -> TestTree
golden name variant f k =
  goldenVsString file ("test/golden/format/" <> file) $ do
    bytes <- maybe (assertFailure ("no stream " <> name)) (pure . encodeUtf8) (lookup name streams)
    visible <$> ghostty (withTerminal config $ \t -> feed t bytes *> k t)
  where
    file = name <> (if null variant then "" else "-" <> variant) <> "." <> ext f
    ext = \case
      Plain -> "plain"
      Vt -> "vt"
      Html -> "html"

-- | Control bytes written visibly, as the module header says.
visible :: ByteString -> LByteString
visible = toLazy . BS.concatMap byte
  where
    byte = \case
      0x1b -> "\\e"
      0x07 -> "\\a"
      0x0d -> "\\r"
      0x0a -> "\n"
      w | w < 0x20 || w == 0x7f -> BC.pack ("\\x" <> pad (showHex w ""))
      w -> BS.singleton w
    pad s = replicate (2 - length s) '0' <> s

-- * Ranges

ranges :: [TestTree]
ranges =
  [ testCase "a range is the cells between its ends, in either order" $ do
      r <- formatted "abcdef\r\nghijkl" $ \t -> do
        fwd <- formatRange t plain (Span @'Active (Point 2 0) (Point 1 1))
        rev <- formatRange t plain (Span @'Active (Point 1 1) (Point 2 0))
        pure (fwd, rev)
      r @?= (Just "cdef\ngh", Just "cdef\ngh"),
    testCase "a block is the rectangle between its corners" $ do
      r <- formatted "abcdef\r\nghijkl\r\nmnopqr" $ \t ->
        formatRange t plain (Block @'Active (Point 4 2) (Point 1 0))
      r @?= Just "bcde\nhijk\nnopq",
    testCase "whole rows are whole in either order" $ do
      r <- formatted "abcdef\r\nghijkl\r\nmnopqr" $ \t ->
        (,) <$> formatRange t plain (WholeRows @'Active 2 0) <*> formatRange t plain (WholeRows @'Active 0 2)
      r @?= (Just "abcdef\nghijkl\nmnopqr", Just "abcdef\nghijkl\nmnopqr"),
    testCase "whole rows of the grid reach into the scrollback" $ do
      r <- formatted scrolled $ \t -> do
        grid' <- formatRange t plain (WholeRows @'Grid 1 3)
        history <- formatRange t plain (WholeRows @'History 0 0)
        active <- formatRange t plain (WholeRows @'Active 0 0)
        pure (grid', history, active)
      r @?= (Just "line 2\nline 3\nline 4", Just "line 1", Just "line 4"),
    testCase "an end outside its coordinate system is Nothing" $ do
      r <- formatted scrolled $ \t -> do
        row <- formatRange t plain (WholeRows @'Active 0 5)
        col <- formatRange t plain (Span @'Active (Point 0 0) (Point 20 0))
        history <- formatRange t plain (WholeRows @'History 0 3)
        pure (row, col, history)
      r @?= (Nothing, Nothing, Nothing),
    testCase "the viewport is what is scrolled into view" $ do
      r <- formatted scrolled $ \t -> formatRange t plain (WholeRows @'Viewport 0 1)
      r @?= Just "line 4\nline 5"
  ]
  where
    scrolled = maybe "" encodeUtf8 (lookup "scrollback" streams)

-- * Options

options :: [TestTree]
options =
  [ testCase "an empty terminal formats as nothing, in HTML an empty block" $ do
      r <- formatted "" $ \t -> traverse (format t . formatOptions) [minBound .. maxBound]
      r @?= ["", "", "<div style=\"font-family: monospace; white-space: pre;\"></div>"],
    testCase "trim drops trailing spaces; without it they stay" $ do
      r <- formatted "a  \r\nb" $ \t -> do
        trimmed <- format t plain
        off <- format t plain {trim = False}
        pure (trimmed, off)
      r @?= ("a\nb", "a  \nb"),
    testCase "a format changes nothing in the terminal" $ do
      r <- formatted "abc\r\nd" $ \t -> do
        before <- (,) <$> getT t CursorX <*> getT t CursorY
        traverse_ (format t . formatOptions) [minBound .. maxBound]
        _ <- formatRange t plain (WholeRows @'Active 0 1)
        after <- (,) <$> getT t CursorX <*> getT t CursorY
        again <- feed t "e" *> format t plain
        pure (before == after, again)
      r @?= (True, "abc\nde"),
    testCase "the cursor extra places the cursor; without it, none" $ do
      r <- formatted "ab\r\ncd\ESC[1;2H" $ \t -> do
        with' <- format t (formatOptions Vt) {extras = noTerminalExtras {screen = noScreenExtras {cursor = True}}}
        without <- format t (formatOptions Vt)
        pure ("\ESC[1;2H" `BS.isInfixOf` with', "\ESC[1;2H" `BS.isInfixOf` without)
      r @?= (True, False),
    -- Regression 10 (DESIGN.md §4.8): at libghostty-vt 1.0 a new formatter
    -- starts with the palette, cursor-style and hyperlink extras on, where a
    -- zeroed options struct had none. Each option is now set on every
    -- formatter, so no extras means none.
    testCase "regression 10: no extras is none, whatever the library's defaults" $ do
      let vt = formatOptions Vt
          state = "\ESC]4;1;rgb:12/34/56\ESC\\\ESC[1m\ESC]8;;https://example.com/\ESC\\"
      bare <- formatted "ab" $ \t -> format t vt
      r <- formatted ("ab" <> state) $ \t -> do
        without <- format t vt
        palette <- format t vt {extras = noTerminalExtras {palette = True}}
        pure (without, "\ESC]4;" `BS.isInfixOf` palette)
      r @?= (bare, True),
    testCase "the pwd extra carries OSC 7" $ do
      r <- formatted "\ESC]7;file:///home\ESC\\x" $ \t -> do
        out <- format t (formatOptions Vt) {extras = noTerminalExtras {pwd = True}}
        pure ("\ESC]7;file:///home" `BS.isInfixOf` out)
      r @?= True
  ]

-- * Helpers

-- | Plain text with the defaults.
plain :: FormatOptions
plain = formatOptions Plain

-- | Feed a fresh 20×5 terminal and run the action on it.
formatted :: ByteString -> (forall s. Terminal s -> Eff '[Ghostty, IOE] a) -> IO a
formatted bytes k = ghostty $ withTerminal config $ \t -> feed t bytes *> k t

-- | A 20×5 terminal: narrow enough to wrap, short enough to scroll.
config :: TerminalConfig
config = terminalConfig (grid 20 5)
