-- | T4's layout, and a grid painted on the host (PLAN.md M2.9, DESIGN.md
-- §5.6, §11.2).
--
-- Each golden is a document laid out and painted by tendril, fed to a fresh
-- libghostty terminal after any text before it, and dumped as the
-- formatter's screen, the styled runs and the cursor ("Test.Tendril.Render.Host"),
-- in @test/golden/render/<name>.screen@. Goldens are read before they are
-- accepted (@--accept@), and the commit says why the output is right (§11.1).
--
-- The property is the goldens' rule in general: whatever document is laid
-- out, at whatever width and in either width mode, the host shows the grid,
-- cell for cell, and its cursor.
module Test.Tendril.Render (tests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Tendril.Render.Doc
import Tendril.Render.Grid (Cell (..), Glyph (..), Grid, GridPos (..), grid, rowText)
import Tendril.Render.Layout (layout, sanitize)
import Tendril.Render.Style (Color (..), Style (..), Underline (..), plain)
import Tendril.Term.Output (paint)
import Tendril.Text.Units (fromOffset, offset)
import Tendril.Text.Width (WidthMode (..))
import Test.QuickCheck (Gen, Property, arbitrary, choose, counterexample, elements, forAllShrink, frequency, ioProperty, listOf, oneof, resize, shrink, shrinkList, (===))
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
import Test.Tendril.Render.Host (Host (cursor), describe, dump, host, hostCells)
import Test.Tendril.Text.Gen (genText)

tests :: TestTree
tests =
  testGroup
    "Render"
    [ testGroup "goldens" (map golden goldens),
      testGroup "layout" layoutCases,
      testGroup "grid" gridCases,
      testGroup "output" outputCases,
      adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 100)) $
        testProperty "the host shows the grid" hostShowsGrid
    ]

-- * Goldens

-- | A golden: its name, the host's width mode, columns and rows, the bytes
-- before the paint, and the document.
data Golden = Golden String WidthMode Word16 Word16 ByteString Doc

golden :: Golden -> TestTree
golden (Golden name mode cols rows before d) =
  goldenVsString name ("test/golden/render/" <> name <> ".screen") $
    dump <$> host mode cols rows (before <> paint (layout mode (offset (fromIntegral cols)) d))

prompt :: Style
prompt = plain {bold = True, foreground = Palette 2}

goldens :: [Golden]
goldens =
  [ -- A prompt and an input, the cursor after it.
    Golden "prompt" Clusters 20 4 "" (doc [Line [Run prompt "> ", Run plain "print(1)", Cursor]]),
    -- Output above the live region stays: the region starts on the row
    -- the host's cursor is on.
    Golden "after-output" Clusters 20 5 "earlier output\r\nmore\r\n" $
      doc [Line [Run prompt ">>> ", Run plain "x = ", Cursor, Run plain "1"]],
    -- Every attribute, the three kinds of colour, each underline, and a link.
    Golden "styles" Clusters 30 8 "" $
      doc
        [ Line [Run plain {bold = True} "bold", Run plain " ", Run plain {faint = True} "faint", Run plain " ", Run plain {italic = True} "italic"],
          Line [Run plain {blink = True} "blink", Run plain " ", Run plain {inverse = True} "inv", Run plain " ", Run plain {invisible = True} "hid", Run plain " ", Run plain {strikethrough = True} "strike", Run plain " ", Run plain {overline = True} "over"],
          Line (intersperse (Run plain " ") [Run plain {underline = u} (T.toLower (show u)) | u <- [Single ..]]),
          Line [Run plain {foreground = Palette 1} "p1", Run plain " ", Run plain {foreground = Palette 208, background = Palette 4} "p208/4", Run plain " ", Run plain {foreground = Rgb 1 2 3, background = Rgb 250 251 252} "rgb"],
          Line [Run plain {underline = Curly, underlineColor = Rgb 255 0 0} "red curl", Run plain " ", Run plain {underline = Single, underlineColor = Palette 3} "p3"],
          Line [Run plain {link = Just "https://example.com/"} "link", Run plain " after ", Run plain {link = Just "https://example.com/", bold = True} "bold link"]
        ],
    -- A long line wraps; a wide character that does not fit the last column
    -- leaves it blank; a cursor after a full row starts a row of its own.
    Golden "wraps" Clusters 10 8 "" $
      doc
        [ text "0123456789abcdefghijklm",
          text "123456789\x5BBD\&x",
          Line [Run plain "0123456789", Cursor]
        ],
    -- Clusters as one cell or two, in grapheme-cluster mode.
    Golden "clusters" Clusters 20 4 "" (doc [text "e\x0301 \x1F469\x200D\x1F4BB \x1F1FA\x1F1F8 \x2764\xFE0F \x1F44D\x1F3FD|", Line [Run plain "\x5BBD", Cursor, Run plain "\x5BBD"]]),
    -- The same text without the mode: each code point is drawn alone.
    Golden "codepoints" CodePoints 20 4 "" (doc [text "e\x0301 \x1F469\x200D\x1F4BB \x1F1FA\x1F1F8 \x2764\xFE0F \x1F44D\x1F3FD|", Line [Run plain "\x5BBD", Cursor, Run plain "\x5BBD"]]),
    -- Controls are drawn, never sent; a tab moves to the next stop; a
    -- newline in a run breaks the line.
    Golden "controls" Clusters 24 5 "" (doc [Line [Run plain "a\tb\tc", Run plain {bold = True} "\ESC[2J\DEL\x85\x200B!"], Line [Run plain "one\ntwo", Cursor]]),
    -- A menu under the cursor of a multi-line input, moved left to fit; the
    -- box cuts a wide character in two, and the grid grows to hold it.
    Golden "menu" Clusters 16 8 "" $
      Doc
        { lines = [Line [Run prompt "> ", Run plain "\x5BBD\x5BBD = 1"], Line [Run prompt ". ", Run plain "  print(x.lo", Cursor, Run plain ")"]],
          boxes =
            [ Box
                { anchor = BelowCursor,
                  fill = plain {background = Palette 8},
                  body = [Line [Run plain {background = Palette 8, bold = True} "lower"], styled plain {background = Palette 8} "lookup", styled plain {background = Palette 8} "loads"]
                },
              Box {anchor = At 0 (offset 3), fill = plain {inverse = True}, body = [text "Z", text ""]}
            ]
        },
    -- No cursor: hidden.
    Golden "no-cursor" Clusters 10 3 "" (doc [text "status"]),
    -- An empty document draws nothing.
    Golden "empty" Clusters 10 3 "above\r\n" (doc [])
  ]

-- * Layout

rowsOf :: Grid -> [Text]
rowsOf g = map rowText (toList g.rows)

glyphs :: Grid -> [[Glyph]]
glyphs g = map (map (.glyph) . toList) (toList g.rows)

layoutCases :: [TestTree]
layoutCases =
  [ testCase "an empty line is one empty row" $
      rowsOf (layout Clusters (offset 3) (doc [text ""])) @?= ["   "],
    testCase "a line that fills its row adds none, without a cursor after it" $
      rowsOf (layout Clusters (offset 3) (doc [text "abc"])) @?= ["abc"],
    testCase "a cursor after a full row is at the start of the next" $
      (layout Clusters (offset 3) (doc [Line [Run plain "abc", Cursor]])).cursor @?= Just (GridPos 1 (offset 0)),
    testCase "a cursor before a wide character that wraps is where it lands" $
      (layout Clusters (offset 3) (doc [Line [Run plain "ab", Cursor, Run plain "\x5BBD"]])).cursor @?= Just (GridPos 1 (offset 0)),
    testCase "a cursor inside a cluster is at its start" $
      (layout Clusters (offset 5) (doc [Line [Run plain "ae", Cursor, Run plain "\x0301"]])).cursor @?= Just (GridPos 0 (offset 1)),
    testCase "the first cursor is the cursor" $
      (layout Clusters (offset 5) (doc [Line [Run plain "a", Cursor], Line [Cursor]])).cursor @?= Just (GridPos 0 (offset 1)),
    testCase "a combining mark in a run of its own joins the cell before, in its style" $
      glyphs (layout Clusters (offset 2) (doc [Line [Run plain "e", Run plain {bold = True} "\x0301"]]))
        @?= [[Narrow "e\x0301", Blank]],
    testCase "a tab moves to the next multiple of eight, and stops at the row's end" $
      rowsOf (layout Clusters (offset 10) (doc [text "ab\tc\td"])) @?= ["ab      c ", "d         "],
    testCase "a wide character on a grid one column wide is blank" $
      rowsOf (layout Clusters (offset 1) (doc [text "\x5BBD\&a"])) @?= [" ", "a"],
    testCase "one column wide in grapheme-cluster mode, a cluster is its first code point, as libghostty-vt draws it" $
      map (\m -> rowsOf (layout m (offset 1) (doc [text "e\x0301"]))) [Clusters, CodePoints] @?= [["e"], ["e\x0301"]],
    testCase "a cluster wide before a VS15 makes it narrow wraps from the last column" $
      rowsOf (layout Clusters (offset 3) (doc [text "ab\x231A\xFE0E"])) @?= ["ab ", "\x231A\xFE0E  "],
    testCase "a cluster's leading zero-width code points are not drawn" $
      rowsOf (layout Clusters (offset 3) (doc [text "\x11A8\x0903"])) @?= ["\x0903  "],
    testCase "a width of 0 is 1" $
      fromOffset (layout Clusters (offset 0) (doc [])).width @?= 1,
    testCase "controls are drawn visibly, and the host's other controls removed" $
      sanitize "\NUL\ESC\DEL\x9B\x2028\x202E\xFEFF" @?= "^@^[^?<U+009B>",
    testCase "a box at the right edge moves left to fit" $
      rowsOf (layout Clusters (offset 6) Doc {lines = [text "abcdef"], boxes = [Box (At 0 (offset 5)) plain [text "XY"]]}) @?= ["abcdXY"],
    testCase "a box wider than the grid is cut to it" $
      rowsOf (layout Clusters (offset 4) Doc {lines = [], boxes = [Box (At 1 (offset 0)) plain [text "abcdef"]]}) @?= ["    ", "abcd"],
    testCase "a box below a missing cursor goes below the last row" $
      rowsOf (layout Clusters (offset 4) Doc {lines = [text "ab"], boxes = [Box BelowCursor plain [text "m"]]}) @?= ["ab  ", "m   "],
    testCase "a box over half a wide character blanks the other half" $
      rowsOf (layout Clusters (offset 6) Doc {lines = [text "\x5BBD\x5BBD\x5BBD"], boxes = [Box (At 0 (offset 1)) plain [text "ab"]]})
        @?= [" ab \x5BBD"]
  ]

-- * Grid

gridCases :: [TestTree]
gridCases =
  [ testCase "a glyph the host would draw otherwise is blank" $
      glyphs (grid Clusters (offset 6) [[c (Narrow "\ESC"), c (Narrow "ab"), c (Narrow "\x5BBD"), c (Wide "a"), c WideTail, c (Narrow "")]] Nothing)
        @?= [[Blank, Blank, Blank, Blank, Blank, Blank]],
    testCase "a wide glyph without its tail, or past the width, is blank" $
      glyphs (grid Clusters (offset 3) [[c (Wide "\x5BBD"), c (Narrow "a"), c (Wide "\x5BBD"), c WideTail]] Nothing)
        @?= [[Blank, Narrow "a", Blank]],
    testCase "a cluster is one glyph only in grapheme-cluster mode" $
      [glyphs (grid m (offset 2) [[c (Wide "\x1F1FA\x1F1F8"), c WideTail]] Nothing) | m <- [Clusters, CodePoints]]
        @?= [[[Wide "\x1F1FA\x1F1F8", WideTail]], [[Blank, Blank]]],
    testCase "a variation selector the host ignores is dropped" $
      [glyphs (grid m (offset 4) [[c (Narrow t)]] Nothing) | m <- [Clusters, CodePoints], t <- ["a\xFE0F", "\x2764\xFE0E", "\x2764\xFE0F\xFE0F", "\x1F44D\xFE0E"]]
        @?= map
          (\g -> [[g, Blank, Blank, Blank]])
          [Narrow "a", Narrow "\x2764\xFE0E", Blank, Narrow "\x1F44D\xFE0E", Narrow "a", Narrow "\x2764\xFE0E", Narrow "\x2764\xFE0F\xFE0F", Blank],
    testCase "a glyph the host would join to the cell before is blank" $
      glyphs (grid Clusters (offset 4) [[c (Narrow "a"), c (Narrow "\x0903"), c Blank, c (Narrow "\x0903")]] Nothing)
        @?= [[Narrow "a", Blank, Blank, Blank]],
    testCase "the host never joins a code point below U+0100 to the cell before" $
      glyphs (grid Clusters (offset 2) [[c (Narrow "\x0600"), c (Narrow "x")]] Nothing) @?= [[Narrow "\x0600", Narrow "x"]],
    testCase "a cluster wide before a VS15 makes it narrow is blank in the last column" $
      glyphs (grid Clusters (offset 2) [[c (Narrow "a"), c (Narrow "\x231A\xFE0E")]] Nothing) @?= [[Narrow "a", Blank]],
    testCase "one column wide in grapheme-cluster mode, a cluster is its first code point" $
      glyphs (grid Clusters (offset 1) [[c (Narrow "e\x0301")]] Nothing) @?= [[Narrow "e"]],
    testCase "a cursor outside the grid is none" $
      (grid Clusters (offset 2) [[]] (Just (GridPos 0 (offset 2)))).cursor @?= Nothing
  ]
  where
    c g = Cell g plain

-- * Output

outputCases :: [TestTree]
outputCases =
  [ testCase "a link's control characters are percent-encoded" $ do
      let bytes = paint (layout Clusters (offset 4) (doc [styled plain {link = Just "x:\ESC]0;t\a"} "a"]))
      "\ESC]8;;x:%1B]0;t%07\ESC\\" `BS.isInfixOf` bytes @?= True,
    testCase "a row's trailing plain blanks are erased, not written" $
      paint (layout Clusters (offset 8) (doc [text "ab"])) @?= "\ESC[0mab\ESC[K\ESC[?25l",
    testCase "a row drawn to its last column is not erased" $
      paint (layout Clusters (offset 2) (doc [text "ab"])) @?= "\ESC[0mab\ESC[?25l"
  ]

-- * The property

-- | Lay a document out, paint it on a host of its width, and read the host
-- back: its cells are the grid's, and its cursor is the grid's.
hostShowsGrid :: Property
hostShowsGrid =
  forAllShrink ((,,) <$> elements [Clusters, CodePoints] <*> choose (1, 24) <*> genDoc) shrinkCase $ \(mode, cols, d) ->
    let g = layout mode (offset (fromIntegral cols)) d
        rows = fromIntegral (length g.rows) + 1
        ours = map (map noEmptyLink . toList) (toList g.rows)
     in ioProperty $ do
          h <- host mode cols rows (paint g)
          let theirs = take (length ours) (hostCells h)
              wrong =
                [ show r <> "," <> show c <> ": " <> cellText a <> " shown as " <> cellText b
                | (r, ra, rb) <- zip3 [0 :: Int ..] ours theirs,
                  (c, a, b) <- zip3 [0 :: Int ..] ra rb,
                  a /= b
                ]
          pure
            . counterexample (show (paint g))
            . counterexample (toString (unlines wrong))
            $ (wrong, h.cursor) === ([], g.cursor)
  where
    -- An empty link is none: OSC 8 ends a link with an empty URI.
    noEmptyLink cell = if cell.style.link == Just "" then cell {style = cell.style {link = Nothing}} else cell
    cellText c = show c.glyph <> " " <> describe c.style

shrinkCase :: (WidthMode, Word16, Doc) -> [(WidthMode, Word16, Doc)]
shrinkCase (m, cols, d) =
  [(m, cols', d) | cols' <- shrink cols, cols' >= 1]
    <> [(m, cols, Doc ls bs) | (ls, bs) <- shrink2 d]
  where
    shrink2 (Doc ls bs) =
      [(ls', bs) | ls' <- shrinkList shrinkLine ls]
        <> [(ls, bs') | bs' <- shrinkList shrinkBox bs]
    shrinkLine (Line is) = Line <$> shrinkList shrinkInline is
    shrinkInline = \case
      Cursor -> []
      Run st t -> [Run plain t | st /= plain] <> [Run st (T.pack t') | t' <- shrinkList (const []) (T.unpack t)]
    shrinkBox b = [b {body = body'} | body' <- shrinkList shrinkLine b.body]

genDoc :: Gen Doc
genDoc = Doc <$> resize 4 (listOf genLine) <*> resize 2 (listOf genBox)

genLine :: Gen Line
genLine = Line <$> resize 4 (listOf (frequency [(6, Run <$> genStyle <*> genRunText), (1, pure Cursor)]))

genRunText :: Gen Text
genRunText = oneof [genText, T.pack <$> listOf (elements "ab \t\x5BBD\x0301"), pure "\n"]

genBox :: Gen Box
genBox =
  Box
    <$> oneof [pure BelowCursor, At <$> choose (0, 6) <*> (offset <$> choose (0, 30))]
    <*> genStyle
    <*> resize 3 (listOf genLine)

genStyle :: Gen Style
genStyle = frequency [(2, pure plain), (3, styled')]
  where
    styled' = do
      fg <- genColor
      bg <- genColor
      ul <- genColor
      u <- frequency [(4, pure NoUnderline), (1, elements [minBound .. maxBound])]
      l <- frequency [(4, pure Nothing), (1, Just <$> elements ["https://example.com/", "file:///tmp/a;b", ""])]
      Style fg bg ul <$> flag <*> flag <*> flag <*> pure u <*> flag <*> flag <*> flag <*> flag <*> flag <*> pure l
    flag = frequency [(4, pure False), (1, pure True)]
    genColor = frequency [(3, pure Default), (1, Palette <$> arbitrary), (1, Rgb <$> arbitrary <*> arbitrary <*> arbitrary)]
