{-# LANGUAGE StrictData #-}

-- | Laying a "Tendril.Render.Doc" out at the host's width (DESIGN.md §5.6):
-- its lines wrapped into rows of cells, the cursor placed, and its boxes
-- drawn over them.
--
-- The layout measures text as the host does ("Tendril.Text.Width.pieces",
-- in the mode the host is in), so the grid is what the host shows when sent
-- it. Each line is measured whole, across its runs, since the host joins a
-- cluster whatever its styles: a combining mark in a run of its own belongs
-- to the cell before it, in that cell's style.
--
-- A line wraps where its next piece does not fit. A wide piece that does not
-- fit in the last column leaves it blank and starts the next row, as a
-- terminal does. The cursor is drawn where the text after it starts, so a
-- cursor after a line that fills its last row is at the start of a row of its
-- own, and a cursor inside a cluster is at the cluster's start.
module Tendril.Render.Layout
  ( layout,
    sanitize,
  )
where

import Data.Char (isControl)
import Data.List (unsnoc)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Numeric (showHex)
import Tendril.Render.Doc (Anchor (..), Box (..), Doc (..), Inline (..), Line (..))
import Tendril.Render.Grid (Cell (..), Glyph (..), Grid, GridPos (..), blank, grid)
import Tendril.Render.Style (Style, plain)
import Tendril.Text.Unicode.Tables (GraphemeBreak (..), graphemeBreak)
import Tendril.Text.Units (Offset, Unit (..), fromOffset, offset)
import Tendril.Text.Width (WidthMode (..), peakWidth, pieces)

-- $setup
-- >>> import Tendril.Render.Doc (doc, text, cursorHere)
-- >>> import Tendril.Render.Grid (rowText)
-- >>> let rowsOf g = map rowText (toList g.rows)

-- | The grid a document is drawn in, at a width (1 if it is 0), for a host
-- that measures text in the mode.
--
-- >>> let g = layout Clusters (offset 4) (doc [text "abcdef", text "\x5BBD\x5BBD\x5BBD" <> cursorHere])
-- >>> rowsOf g
-- ["abcd","ef  ","\23485\23485","\23485  "]
-- >>> g.cursor
-- Just (GridPos {row = 3, col = Offset 2})
layout :: WidthMode -> Offset 'Column -> Doc -> Grid
layout mode w0 d = grid mode (offset w) (map toList (toList withBoxes)) cursor
  where
    w = fromOffset (max (offset 1) w0)
    (rows, cursor) = layoutLines mode w (concatMap logical d.lines)
    padded = Seq.fromList (map (padRow w) rows)
    withBoxes = foldl' (drawBox mode w cursor) padded d.boxes

-- | A logical line: the 'Doc' lines with each newline in a run made a line
-- break, and each run's text sanitized.
data Item = Chars Style Text | Mark

logical :: Line -> [[Item]]
logical (Line inlines) = go [] inlines
  where
    go acc = \case
      [] -> [reverse acc]
      Cursor : rest -> go (Mark : acc) rest
      Run s t : rest -> case T.splitOn "\n" t of
        [] -> go acc rest
        t0 : more -> lineBreaks (Chars s (sanitize t0) : acc) s more rest
    lineBreaks acc s more rest = case more of
      [] -> go acc rest
      next : more' -> reverse acc : lineBreaks [Chars s (sanitize next)] s more' rest

-- | A run's text as the layout draws it: a tab and a newline kept (the
-- layout moves to the next tab stop, or the next line); any other C0
-- control and DEL in caret notation; a C1 control as its code point; and
-- every other code point the host would treat as a control (a zero-width
-- space, a line or paragraph separator, a bidi control) removed. What is left
-- has no character a terminal acts on.
--
-- >>> sanitize "a\ESC[31m\DEL\x85\x200B\tb"
-- "a^[[31m^?<U+0085>\tb"
sanitize :: Text -> Text
sanitize = T.concatMap visible
  where
    visible c
      | c == '\t' || c == '\n' = T.singleton c
      | c == '\DEL' = "^?"
      | c < ' ' = T.pack ['^', chr (ord c + 64)]
      | isControl c = "<U+" <> T.justifyRight 4 '0' (T.toUpper (toText (showHex (ord c) ""))) <> ">"
      | graphemeBreak c `elem` [Control, CR, LF] = ""
      | otherwise = T.singleton c

-- | The rows of the lines, and the first cursor among them.
layoutLines :: WidthMode -> Word -> [[Item]] -> ([[Cell]], Maybe GridPos)
layoutLines mode w = go 0 Nothing
  where
    go row cur = \case
      [] -> ([], cur)
      l : ls ->
        let (rows, cur') = layoutLine mode w (isNothing cur) l
            here = fmap (\p -> p {row = p.row + row}) cur'
            (rest, final) = go (row + fromIntegral (length rows)) (cur <|> here) ls
         in (rows <> rest, final)

-- | The state of a line's layout: the rows done, the row being filled
-- (reversed), its column, and the cursor once placed or while it waits for
-- the next cell.
data Fill = Fill
  { done :: [[Cell]],
    current :: [Cell],
    col :: Word,
    pending :: Bool,
    placed :: Maybe GridPos
  }

-- | One logical line's rows, at a width, and its cursor, if it is wanted and
-- the line has one; the cursor's row is counted from the line's first.
layoutLine :: WidthMode -> Word -> Bool -> [Item] -> ([[Cell]], Maybe GridPos)
layoutLine mode w wantCursor items = finish (foldl' place start (zip3 starts styles ps))
  where
    segments = [(s, t) | Chars s t <- items]
    whole = T.concat (map snd segments)
    -- A tab is the host's cursor motion, not a piece of text: the text
    -- between tabs is measured apart, as the host does.
    ps = intercalate [("\t", offset 0)] (map (pieces mode) (T.splitOn "\t" whole))
    starts = scanl (+) 0 (map (T.length . fst) ps)
    styles = pieceStyles (concatMap (\(s, t) -> replicate (T.length t) s) segments) ps
    -- The code point the first mark is before.
    mark = if wantCursor then firstMark 0 items else Nothing
    firstMark n = \case
      [] -> Nothing
      Mark : _ -> Just n
      Chars _ t : rest -> firstMark (n + T.length t) rest
    start = Fill {done = [], current = [], col = 0, pending = False, placed = Nothing}

    place f (k, s, (t, cw)) =
      let reached = maybe False (\m -> k <= m && m < k + T.length t) mark
          f' = if reached then f {pending = True} else f
       in case fromOffset cw of
            _ | t == "\t" -> tab s (wrapIfFull f')
            0 -> f'
            1
              -- The host places a cluster by the width it has while it
              -- reads it, so one that is wide before a VS15 makes it narrow
              -- wraps from the last column.
              | f'.col + 1 == w,
                f'.col > 0,
                mode == Clusters,
                peakWidth t >= offset 2 ->
                  emit [Cell (Narrow t) s] (newRow f' {current = blank plain : f'.current, col = w})
              | otherwise -> emit [Cell (Narrow t) s] (wrapIfFull f')
            _
              | w == 1 -> emit [blank s] (wrapIfFull f')
              | f'.col + 2 > w ->
                  let filled = if f'.col < w then f' {current = blank plain : f'.current, col = f'.col + 1} else f'
                   in emit wide (newRow filled)
              | otherwise -> emit wide f'
              where
                wide = [Cell (Wide t) s, Cell WideTail s]

    tab s f = emit (replicate (fromIntegral (min (8 - f.col `mod` 8) (w - f.col))) (blank s)) f

    -- Place the cursor if it waits, then the cells.
    emit cs f =
      let f' = settle f
       in f' {current = reverse cs <> f'.current, col = f'.col + fromIntegral (length cs)}
    settle f
      | f.pending = f {pending = False, placed = Just (GridPos (fromIntegral (length f.done)) (offset f.col))}
      | otherwise = f
    wrapIfFull f = if f.col >= w then newRow f else f
    newRow f = f {done = reverse f.current : f.done, current = [], col = 0}

    finish f0 =
      let f1 = if maybe False (>= T.length whole) mark && isNothing f0.placed then f0 {pending = True} else f0
          f2 = if f1.pending then settle (wrapIfFull f1) else f1
       in (reverse (reverse f2.current : f2.done), f2.placed)

-- | Each piece's style: its first code point's.
pieceStyles :: [Style] -> [(Text, a)] -> [Style]
pieceStyles ss = \case
  [] -> []
  (t, _) : rest -> fromMaybe plain (viaNonEmpty head ss) : pieceStyles (drop (T.length t) ss) rest

padRow :: Word -> [Cell] -> Seq Cell
padRow w cs = Seq.fromList (take (fromIntegral w) (cs <> repeat (blank plain)))

-- | Draw a box over the rows, growing them downwards to hold it.
drawBox :: WidthMode -> Word -> Maybe GridPos -> Seq (Seq Cell) -> Box -> Seq (Seq Cell)
drawBox mode w cursor rows b = foldl' overlay grown (zip [top ..] boxRows)
  where
    unbounded = maxBound `div` 2
    boxLines = map (fst . layoutLine mode unbounded False . concat . logical . noBreaks) b.body
    -- A line without breaks, at a width nothing reaches, is one row.
    natural = map (concat . take 1) boxLines
    bw = min w (fromIntegral (foldl' max 0 (map length natural)))
    boxRows = map (fitBox . cut) natural
    cut cs = case splitAt (fromIntegral bw) cs of
      (kept, _) -> case unsnoc kept of
        Just (initial, Cell (Wide _) _) -> initial <> [blank b.fill]
        _ -> kept
    fitBox cs = cs <> replicate (fromIntegral bw - length cs) (blank b.fill)
    (top, left0) = case b.anchor of
      At r c -> (r, fromOffset c)
      BelowCursor -> case cursor of
        Just p -> (p.row + 1, fromOffset p.col)
        Nothing -> (fromIntegral (Seq.length rows), 0)
    left = fromIntegral (min left0 (w - bw)) :: Int
    height = fromIntegral top + length boxRows
    grown = rows <> Seq.replicate (max 0 (height - Seq.length rows)) (padRow w [])
    overlay rs (r, cs) =
      Seq.adjust' (\row -> Seq.take left row <> Seq.fromList cs <> Seq.drop (left + length cs) row) (fromIntegral r) rs
    noBreaks (Line is) = Line [Run s (T.replace "\n" "" t) | Run s t <- is]
