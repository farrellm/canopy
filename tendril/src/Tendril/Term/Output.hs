-- | The VT tendril sends the host (DESIGN.md §5.6): a "Tendril.Render.Grid"
-- drawn as cells, their styles as SGR and their links as OSC 8.
--
-- So far this is 'paint', a grid drawn whole from the top of the live
-- region; the diff from the previous grid, and synchronized output (2026)
-- around a frame, come next (PLAN.md M2.10).
module Tendril.Term.Output
  ( paint,
    sgr,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as B
import Data.Char (toUpper)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Numeric (showHex)
import Tendril.Render.Grid (Cell (..), Glyph (..), Grid, GridPos (..))
import Tendril.Render.Style (Color (..), Style (..), Underline (..), plain)
import Tendril.Text.Units (fromOffset)

-- $setup
-- >>> import Data.ByteString.Builder (toLazyByteString)
-- >>> import Tendril.Render.Style (Style (..), plain, Color (..))

-- | The grid drawn whole, from the host's cursor, which is at the first
-- column of the row the live region starts on. Each row is drawn below the
-- one before, scrolling the host if the region reaches its last row; a row's
-- trailing cells that are blank and plain are erased, not written. Then the
-- cursor is moved to the grid's cursor and shown, or hidden if the grid has
-- none. The host's style and link are reset first and left reset.
paint :: Grid -> ByteString
paint g = toStrict . B.toLazyByteString $ "\ESC[0m" <> rows <> place
  where
    rs = toList g.rows
    width = fromOffset g.width
    rows = mconcat (intersperse "\r\n" (map (drawRow width) rs))
    lastRow = fromIntegral (max 1 (length rs)) - 1 :: Word
    place = case g.cursor of
      Nothing -> "\ESC[?25l"
      Just p ->
        csiN (lastRow - p.row) 'A'
          <> "\r"
          <> csiN (fromOffset p.col) 'C'
          <> "\ESC[?25h"

-- | One row, starting and ending in the plain style.
drawRow :: Word -> Seq Cell -> Builder
drawRow width cs = drawn <> toPlain final <> erase
  where
    trailing = Seq.length (Seq.takeWhileR (== Cell Blank plain) cs)
    kept = toList (Seq.take (Seq.length cs - trailing) cs)
    (final, drawn) = foldl' cell (plain, mempty) kept
    cell (cur, acc) c = (c.style, acc <> transition cur c.style <> drawGlyph c.glyph)
    -- Erasing from the last column would erase that column: a row drawn to
    -- its end needs none.
    erase = if trailing > 0 && fromIntegral (length kept) < width then "\ESC[K" else mempty
    toPlain s = transition s plain

-- | What changes the host's style from one to the other: SGR when the
-- attributes or colours differ, OSC 8 when the link does.
transition :: Style -> Style -> Builder
transition from to = attrs <> links
  where
    attrs = if from {link = Nothing} == to {link = Nothing} then mempty else sgr to
    links = if nonEmptyLink from == nonEmptyLink to then mempty else osc8 (nonEmptyLink to)
    nonEmptyLink s = s.link >>= \l -> l <$ guard (not (T.null l))

-- | The SGR that sets a style's attributes and colours from any state: a
-- reset, then what the style has. Its link is OSC 8's, not SGR's.
--
-- >>> toLazyByteString (sgr plain {bold = True, foreground = Palette 1, background = Rgb 1 2 3})
-- "\ESC[0;1;38;5;1;48;2;1;2;3m"
sgr :: Style -> Builder
sgr s = "\ESC[" <> mconcat (intersperse ";" ("0" : params)) <> "m"
  where
    params =
      [p | (True, p) <- [(s.bold, "1"), (s.faint, "2"), (s.italic, "3")]]
        <> underline s.underline
        <> [p | (True, p) <- [(s.blink, "5"), (s.inverse, "7"), (s.invisible, "8"), (s.strikethrough, "9"), (s.overline, "53")]]
        <> color "38" s.foreground
        <> color "48" s.background
        <> color "58" s.underlineColor
    underline = \case
      NoUnderline -> []
      Single -> ["4"]
      Double -> ["4:2"]
      Curly -> ["4:3"]
      Dotted -> ["4:4"]
      Dashed -> ["4:5"]
    color role = \case
      Default -> []
      Palette n -> [role <> ";5;" <> B.word8Dec n]
      Rgb r g b -> [role <> ";2;" <> B.word8Dec r <> ";" <> B.word8Dec g <> ";" <> B.word8Dec b]

-- | Start a link, or end one. A URI's control characters are
-- percent-encoded, so none ends the sequence early.
osc8 :: Maybe Text -> Builder
osc8 uri = "\ESC]8;;" <> foldMap escape (maybe "" T.unpack uri) <> "\ESC\\"
  where
    escape c
      | c < ' ' || (c >= '\DEL' && c <= '\x9F') = foldMap percent (BS.unpack (TE.encodeUtf8 (T.singleton c)))
      | otherwise = B.charUtf8 c
    -- Upper-case hex digits, as RFC 3986 asks.
    percent b = "%" <> B.string7 (map toUpper (T.unpack (T.justifyRight 2 '0' (toText (showHex b "")))))

-- | A glyph's text; a blank is a space, and a tail is drawn by its head.
drawGlyph :: Glyph -> Builder
drawGlyph = \case
  Blank -> " "
  Narrow t -> text t
  Wide t -> text t
  WideTail -> mempty
  where
    text = B.byteString . TE.encodeUtf8

-- | @CSI n x@, or nothing when @n@ is 0.
csiN :: Word -> Char -> Builder
csiN n x
  | n == 0 = mempty
  | otherwise = "\ESC[" <> B.wordDec n <> B.char7 x
