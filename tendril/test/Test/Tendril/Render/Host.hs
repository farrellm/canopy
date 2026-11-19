-- | libghostty as the host, for T4's goldens and properties (DESIGN.md
-- §11.2): tendril's VT is fed to a headless terminal, and what the terminal
-- then shows is read back, as the formatter's plain text and as cells.
-- Rendering goldens are screens, not bytes: they fail only when what the user
-- sees changes.
module Test.Tendril.Render.Host
  ( Host (..),
    host,
    hostCells,
    dump,
    describe,
  )
where

import Data.Text qualified as T
import Data.Vector qualified as V
import Ghostty.Effectful.Cell qualified as G
import Ghostty.Effectful.Color qualified as G
import Ghostty.Effectful.Format (Format (..), format, formatOptions)
import Ghostty.Effectful.Point (Point (..))
import Ghostty.Effectful.Render (FrameData (..), allRows, cells, getF, withFrame, withRenderState)
import Ghostty.Effectful.Render.Keys (Cursor (..), CursorPosition (..))
import Ghostty.Effectful.Style qualified as G
import Ghostty.Effectful.Terminal (GridSize (..), extent, feed, terminalConfig, withTerminal)
import Tendril.Render.Grid (Cell (..), Glyph (..), GridPos (..))
import Tendril.Render.Style (Color (..), Style (..), Underline (..), plain)
import Tendril.Text.Units (fromOffset, offset)
import Tendril.Text.Width (WidthMode (..))
import Test.Tendril.Text.Oracle (oracle)

-- | What the host shows.
data Host = Host
  { -- | The formatter's plain text of the screen, scrollback included.
    text :: Text,
    -- | Every row of the screen, as ghostty's cells.
    rows :: [[G.Cell]],
    -- | The cursor, if the modes show it.
    cursor :: Maybe GridPos
  }

-- | A fresh terminal of @cols@ by @rows@, in grapheme-cluster mode (2027) for
-- 'Clusters', fed the bytes.
host :: WidthMode -> Word16 -> Word16 -> ByteString -> IO Host
host mode cols rows bytes = do
  size <- maybe (fail "a zero extent") pure (GridSize <$> extent cols <*> extent rows)
  oracle $ withTerminal (terminalConfig size) $ \t -> do
    _ <- feed t (if mode == Clusters then "\ESC[?2027h" else "")
    _ <- feed t bytes
    txt <- decodeUtf8 <$> format t (formatOptions Plain)
    withRenderState t $ \rs -> withFrame rs $ \f -> do
      cs <- traverse (fmap V.toList . cells) =<< allRows f
      c <- getF f RenderCursor
      let at p = GridPos (fromIntegral p.point.row) (offset (fromIntegral p.point.col))
      pure Host {text = txt, rows = cs, cursor = if c.visible then at <$> c.position else Nothing}

-- | The host's cells as tendril's, to compare with a grid. A space is blank,
-- as a blank is drawn as one, and the empty last column a wide character
-- left is blank.
hostCells :: Host -> [[Cell]]
hostCells h = map (map cell) h.rows
  where
    cell c = Cell (glyph c) (style c)
    glyph c = case c.width of
      G.Wide -> Wide c.text
      G.SpacerTail -> WideTail
      G.SpacerHead -> Blank
      G.Narrow
        | c.text `elem` ["", " "] -> Blank
        | otherwise -> Narrow c.text
    style c =
      Style
        { foreground = color c.style.foreground,
          background = color c.style.background,
          underlineColor = color c.style.underlineColor,
          bold = c.style.bold,
          faint = c.style.faint,
          italic = c.style.italic,
          underline = underline c.style.underline,
          blink = c.style.blink,
          inverse = c.style.inverse,
          invisible = c.style.invisible,
          strikethrough = c.style.strikethrough,
          overline = c.style.overline,
          link = c.hyperlink >>= (.uri)
        }
    color = \case
      G.DefaultColor -> Default
      G.PaletteColor n -> Palette n
      G.RgbColor (G.Rgb r g b) -> Rgb r g b
    underline = \case
      G.NoUnderline -> NoUnderline
      G.SingleUnderline -> Single
      G.DoubleUnderline -> Double
      G.CurlyUnderline -> Curly
      G.DottedUnderline -> Dotted
      G.DashedUnderline -> Dashed

-- | A golden's text: the screen as the formatter gives it, the width of each
-- row's cells, each run of cells on a row whose style is not plain, and the
-- cursor.
--
-- > screen
-- > > print(1)
-- > cells
-- > 0 1.111111111
-- > styles
-- > 0 0-1 bold fg=2
-- > cursor
-- > 0 10
dump :: Host -> LByteString
dump h =
  encodeUtf8 . T.unlines $
    ["screen"]
      <> T.lines h.text
      <> ["cells"]
      <> catMaybes (zipWith widths [0 :: Int ..] h.rows)
      <> ["styles"]
      <> concat (zipWith styleRuns [0 :: Int ..] (hostCells h))
      <> ["cursor", maybe "hidden" (\p -> show p.row <> " " <> show (fromOffset p.col)) h.cursor]
  where
    styleRuns r cs =
      [ show r <> " " <> span' a b <> " " <> describe s
      | (a, b, s) <- runs (zip [0 :: Int ..] (map (.style) cs)),
        s /= plain
      ]
    span' a b = if a == b then show a else show a <> "-" <> show b
    runs = \case
      [] -> []
      (i, s) : rest ->
        let (same, more) = span ((== s) . snd) rest
         in (i, maybe i fst (viaNonEmpty last same), s) : runs more

-- | A row's cells, one character each, if it has any but blanks: @1@ for a
-- narrow character, @2@ and @-@ for a wide one and its tail, @>@ for the
-- last column a wide character left, and @.@ for a blank.
widths :: Int -> [G.Cell] -> Maybe Text
widths r cs =
  let marks = T.dropWhileEnd (== '.') (T.pack (map mark cs))
   in if T.null marks then Nothing else Just (show r <> " " <> marks)
  where
    mark c = case c.width of
      G.Wide -> '2'
      G.SpacerTail -> '-'
      G.SpacerHead -> '>'
      G.Narrow -> if c.text `elem` ["", " "] then '.' else '1'

-- | A style's attributes, in words.
describe :: Style -> Text
describe s =
  unwords $
    [w | (True, w) <- [(s.bold, "bold"), (s.faint, "faint"), (s.italic, "italic"), (s.blink, "blink"), (s.inverse, "inverse"), (s.invisible, "invisible"), (s.strikethrough, "strike"), (s.overline, "overline")]]
      <> ["underline=" <> T.toLower (show s.underline) | s.underline /= NoUnderline]
      <> color "fg" s.foreground
      <> color "bg" s.background
      <> color "ul" s.underlineColor
      <> ["link=" <> l | Just l <- [s.link]]
  where
    color role = \case
      Default -> []
      Palette n -> [role <> "=" <> show n]
      Rgb r g b -> [role <> "=" <> show r <> "," <> show g <> "," <> show b]
