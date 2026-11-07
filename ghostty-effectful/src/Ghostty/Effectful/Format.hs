-- | Formatting what a terminal holds as plain text, VT sequences or HTML
-- (DESIGN.md §4.8): the whole active screen, or a range of it.
--
-- The output is bytes in every format: UTF-8 text for 'Plain' and 'Html',
-- and a VT stream for 'Vt', which redraws the content with its styles and
-- hyperlinks when written to a terminal. A format reads the terminal as it
-- is at the call; nothing is kept between calls.
--
-- > txt  <- format t (formatOptions Plain)
-- > rows <- formatRange t (formatOptions Html) (WholeRows @'Grid 10 20)
module Ghostty.Effectful.Format
  ( -- * Formatting
    format,
    formatRange,

    -- * Ranges
    Range (..),

    -- * Options
    Format (..),
    FormatOptions (..),
    formatOptions,
    TerminalExtras (..),
    noTerminalExtras,
    ScreenExtras (..),
    noScreenExtras,
  )
where

import Effectful (Eff, (:>))
import Effectful.Dispatch.Static (unsafeEff_)
import Ghostty.Effectful.Internal.Effect (Ghostty, owned)
import Ghostty.Effectful.Internal.Format
  ( Format (..),
    FormatOptions (..),
    Range (..),
    ScreenExtras (..),
    TerminalExtras (..),
    formatOptions,
    formatTerminal,
    formatTerminalRange,
    noScreenExtras,
    noTerminalExtras,
  )
import Ghostty.Effectful.Internal.Terminal (Terminal)
import Ghostty.Effectful.Point (KnownCoord)

-- | The terminal's active screen, scrollback included.
format :: (Ghostty :> es) => Terminal s -> FormatOptions -> Eff es ByteString
format t o = do
  _ <- owned "format"
  unsafeEff_ (formatTerminal t o)

-- | A range of the terminal's active screen; 'Nothing' if either end is
-- outside the range's coordinate system.
formatRange :: (Ghostty :> es, KnownCoord c) => Terminal s -> FormatOptions -> Range c -> Eff es (Maybe ByteString)
formatRange t o r = do
  _ <- owned "formatRange"
  unsafeEff_ (formatTerminalRange t o r)
