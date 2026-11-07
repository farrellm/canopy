-- | Widths as libghostty-vt lays text out (DESIGN.md §4.8, §11.2): how many
-- cells a character, or a grapheme cluster, takes when printed. These are
-- the terminal's own tables, which tendril's widths are checked against.
module Ghostty.Effectful.Unicode
  ( codepointWidth,
    graphemes,
    Grapheme (..),
  )
where

import Effectful (Eff, (:>))
import Effectful.Dispatch.Static (unsafeEff_)
import Ghostty.Effectful.Internal.Effect (Ghostty, owned)
import Ghostty.Effectful.Internal.Unicode (Grapheme (..), graphemesOf, widthOf)

-- | The cells one character takes: 0 for controls, combining marks and
-- default-ignorables (ZWJ, variation selectors), 2 for East Asian wide and
-- fullwidth characters (emoji with emoji presentation among them) and
-- regional indicators, and 1 for the rest. Summing these is the line's
-- width only with grapheme clustering (mode 2027) off; with it on, use
-- 'graphemes'.
--
-- >>> import Effectful (runEff)
-- >>> import Ghostty.Effectful (runGhostty)
-- >>> runEff (runGhostty (traverse codepointWidth ("a漢\x301" :: String)))
-- Right [1,2,0]
codepointWidth :: (Ghostty :> es) => Char -> Eff es Word8
codepointWidth c = do
  _ <- owned "codepointWidth"
  pure (widthOf c)

-- | The text's grapheme clusters, each with its width, as the terminal
-- segments and measures them with grapheme clustering on (mode 2027): a
-- valid VS16 makes a cluster wide and a valid VS15 narrow, and any other
-- continuation that adds width makes it wide. Every character of the text
-- is in exactly one cluster, in order. A cluster of control characters
-- has a width the header leaves unspecified.
--
-- >>> import Effectful (runEff)
-- >>> import Ghostty.Effectful (runGhostty)
-- >>> fmap (map (.width)) <$> runEff (runGhostty (graphemes "e\x301\&👍🏽x"))
-- Right [1,2,1]
graphemes :: (Ghostty :> es) => Text -> Eff es [Grapheme]
graphemes t = do
  _ <- owned "graphemes"
  unsafeEff_ (graphemesOf t)
