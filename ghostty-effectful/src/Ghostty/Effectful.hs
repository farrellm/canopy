-- | The high-level interface to libghostty-vt (DESIGN.md §4): the 'Ghostty'
-- effect and its interpreter. The handles are in "Ghostty.Effectful.Terminal"
-- and its siblings, and the errors in "Ghostty.Effectful.Error".
module Ghostty.Effectful
  ( -- * The effect
    Ghostty,
    runGhostty,

    -- * Build identity
    buildMetadata,
  )
where

import Effectful (Eff, IOE, (:>))
import Ghostty.Effectful.Internal.Effect (Ghostty, linkedVersion, runGhostty)

-- | The linked library's identity: its version, then each optional feature it
-- was built without; 'Nothing' if the library would not say. 'runGhostty'
-- refuses to run unless it is exactly the pinned release
-- (@LIBGHOSTTY_VT_VERSION@).
--
-- >>> import Effectful (runEff)
-- >>> isJust <$> runEff buildMetadata
-- True
buildMetadata :: (IOE :> es) => Eff es (Maybe Text)
buildMetadata = liftIO linkedVersion
