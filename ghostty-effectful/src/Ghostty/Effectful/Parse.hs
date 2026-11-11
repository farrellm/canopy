-- | libghostty-vt's standalone parsers (DESIGN.md §4.8): SGR parameters into
-- the attributes they set, and an OSC sequence into the command it holds,
-- without a terminal. Each parse is one call, with nothing kept between
-- calls.
module Ghostty.Effectful.Parse
  ( -- * SGR
    parseSgr,
    Separator (..),
    SgrAttribute (..),

    -- * OSC
    parseOsc,
    OscTerminator (..),
    OscCommand (..),
    UnknownOsc (..),
  )
where

import Effectful (Eff, (:>))
import Ghostty.Effectful.Internal.Effect (Ghostty, ownedIO)
import Ghostty.Effectful.Internal.Parse
  ( OscCommand (..),
    OscTerminator (..),
    Separator (..),
    SgrAttribute (..),
    UnknownOsc (..),
    parseOscBytes,
    parseSgrParams,
  )

-- | The attributes an SGR sequence's parameters set, in order. Each
-- parameter is paired with the separator after it (the last one's is not
-- read): @ESC[1;4:3m@ is @[(1, Semicolon), (4, Colon), (3, Semicolon)]@.
-- No parameters is SGR 0. More than 24 parameters set nothing, since the
-- terminal ignores such a sequence.
--
-- >>> import Effectful (runEff)
-- >>> import Ghostty.Effectful (runGhostty)
-- >>> runEff (runGhostty (parseSgr [(1, Semicolon), (31, Semicolon)]))
-- Right [SgrBold,SgrFg8 1]
parseSgr :: (Ghostty :> es) => [(Word16, Separator)] -> Eff es [SgrAttribute]
parseSgr ps =
  ownedIO "parseSgr" (parseSgrParams ps)

-- | The command an OSC sequence holds, given its body (the bytes between
-- @ESC ]@ and the terminator) and how it was ended; 'Nothing' for an
-- invalid one. A sequence whose number the parser does not implement is
-- 'OscUnknown' with up to the given number of its bytes, or, with a limit
-- of 0, invalid. A sequence cancelled by CAN or SUB has no command, so it
-- need not be parsed.
--
-- >>> import Effectful (runEff)
-- >>> import Ghostty.Effectful (runGhostty)
-- >>> runEff (runGhostty (parseOsc 0 TerminatedByBel "2;hello"))
-- Right (Just (OscChangeWindowTitle "hello"))
parseOsc :: (Ghostty :> es) => Word -> OscTerminator -> ByteString -> Eff es (Maybe OscCommand)
parseOsc limit end body =
  ownedIO "parseOsc" (parseOscBytes (fromIntegral limit) end body)
