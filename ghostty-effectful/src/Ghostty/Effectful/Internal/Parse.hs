-- | The standalone parsers' calls (DESIGN.md §4.8). The public API is
-- "Ghostty.Effectful.Parse".
--
-- A parser is not a handle at this level: each parse makes one, feeds it
-- one sequence, copies out what it found and frees it, in one call. What a
-- parser returns is borrowed until its next call, so nothing borrowed can
-- leave.
module Ghostty.Effectful.Internal.Parse
  ( -- * SGR
    Separator (..),
    SgrAttribute (..),
    parseSgrParams,

    -- * OSC
    OscTerminator (..),
    OscCommand (..),
    UnknownOsc (..),
    parseOscBytes,
    oscCommandType,
  )
where

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Foreign.C.String (castCharToCChar)
import Foreign.C.Types (CBool, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, withArrayLen)
import Foreign.Marshal.Utils (toBool, with)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, peek)
import Ghostty.Effectful.Color (Rgb)
import Ghostty.Effectful.Error (CallSite (..))
import Ghostty.Effectful.Event (OscTerminator (..))
import Ghostty.Effectful.Internal.Convert (fromRgb, fromUnderline, oscTerminator, stringText, unfoldWhileM, unknownEnum)
import Ghostty.Effectful.Internal.Error (expectSuccess, newHandle, outVia)
import Ghostty.Effectful.Style (Underline)
import Ghostty.FFI.Osc
import Ghostty.FFI.Sgr
import Ghostty.FFI.Types (ghosttyStringBytes)

-- * SGR

-- | What follows an SGR parameter: @;@ or @:@. The colon forms are the
-- sub-parameters of ITU T.416 (@4:3@, curly underline; @38:2::r:g:b@).
data Separator
  = -- | @;@
    Semicolon
  | -- | @:@
    Colon
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | One attribute an SGR sequence sets. The constructors are the header's
-- @GHOSTTY_SGR_ATTR_*@ names.
data SgrAttribute
  = -- | SGR 0, or no parameters: every attribute reset
    SgrUnset
  | -- | parameters the parser does not know: the whole list, and the part
    -- of it from where parsing failed
    SgrUnknown [Word16] [Word16]
  | -- | 1
    SgrBold
  | -- | 22: bold and faint off
    SgrResetBold
  | -- | 3
    SgrItalic
  | -- | 23
    SgrResetItalic
  | -- | 2
    SgrFaint
  | -- | 4, 21 or @4:n@: an underline, or none
    SgrUnderline Underline
  | -- | @58:2::r:g:b@: the underline's colour
    SgrUnderlineColor Rgb
  | -- | @58:5:n@: the underline's colour from the palette
    SgrUnderlineColor256 Word8
  | -- | 59
    SgrResetUnderlineColor
  | -- | 53
    SgrOverline
  | -- | 55
    SgrResetOverline
  | -- | 5 or 6
    SgrBlink
  | -- | 25
    SgrResetBlink
  | -- | 7
    SgrInverse
  | -- | 27
    SgrResetInverse
  | -- | 8
    SgrInvisible
  | -- | 28
    SgrResetInvisible
  | -- | 9
    SgrStrikethrough
  | -- | 29
    SgrResetStrikethrough
  | -- | @38;2;r;g;b@: a direct foreground colour
    SgrDirectColorFg Rgb
  | -- | @48;2;r;g;b@: a direct background colour
    SgrDirectColorBg Rgb
  | -- | 40–47: one of the eight background colours, by palette index (0–7)
    SgrBg8 Word8
  | -- | 30–37: one of the eight foreground colours, by palette index (0–7)
    SgrFg8 Word8
  | -- | 39
    SgrResetFg
  | -- | 49
    SgrResetBg
  | -- | 100–107: one of the eight bright background colours, by palette index (8–15)
    SgrBrightBg8 Word8
  | -- | 90–97: one of the eight bright foreground colours, by palette index (8–15)
    SgrBrightFg8 Word8
  | -- | @48;5;n@: a background colour from the palette
    SgrBg256 Word8
  | -- | @38;5;n@: a foreground colour from the palette
    SgrFg256 Word8
  deriving stock (Eq, Show)

-- | @ghostty_sgr_set_params@ and @ghostty_sgr_next@ until it is done, on a
-- parser of its own. An unknown attribute's lists are borrowed from the
-- parser, so they are copied out before the next call. More than
-- 'maxSgrParams' parameters set nothing: the terminal ignores such a
-- sequence, and the parser refuses their separators.
parseSgrParams :: [(Word16, Separator)] -> IO [SgrAttribute]
parseSgrParams ps
  | length ps > maxSgrParams = pure []
  | otherwise =
      bracket (newHandle (CallSite "ghostty_sgr_new") ghosttySgrNew) ghosttySgrFree $ \parser ->
        withArrayLen (map fst ps) $ \n params ->
          withArrayLen (map (sepChar . snd) ps) $ \_ seps -> do
            expectSuccess (CallSite "ghostty_sgr_set_params")
              =<< ghosttySgrSetParams parser params seps (fromIntegral n)
            alloca $ \ap -> unfoldWhileM (toBool <$> ghosttySgrNext parser ap) (attribute =<< peek ap)
  where
    sepChar = \case
      Semicolon -> castCharToCChar ';'
      Colon -> castCharToCChar ':'

    site = CallSite "ghostty_sgr_next"

    attribute :: GhosttySgrAttribute -> IO SgrAttribute
    attribute = \case
      SgrAttrUnset -> pure SgrUnset
      SgrAttrUnknown u ->
        SgrUnknown
          <$> peekArray (fromIntegral u.fullLen) u.fullPtr
          <*> peekArray (fromIntegral u.partialLen) u.partialPtr
      SgrAttrBold -> pure SgrBold
      SgrAttrResetBold -> pure SgrResetBold
      SgrAttrItalic -> pure SgrItalic
      SgrAttrResetItalic -> pure SgrResetItalic
      SgrAttrFaint -> pure SgrFaint
      SgrAttrUnderline u -> SgrUnderline <$> fromUnderline site u
      SgrAttrUnderlineColor c -> pure (SgrUnderlineColor (fromRgb c))
      SgrAttrUnderlineColor256 i -> pure (SgrUnderlineColor256 i)
      SgrAttrResetUnderlineColor -> pure SgrResetUnderlineColor
      SgrAttrOverline -> pure SgrOverline
      SgrAttrResetOverline -> pure SgrResetOverline
      SgrAttrBlink -> pure SgrBlink
      SgrAttrResetBlink -> pure SgrResetBlink
      SgrAttrInverse -> pure SgrInverse
      SgrAttrResetInverse -> pure SgrResetInverse
      SgrAttrInvisible -> pure SgrInvisible
      SgrAttrResetInvisible -> pure SgrResetInvisible
      SgrAttrStrikethrough -> pure SgrStrikethrough
      SgrAttrResetStrikethrough -> pure SgrResetStrikethrough
      SgrAttrDirectColorFg c -> pure (SgrDirectColorFg (fromRgb c))
      SgrAttrDirectColorBg c -> pure (SgrDirectColorBg (fromRgb c))
      SgrAttrBg8 i -> pure (SgrBg8 i)
      SgrAttrFg8 i -> pure (SgrFg8 i)
      SgrAttrResetFg -> pure SgrResetFg
      SgrAttrResetBg -> pure SgrResetBg
      SgrAttrBrightBg8 i -> pure (SgrBrightBg8 i)
      SgrAttrBrightFg8 i -> pure (SgrBrightFg8 i)
      SgrAttrBg256 i -> pure (SgrBg256 i)
      SgrAttrFg256 i -> pure (SgrFg256 i)
      SgrAttrOther (GhosttySgrAttributeTag n) -> unknownEnum site ghosttySgrAttributeTagEnum n

-- | The most parameters a CSI sequence the terminal acts on has (the VT
-- parser's @MAX_PARAMS@), which is also how many separators
-- @ghostty_sgr_set_params@ takes.
maxSgrParams :: Int
maxSgrParams = 24

-- * OSC

-- | A command an OSC sequence holds. The constructors are the header's
-- @GHOSTTY_OSC_COMMAND_*@ names. The parser reports the data of two: a
-- window title, and a sequence whose number it does not implement; the
-- rest it only identifies (a terminal is what acts on them).
data OscCommand
  = OscChangeWindowTitle Text
  | OscChangeWindowIcon
  | OscSemanticPrompt
  | OscClipboardContents
  | OscReportPwd
  | OscMouseShape
  | OscColorOperation
  | OscKittyColorProtocol
  | OscShowDesktopNotification
  | OscHyperlinkStart
  | OscHyperlinkEnd
  | OscConemuSleep
  | OscConemuShowMessageBox
  | OscConemuChangeTabTitle
  | OscConemuProgressReport
  | OscConemuWaitInput
  | OscConemuGuimacro
  | OscConemuRunProcess
  | OscConemuOutputEnvironmentVariable
  | OscConemuXtermEmulation
  | OscConemuComment
  | OscKittyTextSizing
  | OscKittyClipboardProtocol
  | OscKittyDndProtocol
  | OscHierarchicalContext
  | OscKittyDesktopNotification
  | OscUnknown UnknownOsc
  | OscProgramStatus
  deriving stock (Eq, Show)

-- | A sequence whose number the parser does not implement.
data UnknownOsc = UnknownOsc
  { -- | its bytes, number included (@7400;status=busy@), up to the limit
    content :: ByteString,
    -- | whether it was longer than the limit, so 'content' is its start
    truncated :: Bool,
    -- | how it was ended
    terminator :: OscTerminator
  }
  deriving stock (Eq, Show)

-- | @ghostty_osc_next@ for each byte of a sequence's body (what lies
-- between @ESC ]@ and its terminator), then @ghostty_osc_end@, on a parser
-- of its own that keeps up to the given number of bytes of a sequence it
-- does not implement (@GHOSTTY_OSC_OPT_UNKNOWN_MAX_BYTES@; 0 keeps none, so
-- such a sequence is invalid). 'Nothing' for an invalid sequence. What the
-- command holds is borrowed from the parser, so it is copied out before the
-- parser is freed.
parseOscBytes :: CSize -> OscTerminator -> ByteString -> IO (Maybe OscCommand)
parseOscBytes limit end body =
  bracket (newHandle (CallSite "ghostty_osc_new") ghosttyOscNew) ghosttyOscFree $ \parser -> do
    when (limit > 0) $
      with limit $ \lp ->
        expectSuccess (CallSite "ghostty_osc_set") =<< ghosttyOscSet parser GhosttyOscOptUnknownMaxBytes (castPtr lp)
    BS.foldr (\b rest -> ghosttyOscNext parser b >> rest) pass body
    cmd <- ghosttyOscEnd parser $ case end of
      TerminatedBySt -> 0x5c
      TerminatedByBel -> 0x07
    ghosttyOscCommandType cmd >>= command cmd
  where
    site = CallSite "ghostty_osc_command_data"

    -- One datum of the command; the header says each is there for its type,
    -- so anything but success (@GHOSTTY_NO_VALUE@ included) is an invariant.
    datum :: (Storable a) => GhosttyOscCommand -> GhosttyOscCommandData -> IO a
    datum cmd key = outVia site (ghosttyOscCommandData cmd key . castPtr)

    command :: GhosttyOscCommand -> GhosttyOscCommandType -> IO (Maybe OscCommand)
    command cmd = \case
      GhosttyOscCommandInvalid -> pure Nothing
      GhosttyOscCommandChangeWindowTitle -> do
        -- A GhosttyString: exactly its bytes, which are not NUL-terminated
        -- and may hold a NUL (regression 8).
        Just . OscChangeWindowTitle <$> (stringText =<< datum cmd GhosttyOscDataChangeWindowTitleStr)
      GhosttyOscCommandUnknown -> do
        s <- datum cmd GhosttyOscDataUnknownContent
        content <- ghosttyStringBytes s
        truncated <- toBool @CBool <$> datum cmd GhosttyOscDataUnknownTruncated
        terminator <- either (unknownEnum site ghosttyOscTerminatorEnum) pure . oscTerminator =<< datum cmd GhosttyOscDataUnknownTerminator
        pure (Just (OscUnknown UnknownOsc {content, truncated, terminator}))
      GhosttyOscCommandChangeWindowIcon -> pure (Just OscChangeWindowIcon)
      GhosttyOscCommandSemanticPrompt -> pure (Just OscSemanticPrompt)
      GhosttyOscCommandClipboardContents -> pure (Just OscClipboardContents)
      GhosttyOscCommandReportPwd -> pure (Just OscReportPwd)
      GhosttyOscCommandMouseShape -> pure (Just OscMouseShape)
      GhosttyOscCommandColorOperation -> pure (Just OscColorOperation)
      GhosttyOscCommandKittyColorProtocol -> pure (Just OscKittyColorProtocol)
      GhosttyOscCommandShowDesktopNotification -> pure (Just OscShowDesktopNotification)
      GhosttyOscCommandHyperlinkStart -> pure (Just OscHyperlinkStart)
      GhosttyOscCommandHyperlinkEnd -> pure (Just OscHyperlinkEnd)
      GhosttyOscCommandConemuSleep -> pure (Just OscConemuSleep)
      GhosttyOscCommandConemuShowMessageBox -> pure (Just OscConemuShowMessageBox)
      GhosttyOscCommandConemuChangeTabTitle -> pure (Just OscConemuChangeTabTitle)
      GhosttyOscCommandConemuProgressReport -> pure (Just OscConemuProgressReport)
      GhosttyOscCommandConemuWaitInput -> pure (Just OscConemuWaitInput)
      GhosttyOscCommandConemuGuimacro -> pure (Just OscConemuGuimacro)
      GhosttyOscCommandConemuRunProcess -> pure (Just OscConemuRunProcess)
      GhosttyOscCommandConemuOutputEnvironmentVariable -> pure (Just OscConemuOutputEnvironmentVariable)
      GhosttyOscCommandConemuXtermEmulation -> pure (Just OscConemuXtermEmulation)
      GhosttyOscCommandConemuComment -> pure (Just OscConemuComment)
      GhosttyOscCommandKittyTextSizing -> pure (Just OscKittyTextSizing)
      GhosttyOscCommandKittyClipboardProtocol -> pure (Just OscKittyClipboardProtocol)
      GhosttyOscCommandKittyDndProtocol -> pure (Just OscKittyDndProtocol)
      GhosttyOscCommandHierarchicalContext -> pure (Just OscHierarchicalContext)
      GhosttyOscCommandKittyDesktopNotification -> pure (Just OscKittyDesktopNotification)
      GhosttyOscCommandProgramStatus -> pure (Just OscProgramStatus)
      GhosttyOscCommandTypeUnknown n -> unknownEnum (CallSite "ghostty_osc_command_type") ghosttyOscCommandTypeEnum n

-- | The header's type of a command, by which a test holds the constructors'
-- names against the header.
oscCommandType :: OscCommand -> GhosttyOscCommandType
oscCommandType = \case
  OscChangeWindowTitle _ -> GhosttyOscCommandChangeWindowTitle
  OscUnknown _ -> GhosttyOscCommandUnknown
  OscChangeWindowIcon -> GhosttyOscCommandChangeWindowIcon
  OscSemanticPrompt -> GhosttyOscCommandSemanticPrompt
  OscClipboardContents -> GhosttyOscCommandClipboardContents
  OscReportPwd -> GhosttyOscCommandReportPwd
  OscMouseShape -> GhosttyOscCommandMouseShape
  OscColorOperation -> GhosttyOscCommandColorOperation
  OscKittyColorProtocol -> GhosttyOscCommandKittyColorProtocol
  OscShowDesktopNotification -> GhosttyOscCommandShowDesktopNotification
  OscHyperlinkStart -> GhosttyOscCommandHyperlinkStart
  OscHyperlinkEnd -> GhosttyOscCommandHyperlinkEnd
  OscConemuSleep -> GhosttyOscCommandConemuSleep
  OscConemuShowMessageBox -> GhosttyOscCommandConemuShowMessageBox
  OscConemuChangeTabTitle -> GhosttyOscCommandConemuChangeTabTitle
  OscConemuProgressReport -> GhosttyOscCommandConemuProgressReport
  OscConemuWaitInput -> GhosttyOscCommandConemuWaitInput
  OscConemuGuimacro -> GhosttyOscCommandConemuGuimacro
  OscConemuRunProcess -> GhosttyOscCommandConemuRunProcess
  OscConemuOutputEnvironmentVariable -> GhosttyOscCommandConemuOutputEnvironmentVariable
  OscConemuXtermEmulation -> GhosttyOscCommandConemuXtermEmulation
  OscConemuComment -> GhosttyOscCommandConemuComment
  OscKittyTextSizing -> GhosttyOscCommandKittyTextSizing
  OscKittyClipboardProtocol -> GhosttyOscCommandKittyClipboardProtocol
  OscKittyDndProtocol -> GhosttyOscCommandKittyDndProtocol
  OscHierarchicalContext -> GhosttyOscCommandHierarchicalContext
  OscKittyDesktopNotification -> GhosttyOscCommandKittyDesktopNotification
  OscProgramStatus -> GhosttyOscCommandProgramStatus
