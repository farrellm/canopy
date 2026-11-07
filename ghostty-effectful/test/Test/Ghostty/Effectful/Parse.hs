-- | The standalone parsers (PLAN.md M1.9, DESIGN.md §4.8): SGR parameters
-- and OSC sequences, each checked against what the sequence is specified to
-- mean, and the OSC commands held against the header's names.
module Test.Ghostty.Effectful.Parse (tests) where

import Data.Char (isAlphaNum, toLower)
import Data.Text qualified as T
import Ghostty.Effectful.Color (Rgb (..))
import Ghostty.Effectful.Internal.Parse (oscCommandType)
import Ghostty.Effectful.Parse
import Ghostty.Effectful.Style (Underline (..))
import Ghostty.FFI.Layout (CEnum (..))
import Ghostty.FFI.Osc (GhosttyOscCommandType (..), ghosttyOscCommandTypeEnum)
import Test.Ghostty.Effectful.Support (ghostty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Parse"
    [ testGroup
        "SGR"
        [ testCase "bold and a foreground" $
            sgr "1;31" >>= (@?= [SgrBold, SgrFg8 1]),
          testCase "no parameters, and 0, reset everything" $ do
            sgr "" >>= (@?= [SgrUnset])
            sgr "0" >>= (@?= [SgrUnset]),
          testCase "a curly underline, by colon" $
            sgr "4:3" >>= (@?= [SgrUnderline CurlyUnderline]),
          testCase "direct and 256-colour foregrounds and backgrounds" $
            sgr "38;2;1;2;3;48;5;200" >>= (@?= [SgrDirectColorFg (Rgb 1 2 3), SgrBg256 200]),
          testCase "the underline's colour, by colons" $
            sgr "58:2::1:2:3" >>= (@?= [SgrUnderlineColor (Rgb 1 2 3)]),
          testCase "bright colours and resets" $
            sgr "91;102;39;49;22" >>= (@?= [SgrBrightFg8 9, SgrBrightBg8 10, SgrResetFg, SgrResetBg, SgrResetBold]),
          testCase "an unknown parameter keeps its lists" $
            sgr "1;999" >>= (@?= [SgrBold, SgrUnknown [1, 999] [999]])
        ],
      testGroup
        "OSC"
        [ testCase "a window title, ended by BEL or ST" $ do
            r <- ghostty (traverse (\e -> parseOsc 0 e "2;hello") universe)
            r @?= [Just (OscChangeWindowTitle "hello"), Just (OscChangeWindowTitle "hello")],
          testCase "commands the parser only identifies" $ do
            r <- ghostty (traverse (parseOsc 0 TerminatedByBel) ["8;;https://example.com", "8;;", "133;A", "7;file:///tmp"])
            r @?= map Just [OscHyperlinkStart, OscHyperlinkEnd, OscSemanticPrompt, OscReportPwd],
          testCase "an unimplemented number: invalid, or kept to the limit" $ do
            r <- ghostty $ do
              dropped <- parseOsc 0 TerminatedByBel "7400;status=busy"
              kept <- parseOsc 1024 TerminatedBySt "7400;status=busy"
              cut <- parseOsc 4 TerminatedByBel "7400;status=busy"
              pure (dropped, kept, cut)
            r
              @?= ( Nothing,
                    Just (OscUnknown UnknownOsc {content = "7400;status=busy", truncated = False, terminator = TerminatedBySt}),
                    Just (OscUnknown UnknownOsc {content = "7400", truncated = True, terminator = TerminatedByBel})
                  ),
          testCase "a malformed sequence is invalid" $ do
            r <- ghostty (parseOsc 0 TerminatedByBel "not a number")
            r @?= Nothing,
          testCase "every command is a header value, each once, and the header has no other" $
            sort (map code allCommands)
              @?= sort [v | (n, v) <- ghosttyOscCommandTypeEnum.values, n /= "GHOSTTY_OSC_COMMAND_INVALID"],
          testCase "every command is its header row's name" $
            filter
              (\c -> fmap squash (rowName ghosttyOscCommandTypeEnum (code c)) /= Just (squash ("GHOSTTY_OSC_COMMAND_" <> T.drop 3 (T.takeWhile (/= ' ') (show c)))))
              allCommands
              @?= []
        ]
    ]

-- | Parse SGR parameters written as in a sequence: numbers separated by
-- @;@ or @:@.
sgr :: Text -> IO [SgrAttribute]
sgr s = ghostty (parseSgr (params (toString s)))
  where
    params str = case span (`notElem` (";:" :: String)) str of
      ("", []) -> []
      (n, rest) ->
        let sep = case rest of
              ':' : _ -> Colon
              _ -> Semicolon
         in (fromMaybe 0 (readMaybe n), sep) : params (drop 1 rest)

-- | Every command, once.
allCommands :: [OscCommand]
allCommands =
  [ OscChangeWindowTitle "",
    OscChangeWindowIcon,
    OscSemanticPrompt,
    OscClipboardContents,
    OscReportPwd,
    OscMouseShape,
    OscColorOperation,
    OscKittyColorProtocol,
    OscShowDesktopNotification,
    OscHyperlinkStart,
    OscHyperlinkEnd,
    OscConemuSleep,
    OscConemuShowMessageBox,
    OscConemuChangeTabTitle,
    OscConemuProgressReport,
    OscConemuWaitInput,
    OscConemuGuimacro,
    OscConemuRunProcess,
    OscConemuOutputEnvironmentVariable,
    OscConemuXtermEmulation,
    OscConemuComment,
    OscKittyTextSizing,
    OscKittyClipboardProtocol,
    OscKittyDndProtocol,
    OscHierarchicalContext,
    OscKittyDesktopNotification,
    OscUnknown UnknownOsc {content = "", truncated = False, terminator = TerminatedBySt},
    OscProgramStatus
  ]
  where
    -- Kept total, so a constructor added to 'OscCommand' fails to compile
    -- here until it is added to the list.
    _total = \case
      OscChangeWindowTitle _ -> ()
      OscChangeWindowIcon -> ()
      OscSemanticPrompt -> ()
      OscClipboardContents -> ()
      OscReportPwd -> ()
      OscMouseShape -> ()
      OscColorOperation -> ()
      OscKittyColorProtocol -> ()
      OscShowDesktopNotification -> ()
      OscHyperlinkStart -> ()
      OscHyperlinkEnd -> ()
      OscConemuSleep -> ()
      OscConemuShowMessageBox -> ()
      OscConemuChangeTabTitle -> ()
      OscConemuProgressReport -> ()
      OscConemuWaitInput -> ()
      OscConemuGuimacro -> ()
      OscConemuRunProcess -> ()
      OscConemuOutputEnvironmentVariable -> ()
      OscConemuXtermEmulation -> ()
      OscConemuComment -> ()
      OscKittyTextSizing -> ()
      OscKittyClipboardProtocol -> ()
      OscKittyDndProtocol -> ()
      OscHierarchicalContext -> ()
      OscKittyDesktopNotification -> ()
      OscUnknown _ -> ()
      OscProgramStatus -> ()

-- | A command's header value.
code :: OscCommand -> Integer
code c = let GhosttyOscCommandType n = oscCommandType c in toInteger n

-- | The header's name for a value.
rowName :: CEnum -> Integer -> Maybe Text
rowName e n = fst <$> find ((== n) . snd) e.values

-- | A name without case or underscores.
squash :: Text -> Text
squash = T.filter isAlphaNum . T.map toLower
