{-# LANGUAGE PatternSynonyms #-}

-- | Events and @feed@ (PLAN.md M1.6, DESIGN.md §4.5): the order of events
-- across mixed streams and chunk boundaries, each prompt mark's cursor,
-- query replies as 'WritePty', the answers 'TerminalConfig' gives and
-- 'setT' changes, and the other callbacks' events.
module Test.Ghostty.Effectful.Events (tests) where

import Control.Exception (ErrorCall (..), MaskingState (..), getMaskingState)
import Data.ByteString qualified as BS
import Effectful.Exception (try)
import Foreign.Ptr (castFunPtrToPtr, freeHaskellFunPtr, nullFunPtr)
import Ghostty.Effectful.Event
import Ghostty.Effectful.Internal.Callbacks (Pending (..), registerFunPtr)
import Ghostty.Effectful.Internal.Size (Extent (..))
import Ghostty.Effectful.Internal.Terminal (Terminal (..))
import Ghostty.Effectful.Mode (ModeKey (..))
import Ghostty.Effectful.Point (Point (..))
import Ghostty.Effectful.Terminal
import Ghostty.FFI.Terminal (ghosttyTerminalSet, pattern GhosttyTerminalOptBell)
import Ghostty.FFI.Terminal.Callbacks (mkGhosttyTerminalBellFn)
import Test.Ghostty.Effectful.Support (config, grid, withConfig, withFresh)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Events"
    [ -- Regression 15 (DESIGN.md §4.5): the render-hold callback's function
      -- pointer was made and recorded unmasked, so an asynchronous exception
      -- between the two leaked it. Every pointer is now registered through
      -- the helper checked here.
      testCase "regression 15: a callback's function pointer is made masked, and recorded" $ do
        made <- newIORef []
        state <- newIORef Unmasked
        _ <- registerFunPtr made (nullFunPtr <$ (writeIORef state =<< getMaskingState))
        masking <- readIORef state
        recorded <- readIORef made
        (masking, recorded) @?= (MaskedInterruptible, [nullFunPtr]),
      testGroup
        "order"
        [ testCase "OSC 133 marks, bells and titles come in stream order" $ do
            evs <- withFresh (`feed` session)
            evs @?= sessionEvents,
          testCase "the same, split at every byte" $ do
            splits <- withFresh $ \t ->
              forM [0 .. BS.length session] $ \i -> do
                a <- feed t (BS.take i session)
                b <- feed t (BS.drop i session)
                _ <- reset t
                _ <- feed t "\ESCc"
                pure (i, a <> b)
            filter ((/= sessionEvents) . snd) splits @?= [],
          testCase "a mark ended by ESC \\ has the cursor right after it" $ do
            evs <- withFresh (`feed` "\ESC]133;A\ESC\\$ \ESC]133;B\ESC\\1+1")
            evs
              @?= [ mark (PromptStart PrimaryPrompt) 0 0,
                    mark InputStart 2 0
                  ],
          testCase "a title is the title after the write that set it" $ do
            evs <- withFresh (`feed` "\ESC]2;one\a\ESC]0;two\ESC\\")
            evs @?= [TitleChanged "one", TitleChanged "two"]
        ],
      testGroup
        "queries"
        [ testCase "a status report is a WritePty" $ do
            evs <- withFresh (`feed` "ab\ESC[6n")
            evs @?= [WritePty "\ESC[1;3R"],
          testCase "device attributes default to the library's" $ do
            evs <- withFresh (`feed` "\ESC[c\ESC[>c")
            evs @?= [WritePty "\ESC[?62;22c\ESC[>1;0;0c"],
          testCase "device attributes come from the configuration, and setT" $ do
            let custom = DeviceAttributes {conformanceLevel = 65, features = [22, 52], deviceType = 41, firmwareVersion = 7, romCartridge = 0, unitId = 0}
            evs <- withConfig config {deviceAttributes = custom} $ \t -> do
              a <- feed t "\ESC[c"
              setT t SetDeviceAttributes defaultDeviceAttributes
              b <- feed t "\ESC[c"
              pure (a <> b)
            evs @?= [WritePty "\ESC[?65;22;52c", WritePty "\ESC[?62;22c"],
          testCase "the colour scheme is unanswered until set" $ do
            evs <- withFresh $ \t -> do
              a <- feed t "\ESC[?996n"
              setT t SetColorScheme (Just Dark)
              b <- feed t "\ESC[?996n"
              setT t SetColorScheme (Just Light)
              c <- feed t "\ESC[?996n"
              pure (a, b, c)
            evs @?= ([], [WritePty "\ESC[?997;1n"], [WritePty "\ESC[?997;2n"]),
          testCase "ENQ is answered with the configured bytes" $ do
            evs <- withConfig config {enquiry = "canopy\r"} $ \t -> do
              a <- feed t "\ENQ"
              setT t SetEnquiry ""
              b <- feed t "\ENQ"
              setT t SetEnquiry "again"
              c <- feed t "\ENQ"
              pure (a, b, c)
            evs @?= ([WritePty "canopy\r"], [], [WritePty "again"]),
          testCase "XTVERSION reports the configured name, or the library's" $ do
            evs <- withFresh $ \t -> do
              a <- feed t "\ESC[>q"
              setT t SetXtversion "canopy 0.1"
              b <- feed t "\ESC[>q"
              pure (a <> b)
            evs @?= [WritePty "\ESCP>|libghostty\ESC\\", WritePty "\ESCP>|canopy 0.1\ESC\\"],
          testCase "size queries are answered from the grid and cell size, until turned off" $ do
            evs <- withConfig config {cellPixels = CellPixels 10 20} $ \t -> do
              a <- feed t "\ESC[18t\ESC[14t\ESC[16t"
              _ <- resize t (grid 100 30) (CellPixels 8 16)
              b <- feed t "\ESC[18t"
              setT t SetSize False
              c <- feed t "\ESC[18t"
              pure (a, b, c)
            evs @?= ([WritePty "\ESC[8;24;80t\ESC[4;480;800t\ESC[6;20;10t"], [WritePty "\ESC[8;30;100t"], []),
          testCase "an in-band size report comes back from resize" $ do
            evs <- withConfig config {cellPixels = CellPixels 10 20} $ \t -> do
              a <- feed t "\ESC[?2048h"
              b <- resize t (grid 100 30) (CellPixels 10 20)
              pure (a, b)
            evs @?= ([WritePty "\ESC[48;24;80;480;800t"], [WritePty "\ESC[48;30;100;600;1000t"]),
          testCase "a reply is joined into one WritePty" $ do
            evs <- withFresh (`feed` "\ESC[6n\ESC[c\ESC[?2004$p")
            evs @?= [WritePty "\ESC[1;1R\ESC[?62;22c\ESC[?2004;2$y"]
        ],
      testGroup
        "events"
        [ testCase "OSC 133 marks carry their parameters" $ do
            evs <- withFresh (`feed` "\ESC]133;A;k=s\a\ESC]133;C;cmdline_url=ls%20-l\a\ESC]133;D;2\a\ESC]133;D\a")
            evs
              @?= [ mark (PromptStart SecondaryPrompt) 0 0,
                    mark (OutputStart "ls -l") 0 0,
                    mark (CommandEnd (Just 2) "") 0 0,
                    mark (CommandEnd Nothing "") 0 0
                  ],
          testCase "the working directory" $ do
            evs <- withFresh (`feed` "\ESC]7;file://host/tmp\a")
            evs @?= [PwdChanged "file://host/tmp"],
          testCase "clipboard writes are refused until accepted" $ do
            evs <- withFresh $ \t -> do
              a <- feed t "\ESC]52;c;aGk=\a"
              setT t SetClipboardWrite True
              b <- feed t "\ESC]52;c;aGk=\a"
              pure (a, b)
            evs @?= ([], [ClipboardWrite StandardClipboard [ClipboardContent "text/plain" "hi"]]),
          testCase "notifications, progress, and program status" $ do
            evs <- withFresh (`feed` "\ESC]9;done\a\ESC]777;notify;T;B\a\ESC]9;4;1;40\a\ESC]9;4;3\a\ESC]7501;state=working:app=make\a")
            evs
              @?= [ Notification "" "done",
                    Notification "T" "B",
                    Progress ProgressReport {state = ProgressSet, percent = Just 40},
                    Progress ProgressReport {state = ProgressIndeterminate, percent = Nothing},
                    ProgramStatus ProgramReport {state = StatusWorking, need = Nothing, percent = Nothing, record = "", app = "make", title = "", message = ""}
                  ],
          testCase "a render hold, ended by a resize" $ do
            evs <- withFresh $ \t -> do
              a <- feed t "\ESC[?2026h"
              b <- resize t (grid 80 24) noCellPixels
              pure (a, b)
            evs @?= ([RenderHold True], [RenderHold False]),
          testCase "a render hold, ended by reset" $ do
            evs <- withFresh $ \t -> do
              a <- feed t "\ESC[?2026hx\ESC[?2026l\ESC[?2026h"
              b <- reset t
              pure (a, b)
            evs @?= ([RenderHold True, RenderHold False, RenderHold True], [RenderHold False]),
          -- The header: a full reset clears progress and program status,
          -- reporting each, before it reports the reset.
          testCase "a full reset from the program" $ do
            evs <- withFresh (`feed` "\ESC]2;t\a\ESCc")
            evs
              @?= [ TitleChanged "t",
                    Progress ProgressReport {state = ProgressRemove, percent = Nothing},
                    ProgramStatus ProgramReport {state = StatusClear, need = Nothing, percent = Nothing, record = "", app = "", title = "", message = ""},
                    Reset
                  ],
          testCase "a window resize request" $ do
            evs <- withFresh (`feed` "\ESC[8;30;100t\ESC[8;0;90t")
            evs
              @?= [ ResizeRequest WindowSize {cols = Just (Extent 100), rows = Just (Extent 30)},
                    ResizeRequest WindowSize {cols = Just (Extent 90), rows = Nothing}
                  ],
          testCase "unsupported sequences, once their limit is set" $ do
            evs <- withFresh $ \t -> do
              a <- feed t "\ESC]7777;x\a"
              setT t SetUnknownMaxBytes 4
              b <- feed t "\ESC]7777;x\a\ESC_zz\ESC\\\ESC]7778\ESC\\"
              pure (a, b)
            evs
              @?= ( [],
                    [ UnknownSequence UnsupportedSequence {kind = Osc TerminatedByBel, truncated = True, content = "7777"},
                      UnknownSequence UnsupportedSequence {kind = Apc, truncated = False, content = "zz"},
                      UnknownSequence UnsupportedSequence {kind = Osc TerminatedBySt, truncated = False, content = "7778"}
                    ]
                  ),
          testCase "a mode set through setT reports nothing" $ do
            evs <- withFresh $ \t -> do
              setT t (SetMode ModeSynchronizedOutput) True
              feed t "x"
            evs @?= []
        ],
      testGroup
        "regressions"
        [ -- A failure while resolving one stop's events stopped the write
          -- there, so the bytes after it never reached the terminal. Now
          -- every byte is written and the failure thrown after.
          testCase "regression 1: a failure mid-chunk still writes the rest" $ do
            (fp, (mid, before)) <- withFresh $ \t -> do
              -- a bell that records a failure, as a sheltered body would
              fp <- liftIO (mkGhosttyTerminalBellFn (\_ _ -> modifyIORef' t.events (Failed (toException boom) :)))
              _ <- liftIO (ghosttyTerminalSet t.handle GhosttyTerminalOptBell (castFunPtrToPtr fp))
              a <- try @ErrorCall (feed t "\a\ESC]2;x\aabc")
              ax <- getT t CursorX
              liftIO (modifyIORef' t.events (Failed (toException boom) :))
              b <- try @ErrorCall (feed t "de")
              bx <- getT t CursorX
              pure (fp, ((void a, ax), (void b, bx)))
            freeHaskellFunPtr fp
            (mid, before) @?= ((Left boom, 3), (Left boom, 5)),
          -- Replies were joined only within one stop, so an OSC between two
          -- replies split one reply into two writes.
          testCase "regression 2: a reply split by an OSC stop is one write" $ do
            evs <- withFresh (`feed` "\ESC[6n\ESC]7777;x\a\ESC[6n")
            evs @?= [WritePty "\ESC[1;1R\ESC[1;1R"]
        ]
    ]

-- | A REPL session's worth of output: a prompt, typed input, a bell, a
-- title, output, the end of the command and the next prompt. One mark ends
-- with @ESC \\@, which a split can separate.
session :: ByteString
session =
  "\ESC]133;A\a>>> \ESC]133;B\ESC\\"
    <> "1+1\r\n\ESC]133;C\a\a"
    <> "\ESC]2;py\a2\r\n\ESC]133;D;0\a"
    <> "\ESC]133;A\a>>> "

-- | What 'session' reports.
sessionEvents :: [TerminalEvent]
sessionEvents =
  [ mark (PromptStart PrimaryPrompt) 0 0,
    mark InputStart 4 0,
    mark (OutputStart "") 0 1,
    Bell,
    TitleChanged "py",
    mark (CommandEnd (Just 0) "") 0 2,
    mark (PromptStart PrimaryPrompt) 0 2
  ]

-- | A prompt mark with the cursor at a column and row.
mark :: PromptStep -> Word16 -> Word32 -> TerminalEvent
mark step c r = SemanticPrompt PromptMark {step, cursor = Point {col = c, row = r}}

-- | What a test's injected failure throws.
boom :: ErrorCall
boom = ErrorCall "boom"
