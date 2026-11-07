-- | The formatter and selection bindings against the linked library: the
-- formatter's keyed options and their defaults, each by-value shim
-- (DESIGN.md §3.3) does what the call it wraps does, and the formatter's
-- three ways of returning output agree.
--
-- Smoke tests of the raw binding; the formatter goldens are
-- @ghostty-effectful@'s (M1.8).
module Test.Ghostty.FFI.Formatter (tests) where

import Control.Exception (bracket, finally)
import Data.ByteString qualified as BS
import Foreign.C.Types (CBool, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Ghostty.FFI.Allocator
import Ghostty.FFI.Formatter
import Ghostty.FFI.GridRef
import Ghostty.FFI.Io
import Ghostty.FFI.Layout (CStruct (..))
import Ghostty.FFI.Point
import Ghostty.FFI.Selection
import Ghostty.FFI.Terminal
import Ghostty.FFI.Types
import Test.Ghostty.FFI.Support
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Formatter"
    [ testCase "a new formatter starts with the library's defaults" $
        withTerminal 20 4 $ \t -> bracket (newFormatter t) ghosttyFormatterFree $ \f -> do
          getOpt f GhosttyFormatterOptEmit >>= (@?= GhosttyFormatterFormatPlain)
          getOpt f GhosttyFormatterOptUnwrap >>= (@?= (0 :: CBool))
          getOpt f GhosttyFormatterOptTrim >>= (@?= (1 :: CBool))
          getOpt f GhosttyFormatterOptExtras >>= (@?= GhosttyFormatterExtrasDefault)
          alloca $ \(sp :: Ptr GhosttySelection) ->
            ghosttyFormatterGet f GhosttyFormatterOptSelection (castPtr sp) >>= (@?= GhosttyNoValue),
      testCase "set puts an option, and a null value restores its default" $
        withTerminal 20 4 $ \t -> bracket (newFormatter t) ghosttyFormatterFree $ \f -> do
          setOpt f GhosttyFormatterOptExtras GhosttyFormatterExtrasNone
          getOpt f GhosttyFormatterOptExtras >>= (@?= GhosttyFormatterExtrasNone)
          ghosttyFormatterSet f GhosttyFormatterOptExtras nullPtr >>= (@?= GhosttySuccess)
          getOpt f GhosttyFormatterOptExtras >>= (@?= GhosttyFormatterExtrasDefault),
      testCase "the emit and trim options reach the library" $
        withTerminal 20 4 $ \t -> do
          feed t "\ESC[1mhello\ESC[0m   \r\nworld"
          withFormatter t GhosttyFormatterFormatPlain 1 nullPtr (bytesVia . ghosttyFormatterFormatBuf)
            >>= (@?= "hello\nworld")
          withFormatter t GhosttyFormatterFormatPlain 0 nullPtr (bytesVia . ghosttyFormatterFormatBuf)
            >>= \untrimmed -> ("hello   " `BS.isInfixOf` untrimmed) @?= True
          -- VT output keeps the bold.
          vt <- withFormatter t GhosttyFormatterFormatVt 1 nullPtr (bytesVia . ghosttyFormatterFormatBuf)
          ("\ESC[" `BS.isInfixOf` vt, "hello" `BS.isInfixOf` vt) @?= (True, True),
      testCase "format (shimmed), format_buf and format_alloc agree" $
        withTerminal 20 4 $ \t -> do
          feed t "one\r\ntwo"
          withFormatter t GhosttyFormatterFormatHtml 1 nullPtr $ \f -> do
            viaBuf <- bytesVia (ghosttyFormatterFormatBuf f)
            viaWriter <- collect (ghosttyFormatterFormat f)
            viaAlloc <- allocated (ghosttyFormatterFormatAlloc f nullPtr)
            (viaWriter, viaAlloc) @?= (viaBuf, viaBuf)
            ("one" `BS.isInfixOf` viaBuf) @?= True,
      testCase "the selection option formats a selection; buf and alloc agree" $
        withTerminal 20 4 $ \t -> do
          feed t "first\r\nsecond"
          sel <- selected (ghosttyTerminalSelectAll t)
          with sel $ \sp -> withFormatter t GhosttyFormatterFormatPlain 1 sp $ \f -> do
            viaBuf <- bytesVia (ghosttyFormatterFormatBuf f)
            viaAlloc <- allocated (ghosttyFormatterFormatAlloc f nullPtr)
            (viaBuf, viaAlloc) @?= ("first\nsecond", "first\nsecond"),
      testCase "the formatter copies the selection, and a null one clears it" $
        withTerminal 20 4 $ \t -> do
          feed t "alpha beta"
          ref <- gridRef t 7 0
          sel <- with (GhosttyTerminalSelectWordOptions (sizeOf' ghosttyTerminalSelectWordOptionsLayout) ref nullPtr 0) $ \op ->
            selected (ghosttyTerminalSelectWord t op)
          -- The selection's storage is gone before the format.
          f <- with sel $ \sp -> withFormatterOpen t GhosttyFormatterFormatPlain 1 sp
          flip finally (ghosttyFormatterFree f) $ do
            bytesVia (ghosttyFormatterFormatBuf f) >>= (@?= "beta")
            ghosttyFormatterSet f GhosttyFormatterOptSelection nullPtr >>= (@?= GhosttySuccess)
            bytesVia (ghosttyFormatterFormatBuf f) >>= (@?= "alpha beta"),
      testCase "selection_contains (shimmed) tests a point" $
        withTerminal 20 4 $ \t -> do
          feed t "alpha beta"
          ref <- gridRef t 1 0
          sel <- with (GhosttyTerminalSelectWordOptions (sizeOf' ghosttyTerminalSelectWordOptionsLayout) ref nullPtr 0) $ \op ->
            selected (ghosttyTerminalSelectWord t op)
          with sel $ \sp -> do
            contains t sp 4 >>= (@?= 1)
            contains t sp 7 >>= (@?= 0),
      testCase "select_output selects a command's output" $
        withTerminal 20 6 $ \t -> do
          feed t "\ESC]133;A\a$ \ESC]133;B\acmd\r\n\ESC]133;C\aout1\r\nout2\r\n\ESC]133;D;0\a\ESC]133;A\a$ "
          ref <- gridRef t 1 1
          sel <- with ref $ \rp -> selected (ghosttyTerminalSelectOutput t rp)
          with sel $ \sp ->
            withFormatter t GhosttyFormatterFormatPlain 1 sp (bytesVia . ghosttyFormatterFormatBuf)
              >>= (@?= "out1\nout2"),
      testCase "selection_equal and _ordered see a reversed selection as the same" $
        withTerminal 20 4 $ \t -> do
          feed t "some text"
          a <- gridRef t 0 0
          b <- gridRef t 3 0
          let forward = GhosttySelection a b 0
              backward = GhosttySelection b a 0
          with backward $ \bp -> do
            order <- alloca $ \op -> ghosttyTerminalSelectionOrder t bp op >>= (@?= GhosttySuccess) >> peek op
            order @?= GhosttySelectionOrderReverse
            ordered <- selected (ghosttyTerminalSelectionOrdered t bp GhosttySelectionOrderForward)
            with ordered $ \orp -> with forward $ \fp -> do
              equal <- alloca $ \ep -> ghosttyTerminalSelectionEqual t orp fp ep >>= (@?= GhosttySuccess) >> peek ep
              equal @?= (1 :: CBool)
    ]

-- * Helpers

-- | A sized struct's @size@ field, from its descriptor.
sizeOf' :: CStruct -> CSize
sizeOf' l = fromIntegral l.size

-- | A new formatter, every option at the library's default.
newFormatter :: GhosttyTerminal -> IO GhosttyFormatter
newFormatter t = alloca $ \fp -> do
  ghosttyFormatterTerminalNew nullPtr fp t >>= (@?= GhosttySuccess)
  peek fp

-- | Set an option from a value of its type.
setOpt :: (Storable a) => GhosttyFormatter -> GhosttyFormatterOption -> a -> IO ()
setOpt f o v = with v $ \vp -> ghosttyFormatterSet f o (castPtr vp) >>= (@?= GhosttySuccess)

-- | Read an option into a value of its type.
getOpt :: (Storable a) => GhosttyFormatter -> GhosttyFormatterOption -> IO a
getOpt f o = alloca $ \vp -> ghosttyFormatterGet f o (castPtr vp) >>= (@?= GhosttySuccess) >> peek vp

-- | A formatter with the format and trim given, no unwrapping and no extras,
-- over a selection (null for the whole screen). The caller frees it.
withFormatterOpen :: GhosttyTerminal -> GhosttyFormatterFormat -> CBool -> Ptr GhosttySelection -> IO GhosttyFormatter
withFormatterOpen t emit trim sel = do
  f <- newFormatter t
  setOpt f GhosttyFormatterOptEmit emit
  setOpt f GhosttyFormatterOptUnwrap (0 :: CBool)
  setOpt f GhosttyFormatterOptTrim trim
  setOpt f GhosttyFormatterOptExtras GhosttyFormatterExtrasNone
  ghosttyFormatterSet f GhosttyFormatterOptSelection (castPtr sel) >>= (@?= GhosttySuccess)
  pure f

-- | 'withFormatterOpen' for the length of an action.
withFormatter :: GhosttyTerminal -> GhosttyFormatterFormat -> CBool -> Ptr GhosttySelection -> (GhosttyFormatter -> IO a) -> IO a
withFormatter t emit trim sel = bracket (withFormatterOpen t emit trim sel) ghosttyFormatterFree

-- | What a call writes to a Haskell 'GhosttyWriter'.
collect :: (Ptr GhosttyWriter -> IO GhosttyResult) -> IO ByteString
collect call = do
  out <- newIORef BS.empty
  let onWrite _ p n = do
        BS.packCStringLen (castPtr p, fromIntegral n) >>= \b -> modifyIORef' out (<> b)
        pure 1
  withCallback (mkGhosttyWriterFn onWrite) $ \fn ->
    with (GhosttyWriter fn nullPtr) call >>= (@?= GhosttySuccess)
  readIORef out

-- | What a @…_alloc@ call returns, copied out and freed.
allocated :: (Ptr (Ptr Word8) -> Ptr CSize -> IO GhosttyResult) -> IO ByteString
allocated call = alloca $ \pp -> alloca $ \np -> do
  call pp np >>= (@?= GhosttySuccess)
  p <- peek pp
  n <- peek np
  b <- if p == nullPtr then pure BS.empty else BS.packCStringLen (castPtr p, fromIntegral n)
  ghosttyFree nullPtr p n
  pure b

-- | The selection a call writes.
selected :: (Ptr GhosttySelection -> IO GhosttyResult) -> IO GhosttySelection
selected call = withZeroed ghosttySelectionLayout $ \sp -> do
  call sp >>= (@?= GhosttySuccess)
  peek sp

-- | An untracked reference to an active-area cell.
gridRef :: GhosttyTerminal -> Word16 -> Word32 -> IO GhosttyGridRef
gridRef t x y =
  with (GhosttyPoint GhosttyPointTagActive (GhosttyPointCoordinate x y)) $ \pp ->
    withZeroed ghosttyGridRefLayout $ \rp -> do
      ghosttyTerminalGridRef t pp rp >>= (@?= GhosttySuccess)
      peek rp

-- | Whether an active-area cell of row 0 is in a selection.
contains :: GhosttyTerminal -> Ptr GhosttySelection -> Word16 -> IO CBool
contains t sp x =
  with (GhosttyPoint GhosttyPointTagActive (GhosttyPointCoordinate x 0)) $ \pp -> alloca $ \cp -> do
    ghosttyTerminalSelectionContains t sp pp cp >>= (@?= GhosttySuccess)
    peek cp
