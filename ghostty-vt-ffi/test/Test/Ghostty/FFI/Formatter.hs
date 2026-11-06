-- | The formatter and selection bindings against the linked library: each
-- by-value shim (DESIGN.md §3.3) does what the call it wraps does, and the
-- formatter's three ways of returning output agree.
--
-- Smoke tests of the raw binding; the formatter goldens are
-- @ghostty-effectful@'s (M1.8).
module Test.Ghostty.FFI.Formatter (tests) where

import Control.Exception (bracket)
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
    [ testCase "terminal_new (shimmed) honours its options" $
        withTerminal 20 4 $ \t -> do
          feed t "\ESC[1mhello\ESC[0m   \r\nworld"
          withFormatter t (options GhosttyFormatterFormatPlain 1) (bytesVia . ghosttyFormatterFormatBuf)
            >>= (@?= "hello\nworld")
          -- The emit option reaches the library: VT output keeps the bold.
          vt <- withFormatter t (options GhosttyFormatterFormatVt 1) (bytesVia . ghosttyFormatterFormatBuf)
          ("\ESC[" `BS.isInfixOf` vt, "hello" `BS.isInfixOf` vt) @?= (True, True),
      testCase "format (shimmed), format_buf and format_alloc agree" $
        withTerminal 20 4 $ \t -> do
          feed t "one\r\ntwo"
          withFormatter t (options GhosttyFormatterFormatHtml 1) $ \f -> do
            viaBuf <- bytesVia (ghosttyFormatterFormatBuf f)
            viaWriter <- collect (ghosttyFormatterFormat f)
            viaAlloc <- allocated (ghosttyFormatterFormatAlloc f nullPtr)
            (viaWriter, viaAlloc) @?= (viaBuf, viaBuf)
            ("one" `BS.isInfixOf` viaBuf) @?= True,
      testCase "selection_format_buf and _alloc (shimmed) format a selection" $
        withTerminal 20 4 $ \t -> do
          feed t "first\r\nsecond"
          sel <- selected (ghosttyTerminalSelectAll t)
          with sel $ \sp -> do
            let opts = GhosttyTerminalSelectionFormatOptions (sizeOf' ghosttyTerminalSelectionFormatOptionsLayout) GhosttyFormatterFormatPlain 1 1 sp
            viaBuf <- with opts $ \op -> bytesVia (ghosttyTerminalSelectionFormatBuf t op)
            viaAlloc <- with opts $ \op -> allocated (ghosttyTerminalSelectionFormatAlloc t nullPtr op)
            (viaBuf, viaAlloc) @?= ("first\nsecond", "first\nsecond"),
      testCase "a formatter's selection option restricts it to the selection" $
        withTerminal 20 4 $ \t -> do
          feed t "alpha beta"
          ref <- gridRef t 7 0
          sel <- with (GhosttyTerminalSelectWordOptions (sizeOf' ghosttyTerminalSelectWordOptionsLayout) ref nullPtr 0) $ \op ->
            selected (ghosttyTerminalSelectWord t op)
          with sel $ \sp ->
            withFormatter t (optionsOver GhosttyFormatterFormatPlain 1 sp) (bytesVia . ghosttyFormatterFormatBuf)
              >>= (@?= "beta"),
      testCase "selection_contains (shimmed) tests a point" $
        withTerminal 20 4 $ \t -> do
          feed t "alpha beta"
          ref <- gridRef t 1 0
          sel <- with (GhosttyTerminalSelectWordOptions (sizeOf' ghosttyTerminalSelectWordOptionsLayout) ref nullPtr 0) $ \op ->
            selected (ghosttyTerminalSelectWord t op)
          with sel $ \sp -> do
            contains t sp 4 >>= (@?= 1)
            contains t sp 7 >>= (@?= 0),
      testCase "select_output (shimmed) selects a command's output" $
        withTerminal 20 6 $ \t -> do
          feed t "\ESC]133;A\a$ \ESC]133;B\acmd\r\n\ESC]133;C\aout1\r\nout2\r\n\ESC]133;D;0\a\ESC]133;A\a$ "
          ref <- gridRef t 1 1
          sel <- with ref $ \rp -> selected (ghosttyTerminalSelectOutput t rp)
          with sel $ \sp ->
            let opts = GhosttyTerminalSelectionFormatOptions (sizeOf' ghosttyTerminalSelectionFormatOptionsLayout) GhosttyFormatterFormatPlain 1 1 sp
             in with opts (bytesVia . ghosttyTerminalSelectionFormatBuf t) >>= (@?= "out1\nout2"),
      testCase "selection_equal and _ordered see a reversed selection as the same" $
        withTerminal 20 4 $ \t -> do
          feed t "some text"
          a <- gridRef t 0 0
          b <- gridRef t 3 0
          let forward = GhosttySelection (sizeOf' ghosttySelectionLayout) a b 0
              backward = GhosttySelection (sizeOf' ghosttySelectionLayout) b a 0
          with backward $ \bp -> do
            order <- alloca $ \op -> ghosttyTerminalSelectionOrder t bp op >>= (@?= GhosttySuccess) >> peek op
            order @?= GhosttySelectionOrderReverse
            ordered <- selected (ghosttyTerminalSelectionOrdered t bp GhosttySelectionOrderForward)
            with ordered $ \orp -> with forward $ \fp -> do
              equal <- alloca $ \ep -> ghosttyTerminalSelectionEqual t orp fp ep >>= (@?= GhosttySuccess) >> peek ep
              equal @?= (1 :: CBool)
    ]

-- * Helpers

-- | Formatter options: the format and whether to trim, with no extras, over
-- the whole screen.
options :: GhosttyFormatterFormat -> CBool -> GhosttyFormatterTerminalOptions
options emit trim = optionsOver emit trim nullPtr

-- | 'options', restricted to a selection (null for the whole screen).
optionsOver :: GhosttyFormatterFormat -> CBool -> Ptr GhosttySelection -> GhosttyFormatterTerminalOptions
optionsOver emit trim selection =
  GhosttyFormatterTerminalOptions
    { size = sizeOf' ghosttyFormatterTerminalOptionsLayout,
      emit,
      unwrap = 0,
      trim,
      extra = GhosttyFormatterTerminalExtra (sizeOf' ghosttyFormatterTerminalExtraLayout) 0 0 0 0 0 0 screen,
      selection
    }
  where
    screen = GhosttyFormatterScreenExtra (sizeOf' ghosttyFormatterScreenExtraLayout) 0 0 0 0 0 0

-- | A sized struct's @size@ field, from its descriptor.
sizeOf' :: CStruct -> CSize
sizeOf' l = fromIntegral l.size

-- | A formatter over a terminal for the length of an action.
withFormatter :: GhosttyTerminal -> GhosttyFormatterTerminalOptions -> (GhosttyFormatter -> IO a) -> IO a
withFormatter t opts =
  bracket
    ( with opts $ \op -> alloca $ \fp -> do
        ghosttyFormatterTerminalNew nullPtr fp t op >>= (@?= GhosttySuccess)
        peek fp
    )
    ghosttyFormatterFree

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
selected call = withSized ghosttySelectionLayout $ \sp -> do
  call sp >>= (@?= GhosttySuccess)
  peek sp

-- | An untracked reference to an active-area cell.
gridRef :: GhosttyTerminal -> Word16 -> Word32 -> IO GhosttyGridRef
gridRef t x y =
  with (GhosttyPoint GhosttyPointTagActive (GhosttyPointCoordinate x y)) $ \pp ->
    withSized ghosttyGridRefLayout $ \rp -> do
      ghosttyTerminalGridRef t pp rp >>= (@?= GhosttySuccess)
      peek rp

-- | Whether an active-area cell of row 0 is in a selection.
contains :: GhosttyTerminal -> Ptr GhosttySelection -> Word16 -> IO CBool
contains t sp x =
  with (GhosttyPoint GhosttyPointTagActive (GhosttyPointCoordinate x 0)) $ \pp -> alloca $ \cp -> do
    ghosttyTerminalSelectionContains t sp pp cp >>= (@?= GhosttySuccess)
    peek cp
