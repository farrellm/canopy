-- | The terminal binding against the linked library: each by-value shim
-- (DESIGN.md §3.3) does what the call it wraps does, Haskell callbacks run
-- from the @safe@ calls, and @vt_write_until@ stops where the header says.
--
-- These are smoke tests of the raw binding. The behaviour canopy relies on
-- is tested through @ghostty-effectful@.
module Test.Ghostty.FFI.Terminal (tests) where

import Control.Exception (bracket, finally)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Foreign.C.Types (CBool, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (fillBytes, with)
import Foreign.Ptr (FunPtr, Ptr, castFunPtrToPtr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Ghostty.FFI.GridRef
import Ghostty.FFI.Io
import Ghostty.FFI.Layout (CStruct (..))
import Ghostty.FFI.Modes
import Ghostty.FFI.Point
import Ghostty.FFI.Terminal
import Ghostty.FFI.Terminal.Callbacks
import Ghostty.FFI.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Terminal"
    [ testCase "new reports its size, and writes move the cursor" $
        withTerminal 80 24 $ \t -> do
          getU16 t GhosttyTerminalDataCols >>= (@?= 80)
          getU16 t GhosttyTerminalDataRows >>= (@?= 24)
          feed t "hello"
          getU16 t GhosttyTerminalDataCursorX >>= (@?= 5),
      testCase "a Haskell callback runs from a safe write" $
        withTerminal 80 24 $ \t -> do
          bells <- newIORef (0 :: Int)
          withCallback (mkGhosttyTerminalBellFn (\_ _ -> modifyIORef' bells (+ 1))) $ \fn -> do
            setFn t GhosttyTerminalOptBell fn
            feed t "a\a\ab"
          readIORef bells >>= (@?= 2),
      testCase "a query's reply arrives through WRITE_PTY" $
        withTerminal 80 24 $ \t -> do
          replies <- newIORef BS.empty
          let onWrite _ _ p n = BS.packCStringLen (castPtr p, fromIntegral n) >>= \b -> modifyIORef' replies (<> b)
          withCallback (mkGhosttyTerminalWritePtyFn onWrite) $ \fn -> do
            setFn t GhosttyTerminalOptWritePty fn
            feed t "ab\ESC[6n"
          readIORef replies >>= (@?= "\ESC[1;3R"),
      testCase "vt_write_until stops after an OSC's terminator" $
        withTerminal 80 24 $ \t -> do
          let bytes = "ab\ESC]2;t\acd" :: ByteString
          (r, stop) <- writeUntil t bytes GhosttyTerminalBoundaryOscEnd
          r @?= GhosttySuccess
          stop.consumed @?= 8
          stop.reached @?= boundaryBit GhosttyTerminalBoundaryOscEnd
          (stop.kind, stop.end, stop.byte) @?= (GhosttyTerminalStringKindOsc, GhosttyTerminalStringEndBel, 0x07)
          (r', stop') <- writeUntil t (BS.drop 8 bytes) GhosttyTerminalBoundaryOscEnd
          (r', stop'.consumed, stop'.reached) @?= (GhosttyNoValue, 2, 0)
          getU16 t GhosttyTerminalDataCursorX >>= (@?= 4),
      testCase "grid_ref (shimmed) and point_from_grid_ref are inverse" $
        withTerminal 80 24 $ \t -> do
          feed t "xyz"
          let at = GhosttyPoint GhosttyPointTagActive (GhosttyPointCoordinate 1 0)
          ref <- with at $ \pp -> alloca $ \rp -> do
            pokeSized ghosttyGridRefLayout rp
            ghosttyTerminalGridRef t pp rp >>= (@?= GhosttySuccess)
            peek rp
          with ref $ \rp -> do
            alloca $ \cp -> do
              ghosttyTerminalPointFromGridRef t rp GhosttyPointTagActive cp >>= (@?= GhosttySuccess)
              peek cp >>= (@?= GhosttyPointCoordinate 1 0)
            allocaArray 4 $ \buf -> alloca $ \np -> do
              ghosttyGridRefGraphemes rp buf 4 np >>= (@?= GhosttySuccess)
              n <- peek np
              peekArray (fromIntegral n) buf >>= (@?= [fromIntegral (ord 'y')]),
      testCase "a tracked ref (shimmed) follows its cell into scrollback, and moves" $
        withTerminal 10 3 $ \t -> do
          feed t "top"
          let origin = GhosttyPoint GhosttyPointTagActive (GhosttyPointCoordinate 0 0)
          tracked <- with origin $ \pp -> alloca $ \tp -> do
            ghosttyTerminalGridRefTrack t pp tp >>= (@?= GhosttySuccess)
            peek tp
          feed t "\r\n1\r\n2\r\n3\r\n4"
          trackedPoint tracked GhosttyPointTagActive >>= (@?= Nothing)
          trackedPoint tracked GhosttyPointTagScreen >>= (@?= Just (GhosttyPointCoordinate 0 0))
          with origin (ghosttyTrackedGridRefSet tracked t) >>= (@?= GhosttySuccess)
          trackedPoint tracked GhosttyPointTagScreen >>= (@?= Just (GhosttyPointCoordinate 0 2))
          ghosttyTrackedGridRefFree tracked,
      testCase "scroll_viewport (shimmed) leaves and rejoins the active area" $
        withTerminal 10 3 $ \t -> do
          feed t "0\r\n1\r\n2\r\n3\r\n4\r\n5"
          getBool t GhosttyTerminalDataViewportActive >>= (@?= True)
          with ScrollViewportTop (ghosttyTerminalScrollViewport t)
          getBool t GhosttyTerminalDataViewportActive >>= (@?= False)
          with (ScrollViewportDelta 100) (ghosttyTerminalScrollViewport t)
          getBool t GhosttyTerminalDataViewportActive >>= (@?= True),
      testCase "continuation_write (shimmed) agrees with continuation_buf" $
        withTerminal 80 24 $ \t -> do
          with (64 :: CSize) $ setValue t GhosttyTerminalOptContinuationMaxBytes
          feed t "ok\ESC]2;unfinish"
          written <- newIORef BS.empty
          let onWrite _ p n = do
                BS.packCStringLen (castPtr p, fromIntegral n) >>= \b -> modifyIORef' written (<> b)
                pure 1
          withCallback (mkGhosttyWriterFn onWrite) $ \fn ->
            with (GhosttyWriter fn nullPtr) (ghosttyTerminalContinuationWrite t) >>= (@?= GhosttySuccess)
          viaWriter <- readIORef written
          viaBuf <- allocaArray 64 $ \buf -> alloca $ \np -> do
            ghosttyTerminalContinuationBuf t buf 64 np >>= (@?= GhosttySuccess)
            n <- peek np
            BS.packCStringLen (castPtr buf, fromIntegral n)
          viaWriter @?= viaBuf
          viaWriter @?= "\ESC]2;unfinish",
      testCase "a mode reads back through ModeConfig, and packs as the header does" $
        withTerminal 80 24 $ \t -> do
          ghosttyModeNew 2004 0 @?= GhosttyModeBracketedPaste
          (ghosttyModeValue GhosttyModeInsert, ghosttyModeAnsi GhosttyModeInsert) @?= (4, 1)
          getMode t GhosttyModeBracketedPaste >>= (@?= 0)
          feed t "\ESC[?2004h"
          getMode t GhosttyModeBracketedPaste >>= (@?= 1)
    ]

-- * Helpers

withTerminal :: Word16 -> Word16 -> (GhosttyTerminal -> Assertion) -> Assertion
withTerminal cols rows act = do
  t <- alloca $ \tp -> do
    ghosttyTerminalNew nullPtr tp cols rows >>= (@?= GhosttySuccess)
    peek tp
  act t `finally` ghosttyTerminalFree t

withCallback :: IO (FunPtr a) -> (FunPtr a -> IO b) -> IO b
withCallback mk = bracket mk freeHaskellFunPtr

feed :: GhosttyTerminal -> ByteString -> IO ()
feed t b = BSU.unsafeUseAsCStringLen b $ \(p, n) -> ghosttyTerminalVtWrite t (castPtr p) (fromIntegral n)

writeUntil :: GhosttyTerminal -> ByteString -> GhosttyTerminalBoundary -> IO (GhosttyResult, GhosttyTerminalWriteStop)
writeUntil t b boundary =
  BSU.unsafeUseAsCStringLen b $ \(p, n) -> alloca $ \sp -> do
    pokeSized ghosttyTerminalWriteStopLayout sp
    r <- ghosttyTerminalVtWriteUntil t (castPtr p) (fromIntegral n) (boundaryBit boundary) sp
    (r,) <$> peek sp

boundaryBit :: GhosttyTerminalBoundary -> Word32
boundaryBit (GhosttyTerminalBoundary b) = fromIntegral b

-- | Zero a sized struct and set its leading @size_t size@, as
-- @GHOSTTY_INIT_SIZED@ does, from its layout descriptor.
pokeSized :: CStruct -> Ptr a -> IO ()
pokeSized l p = do
  fillBytes p 0 l.size
  poke (castPtr p) (fromIntegral l.size :: CSize)

setFn :: GhosttyTerminal -> GhosttyTerminalOption -> FunPtr a -> IO ()
setFn t o fn = ghosttyTerminalSet t o (castFunPtrToPtr fn) >>= (@?= GhosttySuccess)

setValue :: GhosttyTerminal -> GhosttyTerminalOption -> Ptr a -> IO ()
setValue t o p = ghosttyTerminalSet t o (castPtr p) >>= (@?= GhosttySuccess)

getValue :: (Storable a) => GhosttyTerminal -> GhosttyTerminalData -> IO a
getValue t d = alloca $ \p -> do
  ghosttyTerminalGet t d (castPtr p) >>= (@?= GhosttySuccess)
  peek p

getU16 :: GhosttyTerminal -> GhosttyTerminalData -> IO Word16
getU16 = getValue

getBool :: GhosttyTerminal -> GhosttyTerminalData -> IO Bool
getBool t d = (/= (0 :: CBool)) <$> getValue t d

getMode :: GhosttyTerminal -> GhosttyMode -> IO CBool
getMode t m = with (GhosttyTerminalModeConfig m 0) $ \p -> do
  ghosttyTerminalGet t GhosttyTerminalDataMode (castPtr p) >>= (@?= GhosttySuccess)
  (.value) <$> peek p

trackedPoint :: GhosttyTrackedGridRef -> GhosttyPointTag -> IO (Maybe GhosttyPointCoordinate)
trackedPoint r tag = alloca $ \cp ->
  ghosttyTrackedGridRefPoint r tag cp >>= \case
    GhosttySuccess -> Just <$> peek cp
    GhosttyNoValue -> pure Nothing
    other -> assertFailure ("tracked_grid_ref_point: " <> show other)
