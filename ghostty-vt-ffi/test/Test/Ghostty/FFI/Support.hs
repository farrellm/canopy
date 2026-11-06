-- | What the behavioural tests of the raw binding share: a terminal for the
-- length of a test, callbacks freed after it, feeding bytes, and sized
-- structs initialised as @GHOSTTY_INIT_SIZED@ does.
module Test.Ghostty.FFI.Support
  ( withTerminal,
    withCallback,
    feed,
    pokeSized,
    withSized,
    bytesVia,
  )
where

import Control.Exception (bracket, finally)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Foreign.C.Types (CSize)
import Foreign.Marshal.Alloc (alloca, allocaBytesAligned)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Ghostty.FFI.Layout (CStruct (..))
import Ghostty.FFI.Terminal
import Ghostty.FFI.Types
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

-- | A terminal of the given size, freed when the test ends.
withTerminal :: Word16 -> Word16 -> (GhosttyTerminal -> Assertion) -> Assertion
withTerminal cols rows act = do
  t <- alloca $ \tp -> do
    ghosttyTerminalNew nullPtr tp cols rows >>= (@?= GhosttySuccess)
    peek tp
  act t `finally` ghosttyTerminalFree t

-- | A Haskell callback, freed when the action ends.
withCallback :: IO (FunPtr a) -> (FunPtr a -> IO b) -> IO b
withCallback mk = bracket mk freeHaskellFunPtr

-- | Write bytes to a terminal.
feed :: GhosttyTerminal -> ByteString -> IO ()
feed t b = BSU.unsafeUseAsCStringLen b $ \(p, n) -> ghosttyTerminalVtWrite t (castPtr p) (fromIntegral n)

-- | Zero a sized struct and set its leading @size_t size@, as
-- @GHOSTTY_INIT_SIZED@ does, from its layout descriptor.
pokeSized :: CStruct -> Ptr a -> IO ()
pokeSized l p = do
  fillBytes p 0 l.size
  poke (castPtr p) (fromIntegral l.size :: CSize)

-- | Room for a sized struct, initialised by 'pokeSized'.
withSized :: CStruct -> (Ptr a -> IO b) -> IO b
withSized l act = allocaBytesAligned l.size l.align $ \p -> pokeSized l p >> act (castPtr p)

-- | Call a @…_buf@ function twice, as its protocol says: with a null buffer
-- for the size, which must be @GHOSTTY_OUT_OF_SPACE@ (or success when the
-- output is empty), then with a buffer of that size.
bytesVia :: (Ptr Word8 -> CSize -> Ptr CSize -> IO GhosttyResult) -> IO ByteString
bytesVia call = alloca $ \np -> do
  call nullPtr 0 np >>= \case
    GhosttySuccess -> pure BS.empty
    GhosttyOutOfSpace -> do
      n <- peek np
      allocaArray (fromIntegral n) $ \buf -> do
        call buf n np >>= (@?= GhosttySuccess)
        written <- peek np
        BS.packCStringLen (castPtr buf, fromIntegral written)
    other -> assertFailure ("size query: " <> show other)
