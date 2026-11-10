{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}

-- | A terminal's representation and its raw calls (DESIGN.md §4.2). The
-- public API is "Ghostty.Effectful.Terminal", which exports these types
-- without their constructors; the calls here are what the 'Ghostty'
-- interpreter runs, after its owner-thread check.
--
-- A terminal owns the callbacks the binding installed on it, the buffer they
-- append to, and the answers its query callbacks give
-- ("Ghostty.Effectful.Internal.Callbacks"). It also owns the tracked grid
-- references made from it, in a registry keyed by number. A 'TrackedRef'
-- names its entry, not the C handle, so a reference that was untracked, or
-- whose terminal was freed, is never passed to the library again: it finds
-- no entry.
module Ghostty.Effectful.Internal.Terminal
  ( -- * Handles
    Terminal (..),
    TrackedRef (..),

    -- * Configuration
    TerminalConfig (..),
    terminalConfig,

    -- * Calls
    openTerminal,
    closeTerminal,
    wrote,
    forgetRegion,

    -- * Points
    pastHistory,
    unlessPastHistory,

    -- * Tracked references
    trackPoint,
    untrackRef,
    refCoordinate,
    moveTrackedRef,
    refHasValue,
  )
where

import Control.Exception (onException)
import Data.IntMap.Strict qualified as IntMap
import Foreign.C.Types (CSize)
import Foreign.Marshal.Utils (toBool, with)
import Foreign.Ptr (castPtr)
import Ghostty.Effectful.Error (CallSite (..), ResultCode (..))
import Ghostty.Effectful.Internal.Callbacks
  ( Answers (..),
    Installed,
    Pending,
    installCallbacks,
    releaseCallbacks,
  )
import Ghostty.Effectful.Internal.Error (newHandle, optionalVia, outVia, succeedsUnless)
import Ghostty.Effectful.Internal.Size (CellPixels (..), GridSize (..), fromExtent, noCellPixels)
import Ghostty.Effectful.Terminal.Keys (ColorScheme, DeviceAttributes, defaultDeviceAttributes)
import Ghostty.FFI.GridRef
  ( GhosttyTrackedGridRef,
    ghosttyTrackedGridRefFree,
    ghosttyTrackedGridRefHasValue,
    ghosttyTrackedGridRefPoint,
    ghosttyTrackedGridRefSet,
  )
import Ghostty.FFI.Point
  ( GhosttyPoint (..),
    GhosttyPointCoordinate (..),
    GhosttyPointTag,
    pattern GhosttyPointTagActive,
    pattern GhosttyPointTagHistory,
  )
import Ghostty.FFI.Terminal
  ( GhosttyTerminal,
    ghosttyTerminalFree,
    ghosttyTerminalGet,
    ghosttyTerminalGridRefTrack,
    ghosttyTerminalNew,
    pattern GhosttyTerminalDataScrollbackRows,
  )

-- | A terminal, alive in the region @s@ that
-- 'Ghostty.Effectful.Terminal.withTerminal' opens. The type variable is
-- rank-2 there, as in @ST@, so a 'Terminal' cannot be used after its region
-- has freed it. As @ST@'s, its role is nominal: were it phantom, 'coerce'
-- could take a terminal out of its region (regression 14).
type Terminal :: Type -> Type

type role Terminal nominal

data Terminal s = Terminal
  { -- | the library's handle
    handle :: GhosttyTerminal,
    -- | the tracked references made from it and not yet untracked
    refs :: IORef (IntMap GhosttyTrackedGridRef),
    -- | the next registry key
    nextRef :: IORef Int,
    -- | what the callbacks appended since it was last drained, newest first
    events :: IORef [Pending],
    -- | what the query callbacks answer with
    answers :: IORef Answers,
    -- | the callbacks, freed with the terminal
    installed :: Installed,
    -- | the interpreter's count of live tracked references, across its
    -- terminals ('Ghostty.Effectful.Internal.Effect.refCount')
    liveRefs :: IORef Int,
    -- | how many calls have changed the terminal or its viewport: a frame
    -- read when this was one value describes the terminal only while it
    -- still is. Every operation that writes, resizes, resets, sets an
    -- option or scrolls the viewport counts ('wrote').
    writes :: IORef Word64
  }

-- | A tracked reference to a cell of a terminal in the region @s@: it
-- follows its cell as the terminal scrolls, prunes and reflows, and is
-- freed with the terminal if not before. Its role is nominal, as
-- 'Terminal''s is.
type role TrackedRef nominal

data TrackedRef s = TrackedRef
  { -- | the terminal it was made from
    terminal :: Terminal s,
    -- | its key in the terminal's registry
    key :: Int
  }

instance Eq (TrackedRef s) where
  a == b = a.key == b.key

-- | How a terminal starts, and how it answers the program's queries
-- (DESIGN.md §4.5). Each answer can be changed later with
-- 'Ghostty.Effectful.Terminal.setT', through the option named beside it.
data TerminalConfig = TerminalConfig
  { -- | the grid
    size :: GridSize,
    -- | one cell, in pixels
    cellPixels :: CellPixels,
    -- | the answer to a colour scheme query; 'Nothing' leaves it unanswered
    -- ('SetColorScheme')
    colorScheme :: Maybe ColorScheme,
    -- | the answers to device attribute queries ('SetDeviceAttributes')
    deviceAttributes :: DeviceAttributes,
    -- | the reply to ENQ; empty sends none ('SetEnquiry')
    enquiry :: ByteString,
    -- | the name XTVERSION reports; empty reports @libghostty@
    -- ('SetXtversion')
    xtversion :: Text,
    -- | whether size queries are answered ('SetSize')
    reportSize :: Bool,
    -- | whether clipboard writes are accepted ('SetClipboardWrite')
    acceptClipboardWrites :: Bool
  }
  deriving stock (Eq, Show)

-- | A terminal of the given grid, with the cell size unknown, answering
-- size queries and device attributes as the library would, and nothing
-- else: no colour scheme, no ENQ reply, the library's XTVERSION, and no
-- clipboard writes.
terminalConfig :: GridSize -> TerminalConfig
terminalConfig g =
  TerminalConfig
    { size = g,
      cellPixels = noCellPixels,
      colorScheme = Nothing,
      deviceAttributes = defaultDeviceAttributes,
      enquiry = "",
      xtversion = "",
      reportSize = True,
      acceptClipboardWrites = False
    }

-- | @ghostty_terminal_new@ with the default allocator (§3.3), at the
-- configured grid, with an empty registry and every callback installed. The
-- cell size is the caller's to set, inside the scope that frees the
-- terminal on every path. If the callbacks cannot be installed, the terminal
-- is freed and the error thrown.
openTerminal :: IORef Int -> TerminalConfig -> IO (Terminal s)
openTerminal liveRefs cfg = do
  handle <-
    newHandle (CallSite "ghostty_terminal_new") $ \a out ->
      ghosttyTerminalNew a out (fromExtent cfg.size.cols) (fromExtent cfg.size.rows)
  refs <- newIORef mempty
  nextRef <- newIORef 0
  events <- newIORef []
  answers <-
    newIORef
      Answers
        { scheme = cfg.colorScheme,
          attributes = cfg.deviceAttributes,
          sizeQueries = cfg.reportSize,
          clipboard = cfg.acceptClipboardWrites,
          grid = cfg.size,
          pixels = noCellPixels
        }
  installed <-
    installCallbacks handle events answers cfg.enquiry (encodeUtf8 cfg.xtversion)
      `onException` ghosttyTerminalFree handle
  writes <- newIORef 0
  pure Terminal {handle, refs, nextRef, events, answers, installed, liveRefs, writes}

-- | Count a call that changed the terminal (§4.3: a frame's hyperlinks are
-- read from the terminal only while it has not changed since).
wrote :: Terminal s -> IO ()
wrote t = modifyIORef' t.writes (+ 1)

-- | The same terminal, its region forgotten, for a handle scoped inside the
-- region that keeps it (a render state). The record is rebuilt, since the
-- region's role is nominal and 'coerce' cannot change it.
forgetRegion :: Terminal s -> Terminal ()
forgetRegion t =
  Terminal
    { handle = t.handle,
      refs = t.refs,
      nextRef = t.nextRef,
      events = t.events,
      answers = t.answers,
      installed = t.installed,
      liveRefs = t.liveRefs,
      writes = t.writes
    }

-- | Free every tracked reference still registered, taking their number off
-- the count, then @ghostty_terminal_free@, then the callbacks, which the
-- terminal can no longer call.
closeTerminal :: Terminal s -> IO ()
closeTerminal t = do
  -- The registry's reads and writes here, in 'trackPoint' and in
  -- 'untrackRef' are plain, not atomicModifyIORef': only the owner thread
  -- touches a terminal (DESIGN.md §4.6). This runs as 'bracket''s release,
  -- masked, so no asynchronous exception falls between the read and the
  -- write and frees a reference twice.
  live <- readIORef t.refs
  writeIORef t.refs mempty
  traverse_ ghosttyTrackedGridRefFree live
  modifyIORef' t.liveRefs (subtract (IntMap.size live))
  ghosttyTerminalFree t.handle
  releaseCallbacks t.installed

-- | Whether a point is a scrollback row past the end of the scrollback.
-- The library does not refuse one (at the pin, @PageList.pin@ checks a
-- history row only against the whole screen): it names the active row that
-- many rows down. So every call here that takes a point checks this first,
-- and treats such a point as the outside of its coordinate system that it
-- is. Regression 6.
pastHistory :: GhosttyTerminal -> GhosttyPoint -> IO Bool
pastHistory h p
  | p.tag == GhosttyPointTagHistory = (\n -> fromIntegral p.coordinate.y >= n) <$> scrollbackRows
  | otherwise = pure False
  where
    scrollbackRows :: IO CSize
    scrollbackRows =
      outVia (CallSite "ghostty_terminal_get GHOSTTY_TERMINAL_DATA_SCROLLBACK_ROWS") $
        ghosttyTerminalGet h GhosttyTerminalDataScrollbackRows . castPtr

-- | The action's result for a point not past the scrollback
-- ('pastHistory'), and the given value for one that is.
unlessPastHistory :: GhosttyTerminal -> GhosttyPoint -> a -> IO a -> IO a
unlessPastHistory h p past = ifM (pastHistory h p) (pure past)

-- | @ghostty_terminal_grid_ref_track@: a tracked reference to the cell at a
-- point of the active screen, registered with the terminal and counted;
-- 'Nothing' if the point is outside its coordinate system, which is the
-- header's @GHOSTTY_INVALID_VALUE@ for a valid terminal, or is past the
-- scrollback ('pastHistory').
trackPoint :: Terminal s -> GhosttyPoint -> IO (Maybe (TrackedRef s))
trackPoint t p = unlessPastHistory t.handle p Nothing $ do
  made <-
    with p $ \pp ->
      optionalVia ResultInvalidValue (CallSite "ghostty_terminal_grid_ref_track") $
        ghosttyTerminalGridRefTrack t.handle pp
  forM made $ \ref -> do
    -- Owner thread only (§4.6, as in 'closeTerminal'), so the key is not
    -- taken twice.
    key <- readIORef t.nextRef
    writeIORef t.nextRef (key + 1)
    modifyIORef' t.refs (IntMap.insert key ref)
    modifyIORef' t.liveRefs (+ 1)
    pure TrackedRef {terminal = t, key}

-- | Free a reference and drop it from its terminal's registry. A reference
-- already untracked, or whose terminal is closed, is left alone.
untrackRef :: TrackedRef s -> IO ()
untrackRef ref = do
  -- Owner thread only (§4.6, as in 'closeTerminal'): no other call changes
  -- the registry between this read and the write below.
  (found, rest) <- IntMap.updateLookupWithKey (\_ _ -> Nothing) ref.key <$> readIORef ref.terminal.refs
  for_ found $ \h -> do
    writeIORef ref.terminal.refs rest
    ghosttyTrackedGridRefFree h
    modifyIORef' ref.terminal.liveRefs (subtract 1)

-- | The library's handle for a reference still registered.
registered :: TrackedRef s -> IO (Maybe GhosttyTrackedGridRef)
registered ref = IntMap.lookup ref.key <$> readIORef ref.terminal.refs

-- | @ghostty_tracked_grid_ref_point@: the reference's position in a
-- coordinate system; 'Nothing' if it has lost its cell, has no position
-- there, or was untracked.
--
-- The library gives a cell of the active area a scrollback row past the
-- scrollback (its screen row), the converse of 'pastHistory'. A cell with
-- an active position is not in the scrollback, so it has no scrollback
-- position here. Regression 7.
refCoordinate :: TrackedRef s -> GhosttyPointTag -> IO (Maybe GhosttyPointCoordinate)
refCoordinate ref tag
  | tag == GhosttyPointTagHistory =
      ifM (isJust <$> rawCoordinate ref GhosttyPointTagActive) (pure Nothing) (rawCoordinate ref tag)
  | otherwise = rawCoordinate ref tag

-- | 'refCoordinate' as the library answers it.
rawCoordinate :: TrackedRef s -> GhosttyPointTag -> IO (Maybe GhosttyPointCoordinate)
rawCoordinate ref tag =
  registered ref
    >>= maybe (pure Nothing) (optionalVia ResultNoValue (CallSite "ghostty_tracked_grid_ref_point") . (`ghosttyTrackedGridRefPoint` tag))

-- | @ghostty_tracked_grid_ref_set@: move the reference to a point of the
-- terminal's active screen. 'False' if the point is outside its coordinate
-- system (@GHOSTTY_INVALID_VALUE@, the reference and terminal being valid)
-- or past the scrollback ('pastHistory'), or the reference was untracked;
-- the reference is then unchanged.
moveTrackedRef :: TrackedRef s -> GhosttyPoint -> IO Bool
moveTrackedRef ref p =
  unlessPastHistory ref.terminal.handle p False $
    registered ref >>= maybe (pure False) (\h -> succeedsUnless ResultInvalidValue site =<< with p (ghosttyTrackedGridRefSet h ref.terminal.handle))
  where
    site = CallSite "ghostty_tracked_grid_ref_set"

-- | @ghostty_tracked_grid_ref_has_value@: whether the reference still names
-- a cell; 'False' once untracked.
refHasValue :: TrackedRef s -> IO Bool
refHasValue ref = maybe (pure False) (fmap toBool . ghosttyTrackedGridRefHasValue) =<< registered ref
