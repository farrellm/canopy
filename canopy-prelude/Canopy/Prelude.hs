-- | The project prelude: @relude@, minus the names that collide with
-- @effectful@ (DESIGN.md §2.3).
--
-- Every stanza that consumes it renames it to @Prelude@ through cabal
-- @mixins@, so no module imports it by name.
module Canopy.Prelude (module Relude) where

-- The subtraction, by reason. Ormolu sorts the import list, so the grouping
-- lives here:
--
-- - Collides name-for-name with Effectful.State.Static.Local: State, get, put,
--   modify, gets, state, evalState, execState, runState.
-- - Collides name-for-name with Effectful.Reader.Static: Reader, ask, asks,
--   local, runReader, withReader (effectful's withReader is a reinterpreter,
--   not mtl's).
-- - No effectful counterpart, but the house style has no transformer stacks
--   (§2.7), so a module that wants one says so in its own import list:
--   StateT, MonadState, modify', evalStateT, execStateT, runStateT, ReaderT,
--   MonadReader, runReaderT.
--
-- Nothing is subtracted for optics: relude's (&) is Data.Function's, the same
-- entity optics re-exports, so an unqualified `import Optics` is unambiguous
-- (checked, relude 1.2.2.2, optics 0.4.2.1). A real collision added by a later
-- release is subtracted here, not hidden per module.
import Relude hiding
  ( MonadReader,
    MonadState,
    Reader,
    ReaderT,
    State,
    StateT,
    ask,
    asks,
    evalState,
    evalStateT,
    execState,
    execStateT,
    get,
    gets,
    local,
    modify,
    modify',
    put,
    runReader,
    runReaderT,
    runState,
    runStateT,
    state,
    withReader,
  )
