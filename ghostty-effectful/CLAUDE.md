# ghostty-effectful

The safe, `effectful` interface to libghostty-vt (`DESIGN.md` §4): scoped handles, typed keys, and
events instead of callbacks. No export may expose a pointer.

- **`Ghostty` is one statically dispatched effect** (§4.2), in `Ghostty.Effectful.Internal.Effect`.
  An operation is a plain function in the module of what it acts on, and its first line is
  `owned "<name>"`, the owner-thread check (§4.6); then `unsafeEff_` around the IO call. Handles are
  scoped by a rank-2 type variable and released with `bracket` (`unsafeSeqUnliftIO`).
- **`Internal` modules hold representations** (constructors over raw handles) and the IO calls; the
  public modules re-export their types without constructors. Raw pointers appear only in `Internal`
  (§10.6).
- **Results become `GhosttyError` through `Ghostty.Effectful.Internal.Error`**: a result the header
  says cannot happen for the arguments the types allow is `Invariant`. Make the bad argument
  unrepresentable (as `Extent` does for zero) rather than returning the refusal.
- **A header row is a GADT constructor named for it** (`Ghostty.Effectful.Terminal.Keys`, §4.4):
  `CURSOR_X` is `CursorX`, and an option's name gets `Set`; a test checks it. Marshalled in
  `Internal.Keys`. Adding one means a case in `dataKey`/`optionKey`, `getData`/`setOption`, and
  the test's `allData`/`allOptions` and its total `fresh`/`written` match. A row deliberately left
  out goes in the test's `deferredData`/`deferredOptions` with its PLAN.md step.
- **Callbacks only append** (§4.5, `Internal.Callbacks`): each body is `sheltered`, so an
  exception is recorded and thrown by the drain, never let into C. Anything needing the terminal
  (title, cursor) is a `Pending` entry resolved between writes. A query's answer is configuration
  (`TerminalConfig`, and a `Set…` option named for the callback's row); ENQ and XTVERSION answer in
  C (`cbits/`), since they return a struct by value. Any operation whose C call can run a callback
  returns the events it caused, as `feed`, `resize` and `reset` do.
- **A C enum is closed at this level, with no unknown case**: an unnamed value is `UnknownEnum`
  (`Internal.Convert.unknownEnum`), because the version check and the layout test rule it out.
- Tests may import `Internal` (to build values or reach `runGhosttyPinnedTo`).

Its doctest runs the real library, so `LD_LIBRARY_PATH` must come from `build-libghostty.sh`.
