# ghostty-effectful

The safe, `effectful` interface to libghostty-vt (`DESIGN.md` §4): scoped handles, typed keys, and
events instead of callbacks. No export may expose a pointer.

- **`Ghostty` is one statically dispatched effect** (§4.2), in `Ghostty.Effectful.Internal.Effect`.
  An operation is a plain function in the module of what it acts on, built on `owned "<name>"`, the
  owner-thread check (§4.6): `ownedIO "<name>" io` for one IO call, `countOf` for a counter, and
  `scoped` for a handle scoped by a rank-2 type variable, counted, and released with `bracket`.
  That variable's role is declared nominal (`type role`, §4.2), or `coerce` lets the handle out of
  its region; `Test.Ghostty.Effectful.Region` checks each (regression 14).
- **`Internal` modules hold representations** (constructors over raw handles) and the IO calls; the
  public modules re-export their types without constructors. Raw pointers appear only in `Internal`
  (§10.6).
- **Results become `GhosttyError` through `Ghostty.Effectful.Internal.Error`** (`expectSuccess`,
  `succeedsUnless`, `outVia`, `optionalVia`, `bytesVia`; never a hand-written
  switch). A run of reads in a loop uses their `…At` forms (`outAt`, `optionalAtWith`, `bytesAt`, `bytesAtWith`)
  on memory made once for the run, as `Internal.Render.CellSlots` is, since each `alloca` is a pinned
  allocation (§12.3). Out of
  memory is `OutOfMemory`, and a result the header says cannot happen for the arguments the types
  allow is `Invariant`. Marshalling shared between modules (strings, flags, palettes, enum call
  sites) is in `Internal.Convert`. Make the bad argument
  unrepresentable (as `Extent` does for zero) rather than returning the refusal.
- **A header row is a GADT constructor named for it** (`Ghostty.Effectful.Terminal.Keys`, §4.4):
  `CURSOR_X` is `CursorX`, and an option's name gets `Set`; a test checks it. Marshalled in
  `Internal.Keys`. Adding one means a case in `dataKey`/`optionKey`, `getData`/`setOption`, and
  the test's `allData`/`allOptions` and its total `fresh`/`written` match. A row deliberately left
  out goes in the test's `deferredData`/`deferredOptions` with its PLAN.md step.
- **Callbacks only append** (§4.5, `Internal.Callbacks`): each body is `sheltered`, so an
  exception is recorded and thrown by the drain, never let into C. Anything needing the terminal
  (a title, a working directory, a mark's cursor) is read inside the callback, as the header asks. A query's answer is configuration
  (`TerminalConfig`, and a `Set…` option named for the callback's row); ENQ and XTVERSION answer in
  C (`cbits/gveff_terminal.c`, with the terminal's `USERDATA` struct), since they return a struct by
  value. `SEMANTIC_PROMPT` is C too (D18): it copies each mark into the `USERDATA`, with the cursor
  read inside the callback and the count of Haskell entries before it (D20: `push` counts, and the take counts by it, so every
  append goes through `push`, a test's through `pushPending`); `takeEvents` merges the marks into the Haskell buffer by those
  counts, newest first, building the returned list in order in the same pass (D21). A title or working directory is read inside its callback too, so `feed` is one
  `vt_write` and one take, with no stops. Any operation whose C call can run a callback returns the events it caused, as `feed`,
  `resize` and `reset` do (`Internal.Feed`); `setT`'s cannot,
  and a test holds that. Every struct a callback receives is frozen (no `size`), so it is read whole.
- **Render states and frames** (§4.3, `Internal.Render`, public `Render`): a render state is its
  own rank-2 region inside its terminal's; a frame (`withFrame`, tag `f`) is shorter still, and what
  leaves it is copied (`Cell`). A frame that returns cleans the dirty state; one that throws does
  not. Frame and row reads are typed keys like the terminal's (`FrameData`/`RowData`, prefixed
  `Render`/`Row`, held against the header by `Test.Ghostty.Effectful.Render`; deferred rows listed
  there). `RENDER_HOLD` is C (`cbits/gveff_terminal.c`): it captures into the render states attached
  to the terminal, then calls the Haskell callback, which only appends. A call that changes the
  terminal or its viewport must call `wrote`, since a frame reads hyperlinks from the terminal only
  while that count is unchanged.
- **Formatting** (`Internal.Format`, public `Format`) has no handle: each call makes a formatter,
  sets every option on it (`EMIT`, `UNWRAP`, `TRIM`, `EXTRAS`, `SELECTION`: a new formatter's
  defaults are the library's, not canopy's, regression 10), resolves its range to untracked grid
  references, formats and frees, so the references cannot go stale. Goldens are in
  `test/golden/format/`, control bytes written visibly (`\e`, `\r`); read each before accepting it
  (`--accept`). A call taking a point checks a `History` row with
  `Internal.Terminal.pastHistory` first, since the library does not refuse one past the scrollback.
- **Encoders, parsers, widths** (§4.8, M1.9). Key and mouse encoders are handles (`withKeyEncoder`,
  `withMouseEncoder`, `Internal.Encode`), each owning the event handle it fills in per encode; their
  inputs are plain values in `Input`. Reports, `encodePaste` and the parsers have no handle: a
  parser is made, fed one sequence, copied out of and freed in one call. Every buffer read goes
  through `Internal.Error.bytesVia`, which retries on `OUT_OF_SPACE`, so a short buffer never
  escapes.
  `paste` reads its text through the C MIME reader `cbits/gveff_paste.c` (a struct by value) and
  returns its `WritePty` as an event, `Either Rejected`. `Key` and `OscCommand` are held against
  their header enums by the tests, as typed keys are.
- **A C enum is closed at this level, with no unknown case**: an unnamed value is `UnknownEnum`
  (`Internal.Convert.unknownEnum`), because the version check and the layout test rule it out.
- Tests may import `Internal` (to build values or reach `runGhosttyPinnedTo`).
- **Benchmarks** (§12.3, M1.10) are `ghostty-effectful-bench`, in `bench/Bench/**` (`Feed`,
  `Render`), through the public API only. A per-read or per-cell function keeps its helpers at top
  level (`Internal.Keys.KeyRead`, `Internal.Render`'s cell readers), not in a `where`: GHC allocates
  each local helper's specialisations as closures on every call. Measure a hot-path change against
  `master`, alternating runs, before keeping it. CI gates their allocation against `bench/baseline/ghc-9.12.4.csv`. Each iteration is a
  whole terminal life (`runGhostty`, open, feed, free), so no iteration feeds a terminal another
  left scrollback in.

Its doctest runs the real library, so `LD_LIBRARY_PATH` must come from `build-libghostty.sh`.
