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
- Tests may import `Internal` (to build values or reach `runGhosttyPinnedTo`).

Its doctest runs the real library, so `LD_LIBRARY_PATH` must come from `build-libghostty.sh`.
