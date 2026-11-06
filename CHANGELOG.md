# Changelog

Written as changes land, not at release (DESIGN.md §2.9).

## Unreleased

### M1: ghostty

- `ghostty-vt-ffi-test` is the full layout test (DESIGN.md §3.4): it parses the linked library's
  `ghostty_type_json()` and checks every bound struct's size, alignment, field names, offsets and
  sizes, and every named enum value, against the binding's `hsc2hs` values. The binding exports those
  values as descriptors (`Ghostty.FFI.Layout`), and binds `ghostty_type_json`. Each struct's `Storable`
  instance is checked against its descriptor (size, alignment, and a poke/peek round trip over
  zeroed and all-ones memory). Each enum's `Unknown` view is built from its descriptor and checked,
  through a total match, to exclude exactly the named values. `scripts/check-layout-rows.py`, in CI,
  fails if a bound type has no descriptor or no row.
- `GhosttyBuildInfo` has the `COMPLETE` set and `GhosttyBuildInfoUnknown` view §3.3 requires, which
  M0 left out.

### M0: infrastructure

- Seven packages under one `cabal.project`, each a skeleton wired to its neighbours in §1.3:
  `canopy-prelude`, `ghostty-vt-ffi`, `ghostty-effectful`, `tendril`, `tree-sitter-ffi`,
  `tree-sitter-effectful` and `canopy`.
- libghostty-vt pinned at `c7f68a7f`. `scripts/build-libghostty.sh` builds it, and fetches a pinned
  Zig 0.17.1 when none is installed. The Zig dependencies `zig build` cannot get are fetched
  with curl (`scripts/prefetch-zig-deps.py`), because Zig 0.17.1's fetcher fails on some machines;
  only those `-Demit-lib-vt` needs, not ghostty's lazy app dependencies. The library is found through
  pkg-config at configure time.
  `ghostty-vt-ffi-test` checks that the linked build is the pinned one.
- The tree-sitter runtime is vendored at v0.27.1 (`scripts/vendor-tree-sitter.sh`).
  `scripts/gen-ts-shims.py` generates its 56 by-value shims, and CI checks them with `--check`.
- The prelude, the common stanzas and their check, hlint layering (§2.6 rules 1–7) with fixtures,
  ormolu, the Haddock floor, and CI (§2.8 steps 1–7).
- DESIGN.md §5A.2 and §14.2: the by-value function count is 56, not 46.
- DESIGN.md §3.1, §3.4 and §14.2: libghostty-vt's build info reports the library's `0.1.0-dev`
  with empty build metadata, not the app version. The build script now stamps the pinned commit in
  (`-Dlib-version-string=0.1.0-dev+<commit>`), and the identity test requires an exact match.
- Fixed, from review of the M0 PR:
  - An interrupted Zig download no longer leaves a `zig` without its `lib/` that the next run trusts:
    it unpacks beside its directory and is moved into place whole.
  - Interrupted prefetches no longer leave scratch directories inside Zig's package store.
  - `gen-ts-shims.py` finds the by-value structs in `api.h` instead of keeping a hand list, so a
    tree-sitter bump cannot add one that silently gets no shim.
  - `tree-sitter-ffi`'s sdist ships the vendored ICU and wasm libc licence files, and installs
    `tree_sitter/api.h` beside `tsffi_shims.h`, which includes it. Its C flags no longer fix `-O2`.
  - `ghostty-vt-ffi-test` compiles `GHOSTTY_COMMIT` in, so it passes when run from any directory.
  - §2.6 rule 4 no longer trusts `Main` (fixtures `ChildFromMain`, `ConcurrentFromMain`).
  - `GhosttyString`s are copied out by one function, `ghosttyStringBytes` (§3.3), in place of two
    copies; a null pointer is the empty string.
