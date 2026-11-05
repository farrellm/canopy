# Changelog

Written as changes land, not at release (DESIGN.md §2.9).

## Unreleased

### M0: infrastructure

- Seven packages under one `cabal.project`, each a skeleton wired to its neighbours in §1.3:
  `canopy-prelude`, `ghostty-vt-ffi`, `ghostty-effectful`, `tendril`, `tree-sitter-ffi`,
  `tree-sitter-effectful` and `canopy`.
- libghostty-vt pinned at `c7f68a7f`. `scripts/build-libghostty.sh` builds it, and fetches a pinned
  Zig 0.17.1 when none is installed. The library is found through pkg-config at configure time.
  `ghostty-vt-ffi-test` checks that the linked build is the pinned one.
- The tree-sitter runtime is vendored at v0.27.1 (`scripts/vendor-tree-sitter.sh`).
  `scripts/gen-ts-shims.py` generates its 56 by-value shims, and CI checks them with `--check`.
- The prelude, the common stanzas and their check, hlint layering (§2.6 rules 1–7) with fixtures,
  ormolu, the Haddock floor, and CI (§2.8 steps 1–7).
- DESIGN.md §5A.2 and §14.2: the by-value function count is 56, not 46.
- DESIGN.md §3.1, §3.4 and §14.2: libghostty-vt's build info reports the library's `0.1.0-dev`
  with empty build metadata, not the app version. The build script now stamps the pinned commit in
  (`-Dlib-version-string=0.1.0-dev+<commit>`), and the identity test requires an exact match.
