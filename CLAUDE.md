# canopy

A language-agnostic REPL frontend for the terminal: a terminal emulator (libghostty-vt) with one pane,
a line editor, tree-sitter and LSP. **M0 (infrastructure) is built:** seven wired skeletons, with
nothing user-facing yet. [`DESIGN.md`](./DESIGN.md) is the architecture and is ahead of the code. Where the
two disagree, the design is the intent and the code is behind. Conventions are cassini's
(`../cassini`) on purpose (§2, D10).

| Package | What it is |
| :--- | :--- |
| [`canopy-prelude/`](./canopy-prelude/CLAUDE.md) | `Canopy.Prelude`: relude minus the effectful collisions (§2.3). |
| [`ghostty-vt-ffi/`](./ghostty-vt-ffi/CLAUDE.md) | `Ghostty.FFI.*`: raw capi bindings to libghostty-vt, and the pin (§3). |
| [`ghostty-effectful/`](./ghostty-effectful/CLAUDE.md) | `Ghostty.Effectful.*`: the safe effectful interface (§4). |
| [`tendril/`](./tendril/CLAUDE.md) | `Tendril.*`: the line editor, and `tendril-demo` (§5). |
| [`tree-sitter-ffi/`](./tree-sitter-ffi/CLAUDE.md) | `TreeSitter.FFI.*`: the vendored runtime and the generated shims (§5A). |
| [`tree-sitter-effectful/`](./tree-sitter-effectful/CLAUDE.md) | `TreeSitter.*`: persistent trees, pure reads, typed kinds (§5B). |
| [`canopy/`](./canopy/CLAUDE.md) | `Canopy.*`: the application, its executable and suites (§6–§9). |

| Path | What it is |
| :--- | :--- |
| `cabal.project` | Lists the seven packages. |
| `.ormolu` | ormolu's fixities and re-exports (§2.5). Copied from cassini. Not style config. |
| `.hlint.yaml`, `lint/` | §2.6's layering rules, their fixtures, and `lint/check-layering.sh`. |
| `scripts/` | `build-libghostty.sh` (with `prefetch-zig-deps.py`), `vendor-tree-sitter.sh`, `gen-ts-shims.py`, `check-common-stanzas.sh`, `check-haddock.py`. |
| `_deps/` | Gitignored output of `build-libghostty.sh`: the Zig, the ghostty checkout, and the built library. |
| `.github/workflows/ci.yml` | CI, §2.8 steps 1–7. Step 8 arrives with the first benchmark. |

## Rules

1. **A decision that changes changes `DESIGN.md` in the same commit** (§2.9). D-numbers and section
   numbers are cited from elsewhere, so keep them stable. A deferred decision whose trigger fires
   gets an answer, not a deletion.
2. **House style is §2.3–§2.7, so it is not re-litigated.** In short:
   - relude through the `canopy-prelude` mixins
   - `effectful`, never mtl
   - `optics`, never `lens`: record dot to read, labels to update
   - explicit export lists
   - no partial functions
   - Haddock on every export, with a 100% floor
3. **The common stanzas are copied into every `.cabal`.** Change all seven together, and
   `scripts/check-common-stanzas.sh` checks that they agree. The project-wide extensions are exactly
   `OverloadedRecordDot`, `OverloadedStrings` and `OverloadedLabels`. Anything else, including
   `BlockArguments`, is declared per module or not used, and M0 writes `f $ do`.
4. **Layering is a lint rule, by module name, across packages** (§2.6). `Main` is trusted with
   nothing, because `tendril-demo`'s `Main` and `canopy`'s have the same name. So a test suite's
   tests live in `Test.*` modules, not in its `Main`. A rule change comes with fixtures in
   `lint/fixtures/{reported,allowed}`.
5. **Ormolu reads `default-extensions` from each package's `.cabal`. Never pass `--no-cabal`.**
6. **Pins move only on purpose.** `ghostty-vt-ffi/GHOSTTY_COMMIT` and
   `tree-sitter-ffi/TREE_SITTER_VERSION` each change in one commit that says why. A tree-sitter bump
   also regenerates the shims.
7. **Goldens are read before they are accepted.** Every fixed bug adds a numbered regression case
   and a `CHANGELOG.md` line in the same commit (§11.1).

## Toolchain and commands

GHC 9.12.4, cabal 3.16.1.0, ormolu 0.8.0.2, hlint 3.10 and doctest 0.25.0, as in cassini. Zig
≥0.17 is needed only to build libghostty-vt, and the script fetches it. Everything runs from the
root:

- Library: `eval "$(scripts/build-libghostty.sh)"`, once per shell. It sets `PKG_CONFIG_PATH` and
  `LD_LIBRARY_PATH`. Without it, any package that depends on `ghostty-vt-ffi` fails at configure
  time. That is by design (§3.1).
- Build: `cabal build all --enable-tests --enable-benchmarks --ghc-options=-Werror`
- Test: `cabal test all`, and `python3 scripts/gen-ts-shims.py --check`
- Doctest: `cabal repl --with-repl=doctest --repl-options=-Wno-missing-export-lists lib:<pkg>`, for
  `tendril`, `ghostty-effectful` and `tree-sitter-effectful`
- Lint: `hlint --ignore-glob='lint/fixtures/**' .`, `lint/check-layering.sh` and
  `scripts/check-common-stanzas.sh`
- Format: `ormolu --mode check $(git ls-files '*.hs')`
- Haddock: `cabal haddock all 2>&1 | python3 scripts/check-haddock.py`
