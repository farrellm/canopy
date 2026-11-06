# ghostty-vt-ffi

Raw bindings to libghostty-vt (`DESIGN.md` §3): `capi` imports, `hsc2hs` layouts, and raw types
only, plus the one copy-out helper `ghosttyStringBytes` that every `GhosttyString` reader uses.
Only `Ghostty.Effectful.**` and `Test.Ghostty.**` may import it (§2.6 rule 1).

- `GHOSTTY_COMMIT` is the pin. `scripts/build-libghostty.sh` builds that commit. The library is found
  through `pkgconfig-depends` at configure time, and the `static` flag switches to
  `libghostty-vt-static`.
- **C enums are newtypes with pattern synonyms, `COMPLETE` with an `…Unknown` view** (§3.3). Never
  use a closed sum. Every value comes from `hsc2hs` (`#const`), never a hand-copied number.
- **Each import's comment says why it is `safe` or `unsafe`.** An `unsafe` call that re-enters
  Haskell crashes, and the compiler cannot catch that.
- `ghostty-vt-ffi-test` compiles `GHOSTTY_COMMIT` in (Template Haskell, `makeRelativeToProject`), so
  it runs from any directory and rebuilds when the pin moves. In M0 it checks only the build
  identity. The full layout test (§3.4) arrives in M1.
