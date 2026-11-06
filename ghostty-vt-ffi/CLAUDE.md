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
- **Every struct and enum the binding marshals exports a descriptor** (`Ghostty.FFI.Layout`) beside
  it, every number from `hsc2hs`, named `tLayout` or `tEnum` after its type `T`. Each has one row in
  `test/Test/Ghostty/FFI/Layout.hs`: `StructRow` with a sample value whose fields are all non-zero
  and not all ones, or `EnumRow` with a total match on the `COMPLETE` set. That is the layout test
  (§3.4). A struct descriptor lists every field, because the test requires the library's field names
  exactly. An enum's `Unknown` view is built from its descriptor. `scripts/check-layout-rows.py`
  fails if a type has no descriptor or no row.
- Write hsc2hs directives inside expressions parenthesised, `(#const X)`: a bare one takes the rest
  of the line.
- `ghostty-vt-ffi-test` compiles `GHOSTTY_COMMIT` in (Template Haskell, `makeRelativeToProject`), so
  it runs from any directory and rebuilds when the pin moves.
