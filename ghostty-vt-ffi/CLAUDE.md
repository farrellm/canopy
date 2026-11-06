# ghostty-vt-ffi

Raw bindings to libghostty-vt (`DESIGN.md` §3): `capi` imports, `hsc2hs` layouts, and raw types
only, plus the one copy-out helper `ghosttyStringBytes` that every `GhosttyString` reader uses.
Only `Ghostty.Effectful.**` and `Test.Ghostty.**` may import it (§2.6 rule 1).

- **Most `.hsc` files and the layout test are generated: edit `gen/<path>.in`, not `<path>`.**
  `python3 scripts/gen-ghostty-ffi.py` renders every template (the module by hand, with
  `{{directive}}` lines for the mechanical parts; the script's docstring lists them), and CI's
  `--check` fails on a stale or hand-edited output. A generated file says so on its first line.
  `Layout.hs`, `Screen.hs`, `Allocator.hs` and `Terminal/Handle.hs` are hand-written.
- **Every function and type in the pinned headers is bound or listed in `coverage.txt`** with the
  PLAN.md step that binds it (`scripts/check-ghostty-coverage.py`). Binding a header's last symbol
  means deleting its line; the check fails on an entry that defers nothing.
- **Enums are exhaustive at the pin**: the layout test fails on a library value without a pattern.
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
- **One module per header**, partially bound when only part is needed yet; the module header says
  which part. `terminal.h`'s callbacks are in `Ghostty.FFI.Terminal.Callbacks`, and its handle in
  `Ghostty.FFI.Terminal.Handle` (so `GridRef` can name it without a cycle).
- Names (the generator's rules): a pattern is its C name in CamelCase (`GHOSTTY_TERMINAL_OPT_BELL`
  → `GhosttyTerminalOptBell`), and a record field is the C field in camelCase (`data` → `data_`).
  Where a pattern's name is already a type's, it takes a `Tag` suffix
  (`GhosttyTerminalResizeWindowGridTag`) and its doc says so.
- **A tagged union is a sum** (§3.3), with arms named after the union's members without the
  `Ghostty` prefix (`ScrollViewportDelta`) and an `…Other tag` arm. C `bool` is `CBool`.
- **A function that takes a struct by value calls a shim** in `include/gvtffi_shims.h` (`static
  inline`, `gvtffi_` prefix). The header also holds hsc2hs's `GVTFFI_FIELD_SIZE` and
  `GVTFFI_ARRAY_LEN`, so every `.hsc` includes it rather than `<ghostty/vt.h>`.
- Callback types are `FunPtr` synonyms, each with a `mk…` `wrapper`; the library's own function
  pointers handed to a callback (clipboard `reply`) have a `call…` `dynamic` import.
- Write hsc2hs directives inside expressions parenthesised, `(#const X)`: a bare one takes the rest
  of the line.
- `ghostty-vt-ffi-test` compiles `GHOSTTY_COMMIT` in (Template Haskell, `makeRelativeToProject`), so
  it runs from any directory and rebuilds when the pin moves.
