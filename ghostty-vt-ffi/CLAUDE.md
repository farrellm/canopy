# ghostty-vt-ffi

Raw bindings to libghostty-vt (`DESIGN.md` §3): `capi` imports, `hsc2hs` layouts, and raw types
only, plus the one copy-out helper `ghosttyStringBytes` that every `GhosttyString` reader uses.
Only `Ghostty.Effectful.**` and `Test.Ghostty.**` may import it (§2.6 rule 1).

- **Most `.hsc` files and the layout test are generated: edit `gen/<path>.in`, not `<path>`.**
  `python3 scripts/gen-ghostty-ffi.py` renders every template (the module by hand, with
  `{{directive}}` lines for the mechanical parts; the script's docstring lists them), and CI's
  `--check` fails on a stale or hand-edited output. A generated file says so on its first line.
  `Layout.hs`, `Allocator.hs`, `Terminal/Handle.hs`, `KittyGraphics.hs` and `Unicode.hs` are
  hand-written (they have nothing to generate). So is everything in a
  template outside its directives: imports, handles, tagged unions and structs with arrays.
- **Every function and type in the pinned headers is bound or listed in `coverage.txt`** with the
  PLAN.md step that binds it (`scripts/check-ghostty-coverage.py`). Binding a header's last symbol
  means deleting its line; the check fails on an entry that defers nothing.
- **Enums are exhaustive at the pin**: the layout test fails on a library value without a pattern.
- `LIBGHOSTTY_VT_VERSION` is the pin, a release (`1.0.0`). `scripts/build-libghostty.sh` builds it
  offline from upstream's release tarball and deps tarball, each checked against a pinned sha256. The
  ABI is stable across 1.x: a minor bump only adds. The library is found
  through `pkgconfig-depends` at configure time, and the `static` flag switches to
  `libghostty-vt-static`.
- **C enums are newtypes with pattern synonyms, `COMPLETE` with an `…Unknown` view** (§3.3). Never
  use a closed sum. Every value comes from `hsc2hs` (`#const`), never a hand-copied number. Every
  key enum starts at `INVALID = 0` (`…Invalid`), which no binding maps; a test that holds keys
  against a header leaves it out.
- **Sized or frozen**: a struct with a leading `size` is initialised as `GHOSTTY_INIT_SIZED` does
  (`withSized`); a frozen one (`GhosttyGridRef`, `GhosttySelection`, `GhosttyStyle`, every callback
  payload) has no `size` and is zeroed (`withZeroed`). A setter given null restores the default.
- **Each import's comment says why it is `safe` or `unsafe`.** An `unsafe` call that re-enters
  Haskell crashes, and the compiler cannot catch that. `safety.txt` ties each decision to a
  fingerprint of the function's header text; after binding a function, or when
  `scripts/check-ghostty-safety.py` says its header changed, read the header's docs for it, then
  run the script with `--update` and commit the file. Never edit it by hand.
- **`capi` does not check integer widths or pointers to Haskell types**, so
  `scripts/check-ghostty-signatures.py` compares every import, callback type and struct record with
  the header's C types by name. Write the Haskell type the header names: `Word16` for `uint16_t`,
  `Ptr GhosttySelection` for `GhosttySelection *`, the enum or handle of the C name.
- **Every struct and enum the binding marshals exports a descriptor** (`Ghostty.FFI.Layout`) beside
  it, every number from `hsc2hs`, named `tLayout` or `tEnum` after its type `T`. Each has one row in
  `test/Test/Ghostty/FFI/Layout.hs`: `StructRow` with a sample value whose fields are all non-zero
  and not all ones, or `EnumRow` with a total match on the `COMPLETE` set. That is the layout test
  (§3.4). A struct descriptor lists every field, because the test requires the library's field names
  exactly. An enum's `Unknown` view is built from its descriptor. `scripts/check-layout-rows.py`
  fails if a type has no descriptor or no row.
- **One module per header**, partially bound when only part is needed yet; the module header says
  which part. `terminal.h`'s callbacks are in `Ghostty.FFI.Terminal.Callbacks`.
- Behavioural tests share `Test.Ghostty.FFI.Support` (a terminal per test, sized structs, the
  `…_buf` size protocol). Each shim has a test that it does what the call it wraps does. An
  encoder's test expects the sequence its spec gives, worked out before running it. A callback that
  receives a struct by value (`GhosttyMimeReaderFn`) can only be C: the test's is in
  `test/cbits/gvtffi_test.c`.
- Names (the generator's rules): a pattern is its C name in CamelCase (`GHOSTTY_TERMINAL_OPT_BELL`
  → `GhosttyTerminalOptBell`), and a record field is the C field in camelCase (`data` → `data_`).
  Where a pattern's name is already a type's, it takes a `Tag` suffix
  (`GhosttyTerminalResizeWindowGridTag`), and where it is its enum's `Unknown` view, a `Value`
  suffix (`GhosttyMouseButtonUnknownValue`); its doc says so. Constants a header `#define`s over a
  scalar typedef (`GhosttyMods`, `GhosttyKittyKeyFlags`) are patterns of that synonym, from
  `{{defines}}`; they are not enums, so they have no `COMPLETE` set or descriptor.
- **A tagged union is a sum** (§3.3), with arms named after the union's members without the
  `Ghostty` prefix (`ScrollViewportDelta`) and an `…Other tag` arm. C `bool` is `CBool`.
- **A function that takes or returns a struct by value calls a shim** in
  `include/gvtffi_shims.h` (`static inline`, `gvtffi_` prefix), generated from a `{{shim F}}` line
  in `gen/include/gvtffi_shims.h.in`: add the line when binding the function. The header also holds
  hsc2hs's `GVTFFI_FIELD_SIZE` and `GVTFFI_ARRAY_LEN`, so every `.hsc` includes it rather than
  `<ghostty/vt.h>`.
- **Handles are bound where their header is used**, not in `Types` though `types.h` declares them:
  a newtype over `Ptr` to an empty `…Impl` type, with a derived `Storable` (`GhosttyRenderState` in
  `Render`). `GhosttyTerminal` is the exception, in `Terminal.Handle`, so `GridRef` and `Selection`
  can name it without a cycle. `GhosttyKittyGraphics` is in `KittyGraphics`, the header that uses
  it, though only the handle is bound there yet.
- **`safe` for long calls too.** A call that runs no callback is still `safe` when it can walk the
  scrollback (`terminal_compress`, the formatter's format calls): an `unsafe` one
  holds up every Haskell thread at the next GC.
- Callback types are `FunPtr` synonyms, each with a `mk…` `wrapper`; the library's own function
  pointers handed to a callback (clipboard `reply`) have a `call…` `dynamic` import.
- Write hsc2hs directives inside expressions parenthesised, `(#const X)`: a bare one takes the rest
  of the line.
- `ghostty-vt-ffi-test` compiles `LIBGHOSTTY_VT_VERSION` in (Template Haskell,
  `makeRelativeToProject`), so it runs from any directory and rebuilds when the pin moves. It checks
  the linked `ghostty_version()`, the headers' `GHOSTTY_VT_VERSION` and every `FEATURE_*` against it.
