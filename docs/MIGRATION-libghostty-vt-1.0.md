# Migrating to libghostty-vt 1.0.0

A plan to move canopy's ghostty packages from the M0 pin to the first stable release of
libghostty-vt. Written 2026-11-07, from a review of every change between the two. Section
references (§) are to `DESIGN.md`. When this lands, its decisions move into `DESIGN.md` and its
step into `PLAN.md`, and this file is deleted.

## 1. Summary

| | Now | Target |
| :--- | :--- | :--- |
| Pin | commit `c7f68a7f48c6` (2026-11-03), `GHOSTTY_COMMIT` | release **1.0.0**, tag `vt-1.0.0` = `0fa3d577a561` (2026-11-06) |
| Reported version | `0.1.0-dev+c7f68a7f…` (stamped by our build script) | `1.0.0` |
| Source | shallow `git fetch` of the commit, plus `prefetch-zig-deps.py` | signed release tarball, plus its deps tarball |
| ABI | "not yet stable and definitely going to change" (§14.1) | stable for 1.x: additions only, a SONAME of `libghostty-vt.so.1`, versioned ELF symbols |
| Zig | 0.17.1 | 0.17.x (unchanged; 1.0 requires major.minor 0.17 exactly) |

The pin is an ancestor of `vt-1.0.0`, 118 commits behind. Almost all of them are upstream's
pre-1.0 **ABI freeze pass**, and it breaks the C API in many small ways on purpose:

- Every key enum gains `INVALID = 0`, which renumbers 13 enums.
- Setters are renamed (`setopt` → `set`), and setters and getters return `GhosttyResult`.
- Every output buffer is `uint8_t *` with `buf, buf_len, out_written`.
- Callback payloads and `GridRef`, `Selection` and `Style` lose their `size` field.
- `GhosttyStyle` becomes a frozen 16-byte struct with a bit-flag word.
- Formatter options move from a by-value struct to keyed `ghostty_formatter_set`.
- The selection-format functions are removed.
- Duplicate enums are merged (`GhosttyCursorStyle`, `GhosttyImageLayer`), and the mode macros
  are renamed.

Nothing in canopy's *design* changes. It changes in four places:

- **The binding's machinery.** Fewer shims, fewer sized structs, and a version-based pin.
- **About 15 files of hand-written code** across `ghostty-vt-ffi` and `ghostty-effectful`.
- **Three silent behaviour changes** that no type or layout check catches (§4). These are the
  real risk.
- **The build gets simpler.** The release deps tarball removes the Zig-fetcher workaround
  entirely.

**Recommendation:** do it now, as one CR (§7, step M1.11). M1 is complete and M2's oracle steps
are about to consume `Ghostty.Effectful.Encode`, `Format` and `Mode`. Each name and default that
changes now would otherwise churn those too. After 1.0, upstream can only add, so this is the last
migration of this size the binding should ever need.

## 2. What 1.0 promises (and what canopy gains)

Sources: <https://lib.ghostty.org/vt/1.0.0/> (`index`, `stability`, `installing`, `conventions`),
and `PACKAGING-LIBGHOSTTY.md` at the tag.

- **Stable C ABI across 1.x.** A minor release may only add functions, types, enum values, or
  fields at the end of a *sized* struct. Nothing is removed, resized, reordered or re-typed.
  Upstream CI enforces this with abidiff and goldens (`src/terminal/c/abi/`).
- **Behaviour changes are new functions.** The old function is marked `GHOSTTY_DEPRECATED` until
  the next major version.
- **Unknown keys return `GHOSTTY_UNSUPPORTED`** (new, `-8`) instead of being undefined behaviour.
  So a binding built against newer headers can detect an older library.
- **Version checks are first-class.**
  - `ghostty/vt/version.h`: `GHOSTTY_VT_VERSION_{MAJOR,MINOR,PATCH}`, `GHOSTTY_VT_VERSION`
    (`"1.0.0"`) and `GHOSTTY_VT_VERSION_AT_LEAST()`.
  - `ghostty_version()`: a NUL-terminated runtime string.
  - The compatibility rule: the library's major version equals the headers', and its minor version
    is at least the headers'.
- **Snapshots are excluded.** The snapshot *byte format* is not covered by the ABI promise. That
  matters for M6.10 and session resume (§8.7): a stored snapshot can stop decoding after any
  library update.
- **Optional features.** `-Dvt-features` can compile parts out. A function of a compiled-out
  feature still exists but returns `GHOSTTY_UNSUPPORTED`. `ghostty_build_info` gains
  `FEATURE_{SNAPSHOT,FORMATTER,SELECTION,SEARCH,RENDER_STATE,INPUT_ENCODE,COLOR,GRID_INTROSPECTION,GLYPH_PROTOCOL}`.
- **Packaging.**
  - Release tarballs live at `release.files.ghostty.org/libghostty-vt/<V>/`, with minisign
    signatures and a `SHA256SUMS`.
  - A separate **deps tarball** contains every Zig package the build needs, so
    `zig build --system <deps>` runs offline.
  - The tarball carries `LIB_VERSION`, so the build needs no `-Demit-lib-vt`, and its reported
    version is exactly `1.0.0`.
  - pkg-config names are unchanged (`libghostty-vt`, `libghostty-vt-static`). A CMake package
    config is added.
- **Threads** (newly documented): the library creates none, and callbacks run on the calling
  thread. Calls on one handle and its borrowed handles must not overlap. This matches §4.6.
- **NULL handling** (newly specified): a NULL handle or out pointer returns `INVALID_VALUE`, or
  makes the call do nothing. It no longer crashes. canopy never passes NULL where that matters, so
  this is defence in depth.

Verified locally:

- The release tarball's `sha256` matches `SHA256SUMS`.
- Its `include/` is **byte-identical** to the `vt-1.0.0` tag's.
- Its `LIB_VERSION` is `1.0.0`, and it declares `minimum_zig_version = "0.17.0"`.

`SHA256SUMS` itself has not yet been checked against the minisign key. Do that before pinning
(§7, step 1).

```
4a6ef758be721923e1969e7b81b443dcc92d8dd3fb5a39600c03a8817a753676  libghostty-vt-1.0.0.tar.gz
4d5ea5e448e4c7ea8fb55ccbaa8668f2a97d44f438cee831e2b7ecd162d85c29  libghostty-vt-1.0.0-deps.tar.gz
minisign key: RWQlAjJC23149WL2sEpT/l0QKy7hMIFhYdQOFy0Z7z7PbneUgvlsnYcV
```

## 3. Every upstream change, and where it lands in canopy

This was found by diffing the headers with comments stripped (1,483 lines of declaration changes),
then the doc comments of every keyed enum value, then the commit messages. Finally, our own
generator and checks were run against the 1.0 headers (Appendix A).

### 3.1 ABI-wide conventions

| Change | canopy impact |
| :--- | :--- |
| **`INVALID = 0` in every get/set key enum.** 13 enums renumber: terminal options, key and mouse encoder options, render-state, row and kitty-iterator options, OSC, search, selection gesture, snapshot decoder and sys. | Values come from `hsc2hs`, so nothing is hand-numbered. Each enum gains an `…Invalid` pattern from the generator, and each layout-test `EnumRow`'s total match needs the new case (a compile error points at each). The effectful GADTs never map to `INVALID`. |
| **`GHOSTTY_UNSUPPORTED = -8`** for an unknown key, or a compiled-out feature. | `GhosttyResult` gets a pattern (generated). `ResultCode` gets `ResultUnsupported` (`Ghostty.Effectful.Error`). With the exact-version check (D-1, §5), it stays an `Invariant`. |
| **Setters return `GhosttyResult`**: `key_event_set_*`, `mouse_event_set_*` and `clear_button`, `mouse_encoder_reset`, `terminal_scroll_viewport`, and the encoders' `set` and `set_from_terminal`. | The imports change from `IO ()` to `IO GhosttyResult`. Every effectful call site gains `expectSuccess`. |
| **Getter result types change**: `mouse_event_get_button` and `osc_command_data` return `GhosttyResult` instead of `bool`. | The imports change. `Internal.Parse.datum` (`Parse.hs:274`) tests `toBool`. That would read `SUCCESS` (0) as "absent". The signature check flags the import, so a missed caller fails to compile. |
| **Renamed setters**: `ghostty_{key,mouse}_encoder_setopt[_from_terminal]` → `…_set[_from_terminal]`. | Rename the imports, `safety.txt` rows, and the callers in `Internal.Encode`. |
| **One buffer shape**: `uint8_t *buf, size_t buf_len, size_t *out_written` everywhere. Was `char *` or `out_buf_size`/`out_len` in `focus_encode`, `color_scheme_report_encode`, `mode_report_encode`, `size_report_encode`, `paste_encode` (its output), `key_encoder_encode`, `mouse_encoder_encode`, `grid_ref_graphemes` and `grid_ref_hyperlink_uri`. An empty result is `SUCCESS` with 0, even for a size query. | `Ptr CChar` → `Ptr Word8` in 7 imports. `Internal.Encode.bytesVia` (`Encode.hs:120`) changes its element type. Its protocol (64 bytes, then retry at the asked size) is unaffected by the empty-query change. `Test.Ghostty.FFI.Support.bytesVia` already accepts `SUCCESS` for an empty size query. |
| **The sized-struct rule**: the leading `size` is ≥ the struct's first released size (else `INVALID_VALUE`), and zero always means the default. Never nested, in arrays, or by value. There is a new `ghostty_sized_init(ptr, size)` for bindings. | `withSize` and `withSized` already zero and set `size` from `hsc2hs`. Keep them. `ghostty_sized_init` → `coverage.txt` `never` (it duplicates `withSize`, and the layout test checks sizes). |
| **Sized → frozen** (the `size` field removed): `GhosttyGridRef`, `GhosttySelection`, `GhosttyStyle`, and every **callback payload**. The payloads are `GhosttyClipboardWrite`, `GhosttyClipboardRead`, `GhosttyTerminalDesktopNotification`, `ProgressReport`, `ProgramStatus` and `SemanticPrompt`. Zero-initialise them (`{0}`). | Records lose `size`. Delete `withSize` at `GhosttySelection` and `GridRef` construction (`Internal.Format:214`, `:225`, `refTemplate`). **`ShortStruct` becomes unreachable**, because no callback receives a sized struct any more: the check in `Internal.Callbacks:432–440` and the `GhosttyError` constructor go, and so does §4.7's paragraph. The two clipboard *replies* stay sized. |
| **Still sized** (13): `PlacementRenderInfo`, `MouseEncoderSize`, `Paste`, `RenderStateRowSelection`, `RenderStateCursor`, `RenderStateColors`, `TerminalSelect{Word,WordBetween,Line}Options`, `TerminalWriteStop`, `TerminalMemoryUsage`, `ClipboardWriteReply` and `ClipboardReadReply`. | No change. |
| **`GHOSTTY_ENUM_TYPED`** no longer expands to `: int` under GCC ≥ 13 or clang's extension. It is typed only in C23 and C++11. | ABI-neutral, because `MAX_VALUE` padding forces 4 bytes. The layout test's enum size check confirms it. The newtypes stay `CInt`. No action beyond running the test. |
| **ELF symbol versioning** (`ghostty_*@@GHOSTTY_VT_1.0`), SONAME `libghostty-vt.so.1`, and every symbol exported in every build. | Transparent to GHC linking, static or shared. `LD_LIBRARY_PATH` from the build script still works. |
| **`GHOSTTY_DEPRECATED(msg)`** attribute. | Nothing at 1.0. Later: a deprecated binding is a C warning in the capi stub, which `-Werror` does *not* cover (GHC's flag, not gcc's). The safety fingerprint catches the doc change. |

### 3.2 Header by header

| Header | Change | Bound by canopy? | Action |
| :--- | :--- | :--- | :--- |
| `version.h` | New: version macros. | — | Bind `GHOSTTY_VT_VERSION` (hsc2hs `#const_str`) in `Ghostty.FFI.BuildInfo`. Its use is in §6.4. |
| `build_info.h` | `ghostty_version()`, `FEATURE_*` keys 11–19. | yes | Bind `ghostty_version` (`unsafe`). The new enum values get patterns and descriptor rows from the generator. |
| `allocator.h` | Vtable members become named typedefs (`GhosttyAllocator{Alloc,Resize,Remap,Free}Fn`), with `void *reserved[4]`; `resize` and `remap` are optional. | vtable `never` | Add the four typedefs to `coverage.txt` as `never` (allocators are not marshalled, §3.3). |
| `types.h` | `GhosttyCursorStyle` and `GhosttyImageLayer` (+ `MASK_*` defines) are merged here. Also new: `GhosttyCodepointBuffer`, `GHOSTTY_UNSUPPORTED`, `ghostty_sized_init` and `GHOSTTY_DEPRECATED`. `GhosttyFormatterFormat` gets explicit values. | yes | `GhosttyCursorStyle` and `GhosttyImageLayer` move into `Ghostty.FFI.Types` (one module per header). `CodepointBuffer`: bind its layout, since it is a `GRAPHEMES_BUF` value type, or list it as `M6.x` (canopy reads `GRAPHEMES_UTF8`, `Internal.Render:444`). |
| `terminal.h` | `GhosttyTerminalCursorStyle` is removed (→ `GhosttyCursorStyle`). `GHOSTTY_SCROLL_VIEWPORT_*` → `GHOSTTY_TERMINAL_SCROLL_VIEWPORT_*`. `DATA_CURSOR_STYLE` → `DATA_CURSOR_SGR_STYLE` (same value, still `GhosttyStyle`). `scroll_viewport` returns a result. Every option renumbers. Callback payloads are frozen. `OPT_GLYPH_PROTOCOL` is **off by default** (§4). | yes | The generator handles the names. Hand-written: the `ScrollViewport…` arms in the tagged union, `Internal.Keys:412–417` (`cursorStyle` → `GhosttyCursorStyle…`) and `:67`. The callback records drop `size`. |
| `formatter.h` | **Redesigned.** `GhosttyFormatter{Screen,Terminal}Extra` and `GhosttyFormatterTerminalOptions` are removed. `ghostty_formatter_terminal_new(alloc, &f, terminal)` takes no options. There is a new `ghostty_formatter_set` / `_get` with `GhosttyFormatterOption` (`EMIT`, `UNWRAP`, `TRIM`, `SELECTION`, `EXTRAS`) and a `GhosttyFormatterExtras` bitmask (`uint32_t`, 12 `#define`s). The formatter **copies** the selection. **Defaults: `TRIM = true` and `EXTRAS = PALETTE \| STYLE \| HYPERLINK`** (§4). | yes | Rewrite `Ghostty.FFI.Formatter` (`{{enum}}` for the option, `{{defines}}` for the extras). Rewrite `Internal.Format`'s `formatWith`, `terminalOptions` and `terminalExtra`: create, `set` each option explicitly, format. Two shims go. |
| `selection.h` | `GhosttySelection` is frozen. **`ghostty_terminal_selection_format_{buf,alloc}` and `GhosttyTerminalSelectionFormatOptions` are removed**: format a selection through the formatter's `OPT_SELECTION`. `select_output` takes `const GhosttyGridRef *`. Gesture enums gain `INVALID`. | yes | Remove the two imports, their shims, `safety.txt` rows, layout row and `Test.Ghostty.FFI.Formatter` cases (re-express them through `OPT_SELECTION`). `select_output` binds directly (shim gone). |
| `grid_ref.h`, `grid_ref_tracked.h` | `GhosttyGridRef` is frozen. `out_len` → `out_written`. The out-of-range and no-value results are unchanged (checked). | yes | Record drops `size`. `Internal.Terminal:268`/`:328` (`INVALID_VALUE` → absent) stay correct. |
| `style.h` | **`GhosttyStyle` is frozen at 16 bytes**: three `GhosttyStyleColor`s, `GhosttyStyleFlags flags` (`uint16_t`), and `uint16_t reserved`. The 8 bools and `int underline` become `GHOSTTY_STYLE_FLAG_*` bits 0–7, with the underline in bits 8–10 (`GHOSTTY_STYLE_UNDERLINE_SHIFT/MASK`). `GhosttyStyleColor.tag` is now `uint8_t` (it was the enum). The value union lost its `uint64_t _padding`. | yes | `Style.hsc.in`: `{{defines style.h GhosttyStyleFlags GHOSTTY_STYLE_FLAG_}}` and the shift and mask as hand-written constants (the underline accessor is a function-like macro). The tagged-union `Storable` reads the `Word8` tag and converts it. `Internal.Convert`'s `Style` conversion decodes bits; the public `Style` value is unchanged. |
| `render.h` | `GhosttyRenderStateCursorVisualStyle` → `GhosttyCursorStyle`, and `GhosttyRenderStateImageLayer` → `GhosttyImageLayer`. `_OPTION_` → `_OPT_`, with `INVALID`. "No value" keys return **`NO_VALUE` instead of `INVALID_VALUE`** (§4). `GRAPHEMES_BUF` takes a `GhosttyCodepointBuffer *`. | yes | Hand-written: `Internal.Render:258`, `:293`, `:360–363`, and `Render/Keys.hs:56`. The renamed option patterns are used in `Test.Ghostty.Effectful.Render` and `Internal.Render`. |
| `key/event.h` | Setters return results. **`set_utf8(event, GhosttyString)` and `GhosttyString get_utf8(event)`** pass a struct **by value**. | yes | Two **new shims** (`{{shim ghostty_key_event_set_utf8}}`, `{{shim ghostty_key_event_get_utf8}}`). Callers in `Internal.Encode` and `Test.Ghostty.FFI.Encode:63–68`, `:180` change shape. The enum got explicit values; the order was checked as identical (175 keys). |
| `key/encoder.h` | `setopt` → `set` (with a result), `encode` uses `uint8_t`, and the options renumber. | yes | Above. |
| `mouse/event.h`, `mouse/encoder.h` | The same pattern. `get_button` returns `GhosttyResult`. | yes | `Test.Ghostty.FFI.Encode:97–100` changes from `1`/`0` to a result. |
| `modes.h` | 25 mode macros renamed to the Zig names: `KAM` → `DISABLE_KEYBOARD`, `SRM` → `SEND_RECEIVE_MODE`, `DECCKM` → `CURSOR_KEYS`, `X10_MOUSE` → `MOUSE_EVENT_X10`, `LEFT_RIGHT_MARGIN` → `ENABLE_LEFT_AND_RIGHT_MARGIN`, `{NORMAL,BUTTON,ANY}_MOUSE` → `MOUSE_EVENT_{…}`, `{UTF8,SGR,URXVT,SGR_PIXELS}_MOUSE` → `MOUSE_FORMAT_{…}`, `ALT_SCROLL` → `MOUSE_ALTERNATE_SCROLL`, `NUMLOCK_KEYPAD` → `IGNORE_KEYPAD_WITH_NUMLOCK`, `ALT_SENDS_ESC` → `ALT_SENDS_ESCAPE`, `REVERSE_WRAP_EXT` → `REVERSE_WRAP_EXTENDED`, `ALT_SCREEN_SAVE` → `ALT_SCREEN_SAVE_CURSOR_CLEAR_ENTER`, `SYNC_OUTPUT` → `SYNCHRONIZED_OUTPUT`, `COLOR_SCHEME_REPORT` → `REPORT_COLOR_SCHEME`, `VISIBILITY_REPORT` → `REPORT_VISIBILITY`, `IN_BAND_RESIZE` → `IN_BAND_SIZE_REPORTS` and `PASTE_EVENTS` → `KITTY_PASTE_EVENTS`. Values are unchanged. `mode_report_encode` uses `uint8_t`. | yes | `{{modes}}` regenerates. `Internal.Convert:154–196` maps to the new patterns. The public `ModeKey` names are decision D-3. |
| `osc.h` | `OPT` gains `INVALID`. `osc_command_data` returns a result. **`DATA_CHANGE_WINDOW_TITLE_STR`'s output changes from `const char *` to `GhosttyString`** (§4). | yes | `Internal.Parse:282–284`. |
| `paste.h` | `paste_encode`'s output is `uint8_t *`. `terminal_paste`'s `out_written` → `out_pasted` (a rename only). | yes | The import changes. `cbits/gveff_paste.c`: re-read and recompile. |
| `size_report.h` | `size_report_encode` takes `const GhosttySizeReportSize *`, and outputs `uint8_t`. | yes | Shim gone; bind directly. |
| `sgr.h` | `sgr_attribute_tag` takes `const GhosttySgrAttribute *`. | yes | Shim gone. |
| `color_scheme.h`, `focus.h` | Output is `uint8_t *`. | yes | The imports change. |
| `kitty_graphics.h` | `GhosttyKittyPlacementLayer` is removed. The iterator's `LAYER` option is a **bitmask** (`uint32_t`, 0 = no filter). `GHOSTTY_KITTY_IMAGE_DATA_*` → `GHOSTTY_KITTY_GRAPHICS_IMAGE_DATA_*`, with `DATA_PTR`/`DATA_LEN` → `PIXELS`/`EXPECTED_LEN`. `ITERATOR_OPTION` → `ITERATOR_OPT` with `INVALID`. Oldest-image eviction is by transmission. | no (M6.15) | Coverage only. Update PLAN.md M6.15's notes. |
| `search.h` | Data and option keys renumber. `OPT_SELECT_NEXT`/`PREV` are removed in favour of the new functions `ghostty_search_select_next` / `_prev`. | no (M6.6) | Coverage only. Update M6.6's notes. |
| `snapshot.h` | Decoder options renumber. The byte format is outside the ABI promise. | no (M6.10) | Coverage only. Note it in §8.7 and M6.10: a snapshot store must tolerate decode failure after an upgrade. |
| `sys.h` | Options renumber (`INVALID = 0`). | no (M6.15) | Coverage only. |
| `wasm.h` | Docs only. | `never` | — |

### 3.3 Shim count

At 1.0, **15** functions pass a struct by value (19 before). Of the 17 shims canopy binds today:

- **Delete 6.** These functions now take pointers or are gone: `formatter_terminal_new`,
  `terminal_select_output`, `terminal_selection_format_buf`, `terminal_selection_format_alloc`,
  `size_report_encode` and `sgr_attribute_tag`.
- **Keep 11:**
  - `terminal_scroll_viewport`, `terminal_grid_ref`, `terminal_grid_ref_track`,
    `tracked_grid_ref_set` and `terminal_continuation_write`
  - `formatter_format` and `terminal_selection_contains`
  - `sgr_unknown_full` and `sgr_unknown_partial`
  - `mouse_event_set_position` and `mouse_event_get_position`
- **Add 2:** `key_event_set_utf8` and `key_event_get_utf8`. The latter *returns* a
  `GhosttyString`.

That leaves **13** bound shims. The other two by-value functions are `snapshot.h`'s (M6.10).
§3.3's counts ("Eighteen functions take a struct by value, and one returns one", "seventeen so
far") are rewritten. Upstream's sized-struct rule now *forbids* passing sized structs by value, so
new by-value functions will only ever be small frozen ones.

## 4. Silent changes: what no check catches

The generator, the signature check, the coverage check and the layout test together catch every
*shape* change above. They do not catch these. Each needs a deliberate code change and a test that
would have failed.

1. **OSC window title is now a `GhosttyString`, not a `const char *`.**
   - `Internal.Parse` (`Parse.hs:282`) peeks the out value as `ConstPtr CChar` and `packCString`s
     it.
   - Against 1.0, that reads `GhosttyString.ptr` (the first field, so it *appears* to work), then
     scans for a NUL that the string need not have. That is a **buffer over-read**, and the parse
     test may still pass by luck.
   - Fix: read a `GhosttyString` and copy it with `ghosttyStringBytes`.
   - Test: a title whose bytes are followed in the parser's buffer by more non-NUL bytes. Feed a
     longer OSC first, so the buffer is not fresh.
2. **Render-state "no value" is `NO_VALUE`, not `INVALID_VALUE`.**
   - `Internal.Render` (`Render.hs:525–531`) maps `INVALID_VALUE` on a cell's `BG_COLOR`/`FG_COLOR`
     to "default colour". Against 1.0, every default-coloured cell would raise
     `Invariant … NoValue`.
   - The same applies to the cursor colour (`render.h`: "Returns `GHOSTTY_NO_VALUE` if no explicit
     cursor colour is set").
   - Fix: match `ResultNoValue`. Audit every `ResultInvalidValue -> absent` branch in
     `ghostty-effectful/src` against the 1.0 docs. There are 6; the grid-ref ones at
     `Internal.Terminal:268`/`:328` and `Internal.Format:229` were checked unchanged.
   - The render tests catch this one, but only for a frame with default colours, which they have.
3. **A new formatter's defaults differ from a zeroed options struct.**
   - Before, `GHOSTTY_INIT_SIZED` options meant *no extras* and `trim = false` unless set.
   - Now a new formatter starts with `TRIM = true` and `EXTRAS = PALETTE | STYLE | HYPERLINK`.
   - Fix: `Internal.Format` sets **every** option on every formatter it makes (`EMIT`, `UNWRAP`,
     `TRIM`, `EXTRAS`, and `SELECTION` or NULL), so canopy's defaults (`formatOptions`:
     `trim = True`, `noTerminalExtras`) stay canopy's, independent of the library's.
   - The VT goldens (`test/golden/format/*.vt`) catch a miss. **Read any golden diff before
     accepting it** (rule 7). A palette or cursor-style prefix appearing in `colours.vt` is this
     bug, not a fix.
4. **`GHOSTTY_TERMINAL_OPT_GLYPH_PROTOCOL` is off by default.**
   - Before, Glyph Protocol APC sequences were handled. Now an embedder opts in.
   - canopy exposes `SetGlyphProtocol`, and no default is relied on by name. But a terminal that
     used to consume those APCs now ignores them.
   - Action: document the default on `SetGlyphProtocol` (`Terminal/Keys.hs:183`), and decide
     whether `newTerminal` enables it. Recommendation: leave it off. A REPL frontend has no glyph
     glossary to show.
5. **Smaller, behavioural:**
   - `ghostty_terminal_continuation_buf`'s size query returns `SUCCESS`/0 when nothing is pending,
     not `OUT_OF_SPACE`. Only `Test.Ghostty.FFI.Terminal:113` calls it, and it passes a buffer.
   - Kitty image eviction is now oldest-by-transmission.
   - Triple-click selection keeps blank lines.
   - The cursor pin's garbage flag is cleared after resize.
   - `RefCountedSet` reuses dead IDs, which affects style and hyperlink IDs (`CELL_DATA_STYLE_ID`)
     but not their meaning.

   None changes a canopy contract. Re-run the feed benchmark (§12.3), because the style-run scan
   and `RefCountedSet` changes touch the hot path. The allocation gate is on Haskell allocation,
   so it should not move; time may.

## 5. Decisions

Each is recorded in `DESIGN.md` in the bump's commit (rule 1).

**D-1. How the pin identifies the library. Recommended: an exact release version, from the signed
tarball.**

- Replace `ghostty-vt-ffi/GHOSTTY_COMMIT` with `ghostty-vt-ffi/LIBGHOSTTY_VT_VERSION` (`1.0.0`).
  This mirrors `tree-sitter-ffi/TREE_SITTER_VERSION`.
- Put the two tarballs' sha256 sums beside it in the build script, as Zig's are.
- `{{pin}}` becomes `ghosttyPinnedVersion`.
- The version check requires:
  - `ghostty_version()` equal to the pin;
  - empty `VERSION_PRE` and `VERSION_BUILD`;
  - every `FEATURE_*` reported true, because a library built with `-Dvt-features` would turn
    bound calls into `UNSUPPORTED`.
- Alternative: keep a commit pin and a git build at `vt-1.0.0`. This keeps the prefetch
  workaround, and only reports `1.0.0` if the shallow fetch includes the tag. Otherwise it reports
  `1.0.0-dev+<hash>`. Rejected.

**D-2. Runtime compatibility policy. Recommended: stay exact (`==1.0.0`) for now.**

- 1.0 makes "same major, minor ≥ headers" *memory-safe*.
- But canopy's argument that `UnknownEnum` is unreachable (§3.3, §4.7) rests on the exact pin. So
  does the layout test's exhaustiveness. A newer 1.x would add enum values.
- With D11 (static linking for releases), the runtime check only bites in development, where exact
  is right.
- Record the relaxation as **D-deferred-11**: accept any `1.y.z` with `y ≥ pin minor` at runtime,
  and make `UnknownEnum` a reachable, documented error. Trigger: a distribution package of
  libghostty-vt that canopy should link dynamically.
- `pkgconfig-depends` becomes `libghostty-vt ==1.0.0` (and `-static`). Check that cabal 3.16
  parses `==` on a pkg-config version, as §3.1 did for the range form. If it does not,
  `>=1.0.0 && <1.0.1`.

**D-3. Follow upstream's renames in the public effectful API. Recommended: yes, in the bump.**

- `ModeKey`'s constructors become the new macro names: `ModeDisableKeyboard`,
  `ModeSendReceiveMode`, `ModeCursorKeys`, `ModeMouseEventX10`, `ModeMouseEventNormal`,
  `ModeMouseFormatSgr`, `ModeSynchronizedOutput`, `ModeKittyPasteEvents`, …
- The terminal data key `CursorStyle` becomes `CursorSgrStyle`. Upstream renamed it because
  "cursor style" means the *shape* (`GhosttyCursorStyle`).
- §4.4's rule "a header row is a GADT constructor named for it" already requires this. The
  `Test.Ghostty.Effectful.Keys` name check enforces it for keys.
- Nothing outside `ghostty-effectful` uses these yet. After M2's oracle steps, this would ripple.

**D-4. `-Dcpu=baseline` for the release build. Recommended: yes.**

- `build-libghostty.sh` builds for the build machine's CPU (Zig's default `native`).
- That is fine for development. But D11 statically links the library into the shipped `canopy`,
  which would then crash with `SIGILL` on an older CPU.
- Add a `--release` mode to the script (or an env var) that passes `-Dcpu=baseline`. CI's release
  job uses it.
- Upstream's packaging guide recommends this for distribution.

## 6. canopy's machinery, item by item

### 6.1 `scripts/build-libghostty.sh`

- Download `libghostty-vt-$V.tar.gz` and `-deps.tar.gz` into `_deps/ghostty/`, check each sha256,
  and unpack them.
- Build with:

  ```sh
  zig build --system <deps dir> -Doptimize=ReleaseFast --prefix _deps/ghostty/$V
  ```

  - Use a **fresh, empty `ZIG_GLOBAL_CACHE_DIR`**. The upstream guide warns that a Zig 0.17 cache
    holding a package archive can crash the build with "reached unreachable code".
  - No `-Demit-lib-vt`: `LIB_VERSION` selects the library.
  - No `-Dlib-version-string`: the tarball reports `1.0.0` itself.
- **Delete `scripts/prefetch-zig-deps.py`** and the retry loop. Nothing is fetched by Zig any
  more, so the `TlsInitializationFailed` workaround of §3.1 is moot.
- Keep the Zig download and its sha256. `ZIG_VERSION=0.17.1` satisfies 1.0's `0.17.x`.
- The `emit` output is unchanged. pkg-config files are in `lib/pkgconfig` (keep the `share/`
  entry harmlessly, or drop it).
- Optionally verify the minisign signature when `minisign` is on `PATH`. The pinned sha256 is the
  required check.

### 6.2 Generator and checks (`scripts/`)

- `gen-ghostty-ffi.py`, `check-ghostty-{coverage,signatures,safety}.py`: read the headers from
  `_deps/ghostty/<LIBGHOSTTY_VT_VERSION>/include`.
- `{{pin}}`: emit `ghosttyPinnedVersion` from the new file.
- Templates: remove the directives the 1.0 headers no longer have:
  - `{{struct}}` for the three formatter structs and `TerminalSelectionFormatOptions`;
  - `{{enum}}` for `TerminalCursorStyle`, `RenderStateCursorVisualStyle` and
    `RenderStateImageLayer`;
  - the six `{{shim}}` lines.

  Then add:
  - `{{enum types.h GhosttyCursorStyle}}` and `{{enum types.h GhosttyImageLayer}}`, plus
    `{{defines}}` for the image-layer masks;
  - `{{enum formatter.h GhosttyFormatterOption}}` (or `enumT`, with the value types);
  - `{{defines formatter.h GhosttyFormatterExtras GHOSTTY_FORMATTER_EXTRA}}`;
  - `{{defines style.h GhosttyStyleFlags GHOSTTY_STYLE_FLAG_}}`;
  - the two key-event shims.
- Check `{{defines}}` against two new forms: the cast form `((GhosttyStyleFlags)(1u << 0))`, and
  macros continued with `\` (`GHOSTTY_FORMATTER_EXTRAS_DEFAULT`, `GHOSTTY_STYLE_UNDERLINE_MASK`).
  `#const` takes the macro by name, so the values are fine. Only the doc capture might not be.
  The function-like `GHOSTTY_STYLE_UNDERLINE(style)` is excluded by the existing regex.
- The `{{modes}}` regex still matches the renamed lines (checked: `#define … (ghostty_mode_new(n, b)) /**< … */`).
- `check-ghostty-signatures.py` already flags all 35 shape mismatches (Appendix A). After the bump
  it must report none. Confirm it compares `GhosttyStyleColor.tag` as `uint8_t`, a hand-written
  record.
- `check-ghostty-safety.py`: **every bound function's fingerprint changes**, because upstream
  rewrote all the Doxygen. So every import's `safe`/`unsafe` decision is re-read against the 1.0
  docs, not bulk `--update`d. Things to look for:
  - newly documented callbacks or long walks;
  - the formatter's `set`/`get` (no callback, short: `unsafe`);
  - `ghostty_version` (`unsafe`);
  - `key_event` setters (`unsafe`);
  - `terminal_set` (stays `safe`).

### 6.3 `coverage.txt`

- Add `never` for `GhosttyAllocator{Alloc,Resize,Remap,Free}Fn` (allocators are not marshalled)
  and `ghostty_sized_init` (duplicates `withSize`; the layout test checks sizes).
- Bind or list `GhosttyCodepointBuffer`.
- Everything else new is bound in the bump: the formatter's `set`/`get`/option/extras,
  `ghostty_version`, `GhosttyStyleFlags`, `GhosttyCursorStyle`, `GhosttyImageLayer`, and the
  renamed encoder setters.
- Keep the M6.6, M6.10 and M6.15 header entries; their contents changed (§3.2).

### 6.4 Layout test and version check (§3.4)

- The struct rows lose `size` samples: `GridRef`, `Selection`, `Style` and the callback payloads.
- `GhosttyStyle`'s row gets a sample with non-zero flags *and* underline bits.
- Delete four rows: the three formatter structs and `TerminalSelectionFormatOptions`.
- New enum rows: `GhosttyCursorStyle`, `GhosttyImageLayer` and `GhosttyFormatterOption`.
- The existing rows gain `…Invalid` in their total match.
- `BuildInfo`'s row gains the `FEATURE_*` values.
- `Test.Ghostty.FFI.BuildInfo` asserts three things:
  1. the linked `ghostty_version()` equals the pin;
  2. the headers' `GHOSTTY_VT_VERSION` equals the pin, which catches stale `_deps` headers;
  3. every feature is present.
- `runGhosttyPinnedTo` compares versions instead of commits. `VersionMismatch {pinned, linked}`
  keeps its shape, with version strings.

### 6.5 Hand-written FFI modules (`ghostty-vt-ffi`)

| Module | Change |
| :--- | :--- |
| `Style.hsc` (+ template) | The frozen `GhosttyStyle`. `GhosttyStyleColor`'s `Word8` tag. The union without `_padding`. |
| `Formatter.hsc` | The new API: `terminal_new` without options, and `set`/`get`. |
| `Selection.hsc` | Drop the two format functions; `select_output` by pointer. |
| `GridRef.hsc`, `Terminal/Callbacks.hsc` | Drop `size`. |
| `Terminal.hsc` | The `ScrollViewport` sum's arm patterns; `scroll_viewport`'s result. |
| `Key/Event.hsc`, `Key/Encoder.hsc`, `Mouse/*.hsc`, `Osc.hsc`, `Paste.hsc`, `Focus.hsc`, `ColorScheme.hsc`, `Modes.hsc`, `SizeReport.hsc`, `Sgr.hsc` | Import signatures (§3.1). |
| `BuildInfo.hsc` | `ghostty_version`, `GHOSTTY_VT_VERSION` and `ghosttyPinnedVersion`. |
| `test/cbits/gvtffi_test.c` | Recompile against 1.0; the MIME reader's types are unchanged. |
| `test/Test/Ghostty/FFI/*` | `Formatter` (no selection-format; re-test through `OPT_SELECTION`), `Encode` (`get_button`, utf8 shims), `Terminal` (scroll-viewport results), `Support` (no change expected). |

### 6.6 `ghostty-effectful`

| Module | Change |
| :--- | :--- |
| `Error` | `ShortStruct` goes; `ResultUnsupported` arrives. |
| `Internal.Effect` | The version check (§6.4, D-1). |
| `Internal.Callbacks` | Drop the clipboard-write `size` check and `ShortStruct` (`:432–461`; the *reply* stays sized). Payload records lose `size`. |
| `Internal.Format` | Formatter options by key, all set explicitly (§4.3). `GhosttySelection` and `GridRef` without `withSize`. |
| `Internal.Render` | `NO_VALUE` for absent colours (§4.2). `GhosttyCursorStyle`. The `_OPT_` patterns. |
| `Internal.Parse` | `osc_command_data`'s result. The title as a `GhosttyString` (§4.1). |
| `Internal.Encode` | `set`/`set_from_terminal`/`reset` results. `uint8_t` buffers in `bytesVia`. The utf8 shims. Mouse `get_button`. |
| `Internal.Keys`, `Internal.Convert` | `GhosttyCursorStyle`, the renamed modes and `DATA_CURSOR_SGR_STYLE`. Style flag decoding. |
| `Terminal.Keys`, `Mode`, `Render.Keys` | Public renames (D-3). `SetGlyphProtocol`'s documented default. |
| `cbits/gveff_terminal.c`, `cbits/gveff_paste.c` | Recompile and re-read them against the 1.0 docs (ENQ, XTVERSION, RENDER_HOLD and the MIME reader are unchanged in shape). |
| Tests | `Keys` (`ModeSrm` etc. at `:238–242`; the `defaultSettable` helper's `INVALID_VALUE`), `Render` (renamed patterns), `Events` (`SetMode ModeSyncOutput` → the new name), and format goldens (expected unchanged: §4.3). |

### 6.7 CI (`.github/workflows/ci.yml`)

- The cache key: from `GHOSTTY_COMMIT` and `prefetch-zig-deps.py` to `LIBGHOSTTY_VT_VERSION` and
  `build-libghostty.sh`. The path is `_deps/ghostty/<version>`.
- No other step changes. The checks keep their names.

## 7. The plan

**M1.11 — libghostty-vt 1.0.0.** One CR, because no intermediate state builds: the headers, the
library and the binding move together. It is organised as these commits, in this order, so it
reviews in pieces:

1. **Verify the release.** Get `minisign`. Check `SHA256SUMS.minisig` with the key in §2, and both
   tarballs against `SHA256SUMS`. Record the sums in the script.
2. **Build and pin** (§6.1, D-1, D-4). Write `LIBGHOSTTY_VT_VERSION`, delete `GHOSTTY_COMMIT` and
   `prefetch-zig-deps.py`, rewrite `build-libghostty.sh`, point the four scripts at the version,
   and update CI's cache key. Run it once on a clean `_deps/`.
3. **Regenerate** (§6.2). Edit the templates, then run `gen-ghostty-ffi.py`. `--check` must pass.
4. **FFI hand-written code** (§6.5) until `ghostty-vt-ffi` builds with `-Werror`, then
   `check-ghostty-signatures.py` (0 failures) and `check-ghostty-coverage.py` (§6.3).
5. **Safety re-read** (§6.2). Read each bound function's 1.0 doc, adjust any import mode and
   comment, then `check-ghostty-safety.py --update`. The commit message lists every changed mode
   (expected: none).
6. **Layout and version tests** (§6.4). `ghostty-vt-ffi-test` passes, including the new
   header-version and feature assertions.
7. **Effectful** (§6.6): the silent fixes of §4 first, each with its test written to fail first
   (rule 7: numbered regression cases, though these are migration hazards rather than shipped
   bugs). Then the mechanical changes, then D-3's renames.
8. **Goldens and benchmark.**
   - `cabal test all`, with the format goldens expected unchanged. Read any diff (§4.3).
   - The doctests for the three packages.
   - The feed benchmark against the allocation baseline. Do not regenerate the baseline unless it
     moved for a reason the commit states.
9. **Documents** (§8), with the CHANGELOG entry.

Exit criteria:

- Every CI step (§2.8, 1–8) is green.
- `check-ghostty-safety.py` is clean after a per-function read.
- No golden or baseline changed without a stated reason.
- `canopy`/`tendril` are untouched, apart from D-3 renames if any reached them (none today).

**Follow-ups, not part of M1.11:**

- Update PLAN.md M6.6 (search: use `select_next`/`select_prev`), M6.10 (snapshots: the format is
  not ABI, so stored sessions need a fallback), and M6.15 (kitty: `GhosttyImageLayer` mask,
  `PIXELS`/`EXPECTED_LEN`; sys options renumbered) with the 1.0 shapes. These can go in M1.11's
  docs commit.
- Consider binding `GhosttyCodepointBuffer` and `GRAPHEMES_BUF` if M2's width oracle wants
  codepoints without a UTF-8 round trip.

## 8. Document changes (in the bump, rule 1)

- **`DESIGN.md`**
  - **§3.1:** tarball and deps tarball, `--system`, the version pin, `==1.0.0`. Delete the
    prefetch paragraph. Static linking is unchanged.
  - **§3.2:** the result list gains `UNSUPPORTED`. The `INVALID = 0` key convention. Sized structs
    now as upstream's rule states, with the frozen ones named. The "not yet stable" bullet becomes
    the 1.x ABI promise. Add the formatter's keyed options.
  - **§3.3:** shim counts (§3.3 of this file). The formatter and selection-format notes in the
    safe/unsafe paragraph ("selection formatting walk[s] the whole screen" → only the formatter's
    format calls).
  - **§3.4:** the version check by release version, header version and features. Delete the
    `-Dlib-version-string` stamping paragraph.
  - **§4.7:** remove `ShortStruct` and its paragraph. Add `ResultUnsupported` and when it is an
    `Invariant`.
  - **§4.8:** `Format` uses keyed options. `Search` will use `select_next`/`prev`.
  - **§8.7:** snapshots outside the ABI promise.
  - **§14.1:** rewritten. The risk is now the snapshot format, Zig version coupling (each 1.x may
    require a different Zig), and the deferred dynamic-link policy (D-deferred-11).
  - **§14.2:** the `pkgconfig-depends` and `ghostty_build_info` rows get a new dated result (the
    1.0 checks).
  - **§15.2:** add D-deferred-11.
  - **§16.1:** the reference is `https://lib.ghostty.org/vt/1.0.0/`.
- **`CLAUDE.md`** (root): rule 6 names `LIBGHOSTTY_VT_VERSION`. The Paths row drops
  `prefetch-zig-deps.py`. The toolchain line says "Zig 0.17.x, as libghostty-vt 1.0 requires".
- **`ghostty-vt-ffi/CLAUDE.md`:**
  - The pin bullet (`GHOSTTY_COMMIT` → `LIBGHOSTTY_VT_VERSION`, built from the release tarball).
  - The Template Haskell note.
  - Add a line: key enums start at `INVALID = 0`, which no binding maps.
- **`ghostty-effectful/CLAUDE.md`:** the last sentence of the callbacks bullet ("A sized struct a
  callback receives is read only after checking its `size`") goes. The formatting bullet says
  every option is set explicitly.
- **`PLAN.md`:**
  - Add M1.11 with this file's §7 as its bullets, marked done when it lands.
  - Change "M1 (PRs #2–#12)" accordingly.
  - The M6.x notes (§7, follow-ups).
- **`CHANGELOG.md`:** one entry under M1, stating:
  - the pin moved and why;
  - the public renames (D-3), the removal of `ShortStruct` and the addition of `ResultUnsupported`;
  - the three silent hazards fixed, with their regression cases.

## 9. Risks and open questions

- **Exhaustive safety re-read.** About 170 functions, all with rewritten docs. It is the slowest
  part of the bump and the one that cannot be mechanised. Budget for it; do not `--update` in bulk.
- **cabal and `==1.0.0` in `pkgconfig-depends`.** Unverified (D-2). The fallback is the narrow
  range.
- **Zig 0.17 cache crash.** Upstream says to use an empty `ZIG_GLOBAL_CACHE_DIR` with `--system`.
  `_deps/zig-cache` is reused today, so make it per-version and clear it before building.
- **`GHOSTTY_DEPRECATED` won't fail `-Werror`.** A future deprecation shows up as a C warning in
  GHC's capi stubs only. Consider `-optc-Werror=deprecated-declarations` in `ghostty-vt-ffi`'s
  common stanza at the next minor bump. That is a stanza change, so all seven `.cabal` files get it
  (rule 3).
- **Future 1.x bumps** should be small. They are additions, so expect new enum values (the layout
  test fails until named), new functions (the coverage check fails until bound or listed) and
  doc-only safety fingerprints. That is exactly what the existing checks are for.

## Appendix A. How this was reviewed (reproducible)

All in a scratch directory. The working tree was untouched.

1. `git fetch --depth 400 origin tag vt-1.0.0` in `_deps/ghostty/src`. The pin
   `c7f68a7f…` is an ancestor of `vt-1.0.0` (`0fa3d577…`), 118 commits apart.
2. `git archive` both `include/` trees. Strip comments with `gcc -fpreprocessed -dD -E -P` and
   `diff -r`. The only new file is `version.h`.
3. Compare each keyed enum value's documented value types between the versions. This found the OSC
   title, `GRAPHEMES_BUF`, the kitty layer mask and `CURSOR_SGR_STYLE`.
4. Read the commit messages of the ABI commits (`657e82d1a`, `742fc2f2e`, `9e0d83cee`, `02cf8a6a2`,
   `8c0c45c18`, `5db83d768`, `0b7f9c156`, `f80e4d5c6`, `f5dcef045`, `1830cd250`, `7b06c0541`) and
   `PACKAGING-LIBGHOSTTY.md`.
5. Run canopy's own tools against the 1.0 headers (`--include`):
   - `gen-ghostty-ffi.py` renders once 6 shim and 7 struct or enum directives are removed;
   - `check-ghostty-signatures.py` reports 35 mismatches, all listed in §3;
   - `check-ghostty-coverage.py` reports 18 unlisted symbols (§6.3);
   - `check-ghostty-safety.py` reports a changed fingerprint for every bound function.
6. Download the release tarball, check its sha256, and diff its `include/` with the tag's
   (identical).
