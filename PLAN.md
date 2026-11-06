# Canopy — Implementation plan

The milestones of DESIGN.md §13, broken into steps. **Each step is one CR**: it builds with
`-Werror`, passes every CI step, adds its `CHANGELOG.md` line, and updates `DESIGN.md` in the same
commit if it changes a decision (§2.9). Section references (§) are to DESIGN.md.

M0 is done (PR #1). Steps are numbered `M<n>.<k>` and are in dependency order within a milestone.

## Tracks

```
M1 ghostty ──────────┬──> M2 tendril (oracle steps) ──┐
M2 tendril (pure) ───┘                                ├──> M3 canopy ──> M4 canopy-side ──> M5 ──> M6 ──> M7
M4 tree-sitter binding (M4.1–M4.10) ──────────────────┘
```

- **M1**, **M2's pure layers** (M2.1–M2.2, M2.4–M2.7) and **M4's binding half** (M4.1–M4.10) depend on
  nothing but M0 and can proceed in parallel.
- M2's oracle steps (M2.3, M2.8, M2.9, M2.10) need M1's formatter, encoders and widths.
- M3 needs M1 and M2. M4's canopy half (M4.11 onward) needs M3. M5 needs M4.

## Every CR

- Haddock on every new export (100% floor); doctests where an example is the clearest spec.
- New modules are placed in their §1.3/§2.6/§6.1 layer. A new layering rule comes with
  `lint/fixtures/{reported,allowed}` cases.
- Goldens are read before they are accepted, and the commit says why the output is right.
- A bug fixed along the way gets a numbered regression case.
- An open check from §14.2 that the step resolves moves into the §14.2 table in the same CR.

---

## M1 — ghostty

Exit: formatter goldens; every typed key exercised (§13).

### M1.1 Layout test, full

- `ghostty-vt-ffi-test` parses `ghostty_type_json()` and compares every struct size and field offset
  with the `hsc2hs` values, for every struct `Ghostty.FFI.Types` binds (§3.4).
- An enum-coverage check: every value the binding names exists in the header.
- Structured so that later steps add a struct by adding one table row.

### M1.2 FFI: terminal

- `Ghostty.FFI.Terminal`: `new`/`free`/`reset`/`resize`, `vt_write`, `vt_write_until` with the
  boundary enum, keyed `get`/`set`, scroll viewport, grid refs and tracked refs.
- Every `GhosttyTerminalData`/`GhosttyTerminalOption` key and callback type as `hsc2hs` enums and
  structs (newtypes + `COMPLETE` pattern synonyms, §3.3).
- `safe`/`unsafe` decided per import, with the reason in the import's comment.
- Layout-test rows for every new struct.

### M1.3 FFI: render state, formatter, encoders, parsers, unicode

- `Ghostty.FFI.Render` (render state, row and cell iterators, dirty iteration), `.Format`,
  `.Encode` (key, mouse, focus, paste, size report, mode report), `.Parse` (OSC, SGR), `.Unicode`.
- The rest of the headers M1.2 bound in part (`screen.h`, `color.h`, `modes.h`, `osc.h`,
  `size_report.h`, `io.h`), and the output types of terminal keys that live in these headers:
  `GhosttySelection`, `GhosttyMouseShape`, `GhosttyKittyKeyFlags`, the `GhosttyKittyGraphics`
  handle, and `GhosttySgrUnderline` (the `int` in `GhosttyStyle`). M1.5 needs all of them. Each
  header's line leaves `ghostty-vt-ffi/coverage.txt` as it is bound.
- Layout-test rows for each. May be split into two CRs (render+format, encoders+parsers+unicode) if
  it reviews too large.

### M1.4 `Ghostty` effect and scoped terminals

- `Ghostty.Effectful`: the static `Ghostty` effect, `runGhostty` with the version check returning
  `Either VersionMismatch` (§4.7), the owner-thread check on every operation (§4.6).
- `GhosttyError`; `Invariant` thrown for results the header says cannot happen.
- `Ghostty.Effectful.Terminal`: `withTerminal` (rank-2 region, `bracket` release), `resize`, `reset`.
- Tests: handle released on exception; use from a forked thread throws; version mismatch refuses.

### M1.5 Typed keys

- `TerminalData a` / `TerminalOption a` GADTs, one constructor per header row, `Maybe` where
  `NO_VALUE` is possible (§4.4); `getT`/`setT`.
- Tracked grid refs in the high-level API (blocks need them in M3).
- Test enumerates `[Some TerminalData]` (kept exhaustive by an incomplete-pattern match) and calls
  each on a fresh terminal. **Half of M1's exit condition.**

### M1.6 Events and `feed`

- C callbacks installed by the binding that only append to the terminal's `IORef` buffer (§4.5).
- `feed` over `vt_write_until … BOUNDARY_OSC_END`, attaching cursor position to each
  `SemanticPrompt`.
- `TerminalConfig` answers queries (colour scheme, DA, ENQ, XTVERSION, size); updatable via `setT`.
- Tests: event order across mixed OSC 133 / bell / title streams; query replies arrive as `WritePty`.

### M1.7 Render frames

- `Ghostty.Effectful.Render`: render state handle, `withFrame` (frame tag `f`), `dirtyRows`,
  `cells` copying into `Cell`/`Style`/`Rgb`/`Hyperlink`, cursor and palette.
- Tests: only dirty rows reported after a partial update; copied cells survive the next update.

### M1.8 Formatter and goldens

- `Ghostty.Effectful.Format`: plain / VT / HTML of a terminal, a selection, a row range.
- tasty-golden goldens over canned VT streams (colours, wide chars, wraps, hyperlinks).
  **The other half of M1's exit condition.**

### M1.9 Encoders, parsers, unicode

- `Ghostty.Effectful.Encode` (configured from a terminal's modes), `.Parse`, `.Unicode`.
- `paste` returns `Either Rejected` (§4.7).
- Unit tests per encoder; these are M2's oracles.

### M1.10 First benchmark and CI step 8

- `tasty-bench` `feed` throughput benchmark in `ghostty-effectful`.
- Committed allocation baseline; CI step 8 gates allocation, reports time (§12.3, §2.8).

---

## M2 — tendril

Exit: decoder roundtrip and render-diff properties pass; `tendril-demo` usable as a daily readline.

### M2.1 Unicode tables

- `scripts/gen-unicode-tables.py`: pinned UCD version → `Tendril.Text.Unicode.Tables` (grapheme
  break, extended pictographic, `InCB`, East Asian width), committed; `--check` mode in CI like the
  tree-sitter shims (§5.3).

### M2.2 T0 text

- `Tendril.Text.Units` (`Offset (u :: Unit)`, no `Num`), `.Buffer` over `text-rope`, `.Grapheme`
  (UAX #29), `.Width`. `StrictData` (§10.7).
- hlint rule banning `coerce` on unit types outside `Tendril.Text.Units` / `Canopy.Units` (§10.1),
  with fixtures.
- Tests: UCD `GraphemeBreakTest.txt` conformance; unit conversions round-trip.
- First QuickCheck suite: restore the fixed `QUICKCHECK_SEED` / `--quickcheck-replay` in CI.

### M2.3 Width oracle

- `tendril-test` gains its test-only `ghostty-effectful` dependency (§1.3, rule 2's allowed edge).
- Widths agree with libghostty for every codepoint and a cluster corpus (§11.2).

### M2.4 T1 editing

- `Tendril.Edit.Command` (`Command app m` indexed by `ModeKind`), `.Motion`, `.KillRing`,
  `.Selection`, `.Register`; `EditorState` and command application.
- Properties: cursor always on a grapheme boundary; rope metric invariants preserved (§11.3).

### M2.5 Undo tree

- `Tendril.Edit.Undo`: tree, coalescing by command kind and time.
- Properties: undo/redo inverse; branches reachable.

### M2.6 Keymaps: trie and Emacs

- `Tendril.Keymap` (`Trie KeyChord (Command app m)`), `Tendril.Keymap.Emacs`.
- Built-in keymaps are mode-checked at compile time.

### M2.7 Input decoder

- `Tendril.Term.Input`: incremental pure decoder — legacy with ESC timeout, CSI-u / kitty with all
  flags, modifyOtherKeys, bracketed paste, SGR and SGR-pixel mouse, focus, terminal replies (§5.7).
- Unit tests per encoding.

### M2.8 Decoder roundtrip

- Property: random key/mouse events under random modes, encoded by libghostty, decoded by tendril,
  come back equal (§11.2). **Half of M2's exit condition.**

### M2.9 T4 layout and screen goldens

- `Tendril.Render.Style`, `.Doc`, `.Layout`, `.Grid`.
- Golden harness: emitted VT fed into a headless libghostty terminal; golden = plain screen + style
  dump (§11.2).

### M2.10 Diff and output

- `Tendril.Render.Diff`, `Tendril.Term.Output` (synchronized output 2026 when supported).
- Property: render A, then diff A→B, equals render B from scratch, as ghostty screens.
  **The other half of M2's exit condition.**

### M2.11 Plug-in effects and history

- `Highlighter`, `Completer` (`Async`, revision-tagged), `Hinter`, `Validator`, `Structure`,
  `History` effects with trivial interpreters; append-only file history (§5.5).
- Tests: stale-revision completion results dropped.

### M2.12 T3 widgets

- `Tendril.Widget.Prompt`, `.Menu`, `.Hint`, `.Popup`, `.Search` (incremental history search).
- Screen goldens per widget.

### M2.13 Terminal I/O

- `Tendril.Term.Tty`: raw mode under `bracket`, restored on `SIGTSTP`/`SIGCONT` and fatal signals;
  size and SIGWINCH.
- `Tendril.Term.Caps`: probing with DA1 sentinel, kitty keyboard, 2026, 2027 (§5.7).

### M2.14 Driver

- `Tendril.Effect` (`LineEditor`), `Tendril.Run` (`runLineEditor`), `Surface` with `live`/`commits`
  (§5.6); exported `EditorState`, `step`, renderer for canopy's own loop (§5.8).

### M2.15 `tendril-demo`

- A usable readline: Emacs keys, history file, word completion, ghost hints, commits from a timer to
  show the external printer.
- Manual check written up in the CR: used as a daily readline. **M2 exit.**

---

## M3 — canopy, generic

Exit: rlwrap parity and better — output while typing, progress bars, `help()` pager, Ctrl-C.

### M3.1 C0 vocabulary

- `Canopy.Optics` (`use`, `.=`, `%=`, `zoomMaybe` over static local state), `Canopy.Units`
  (`Pos space u`, `Space`).

### M3.2 PTY

- `cbits/` shim + `Canopy.Ffi.Pty` + `Canopy.Child.Pty`: `posix_openpt`, `posix_spawn` with
  `POSIX_SPAWN_SETSID`, winsize, `tcgetattr` from the master (§6.2).
- Tests: child is session leader with a controlling tty; termios changes visible from the master.

### M3.3 Child effect and fake REPL

- `Child` effect; `runChildPty`, `runChildScripted` (§6.3).
- `canopy-fake-repl` test executable: echo REPL emitting OSC 133 (§11.4).
- Test: real PTY path against the fake REPL.

### M3.4 App core and shell

- `Canopy.App.Event`, `.Command`, `.Model`, `.Update` (pure), `ChildState` (§6.4).
- `Canopy.App.Run`: feeder threads on one bounded STM queue; event loop in a row with `Ghostty` and
  without `Concurrent` (§4.6). `Canopy.App.Interpret`, `Canopy.Cli`.
- hlint rule: no wildcard `_ ->` in `Canopy.App.Update` (§10.4), with fixtures.
- Test: `update` on hand-written event sequences.

### M3.5 Profiles and OSC 133 framing

- `Canopy.Profile` type (§7.1) and `Canopy.Profile.Builtin.Generic`.
- `Canopy.Session.Frame`, `.Block`, `.Transcript`: blocks as tracked grid-ref pairs; derived `C`/`D`
  (§7.3).
- Scripted-child tests: blocks delimited correctly across chunk boundaries.

### M3.6 View

- `Canopy.App.View`: ghostty cells → tendril `Doc` with palette resolution; running block in the
  live region, finished blocks committed (§8.3).
- Frame coalescing (~8 ms) and `RenderHold` suppression (§12.2).
- Screen goldens: output while typing; a `\r` progress bar animates in place and is committed once.

### M3.7 Submission and queueing

- `SubmitStyle`: `Lines`, `Bracketed` (auto when child set 2004, via the paste encoder), `Wrapped`,
  `BlankLineTerminated`, `Custom` (§7.5).
- Input queued in `Starting`/`Busy`, type-ahead shown dimmed.

### M3.8 Echo suppression

- `CookedChild` (clear `ECHO` from the master before submit) and `ChildEditorEchoes` (bytes sent +
  end-of-input sequence) (§7.4); runtime fallback with a one-time notice.
- Resolves §14.2's open check on clearing `ECHO` from the master.

### M3.9 Setup, python and ghci profiles

- Typed `SetupStep`s run in `Starting`, hidden startup block shown on failure; refused inputs
  (`:set prompt`) (§7.2).
- `Canopy.Profile.Builtin.Python` (bootstrap) and `.Ghci`.

### M3.10 Regex and sentinel framing

- `PromptRegex` matched on child-terminal rows; `Sentinel` (§7.3). Choose the regex library and record
  it in DESIGN.md.
- Exact-block features unavailable for `PromptRegex` profiles by type (§14.4).

### M3.11 Interrupts, exit, resize

- Ctrl-C → `VINTR` when busy / clear input when editing; Ctrl-D → `VEOF` on empty input (§6.2).
- `Exited` with restart offered, transcript kept. Host resize propagated to child and child terminal.

### M3.12 Passthrough

- `Paging`: host alternate screen, child render state mirrored, host input decoded by tendril and
  re-encoded by libghostty from the child's modes; escape chord (§8.6).
- Paste into a full-screen child through the safety check; `Rejected` asks (§8.5).

### M3.13 User profiles in TOML

- `Canopy.Config.Codec`: `toml-parser` codec to `Profile`, applicative validation reporting every
  error with locations; `extends` via `Profile Partial` (§7.1, §10.5).

### M3.14 Benchmarks and e2e harness

- Keystroke-to-frame and child-throughput benchmarks against §12.1's budgets.
- `canopy-e2e` with the host side attached to a headless libghostty terminal (§11.5); nightly
  workflow with pinned ghci and python. Scenarios: output while typing, progress bar, `help()` pager,
  Ctrl-C. **M3 exit.**
- Resolves §14.2's musl spawn check, if a musl job is added here; otherwise it stays open.

---

## M4 — tree-sitter

Exit: §5B.7 passes; built-in profiles' grammars bind and their queries compile under `Strict`.

### M4.1 `tree-sitter-ffi` complete

- `TreeSitter.FFI.Api` (every scalar/pointer function, `safe`/`unsafe` per §5A.3 on each import),
  `TreeSitter.FFI.Types` (all structs and enums), shims complete.
- Vendored `tree-sitter-json` at a pinned tag in the test suite (§5B.7).
- Tests: struct sizes vs `sizeof`; each shim against the direct C call (a C test helper calls both).

### M4.2 Budget, input reader, dynamic language

- `tsffi_budget_progress` / `TsffiBudget`, `tsffi_buffer_read`, `TreeSitter.FFI.Budget`,
  `TreeSitter.FFI.Language` (`capi "dynamic"`) (§5A.3, §5A.4).
- Tests: deadline and cancel word stop a parse.

### M4.3 Trees, nodes, pure reads

- `Tree g`, `Node g` (ForeignPtr-kept), `TreeSitter.Internal.Pure` with its purity argument in the
  header (§5B.2); every node read and `toSExpr`; `nodeText`.
- Unsafety-audit `grep` updated (§10.6).

### M4.4 Parse effect

- `Parse` effect, `runParse` with owner-thread check, `runParseIsolated`, `Budget`/`CancelToken`,
  `Aborted` (§5B.3).

### M4.5 Edits and incremental parsing

- `EditedTree`, `edit` (copy-then-edit), `splice`, incremental `parse`, `changedRanges`.
- Properties: incremental equals fresh; persistence; `splice` agrees with naive points (§5B.7).

### M4.6 Concurrency stress test

- Reads of one `Tree` from every capability under `-threaded +RTS -N` while another thread edits
  copies, checked against a single-threaded baseline (§5B.7). Backs §14.2's purity row.

### M4.7 Grammars

- `loadGrammar` (dlopen, ABI range, `LoadError`), `deriveGrammar` TH with `addDependentFile`,
  `bindGrammar`, `kindOf`, `fieldOf` (§5B.4).
- Tests on json; `bindGrammar` fails with the right names on a modified `node-types.json`.

### M4.8 Queries

- `Query g c`, `Capture`, `compileQuery`, `QueryError` with line/column, pure `matches`/`captures`
  (§5B.5).

### M4.9 Predicates

- All listed predicates and directives, decoded at compile time; `PredicatePolicy`.
- **Answer D-deferred-10** by running Helix's and nvim-treesitter's queries for the built-in languages
  through each candidate engine; record in DESIGN.md §15.
- Nightly cross-check against the `tree-sitter query` CLI where installed.

### M4.10 Highlighter and benchmark

- `TreeSitter.Highlight` with injections (cached per site) and locals (§5B.6).
- Benchmark: incremental reparse + highlight of a one-character edit (§12.3).

### M4.11 Grammar pins and `canopy grammar`

- `canopy/grammars/pins.toml`, committed `node-types.json` per built-in grammar,
  `Canopy.Syntax.Grammars` splices; `canopy grammar fetch|build` (§9.1).

### M4.12 Grammar store

- `Canopy.Syntax.GrammarStore`: Helix layout, `; inherits:`, compile every query at startup with
  line/column errors.
- Test: every built-in profile's grammar binds and its queries compile under `Strict`. **M4 exit.**
- Resolves §14.2's Helix-query-collection check.

### M4.13 Edit adapter and the tree in the model

- `Canopy.Syntax.Edit` (tendril edits → `splice`s); property across the adapter (§11.3).
- Current input's `Tree` in `Model`, parse as a `Command` with a few-ms budget, idle retry on `Aborted`.

### M4.14 Highlighting

- `Canopy.Syntax.Highlight`: closed `HighlightCapture`, total theme table incl. Helix capture names,
  over `changedRanges`; plugged in as tendril's `Highlighter`.

### M4.15 Completeness

- `Canopy.Syntax.Complete` as the `Validator`: ERROR/MISSING at end, unclosed brackets; profile rules
  (`:{`, trailing `\`).

### M4.16 Indent and structure

- `Canopy.Syntax.Indent` (`indents.scm`, closed `IndentCapture`), `Canopy.Syntax.Structure` (expand/
  shrink selection, bracket matching, auto-pairs).

---

## M5 — LSP

Exit: scripted-LSP session tests; nightly e2e with pyright/basedpyright and HLS.

### M5.1 Transport

- `Canopy.Lsp.Transport`: `Content-Length` JSON-RPC over stdio, as an effect with a scripted
  interpreter (expected requests, canned responses, fail on unexpected) (§11.4).

### M5.2 Client and capabilities

- `Canopy.Lsp.Client`: `SMethod`-indexed requests, initialize/shutdown, `$/cancelRequest` on
  superseded revisions. `Canopy.Lsp.Capabilities`: `Supports m` tokens (§10.3).
- `Canopy.Lsp.Rows` if any `row-types` field is touched.

### M5.3 Source maps and projections

- `SourceMap from to` (§10.2), `Shadow`, `Projection`; Generic and Python projections.
- Properties: source maps round-trip where they map. Goldens for python shadow documents.

### M5.4 Shadow document lifecycle

- Server spawn per profile, `didOpen`/`didChange`, on-disk `.canopy/session-<id>.<ext>` and generated
  `.gitignore` when the profile requires it; server crash → disabled with notice, restart offered.

### M5.5 REPL introspection

- `Introspection` hidden queries when `Ready`, purity declared, cache per (query, transcript length)
  (§9.3); ghci and python.

### M5.6 ghci projection

- Tree-sitter classification (imports hoisted, declarations renamed on shadowing, `canopy_it_N`,
  `x <- e` with type from `:type`); per-construct goldens (§9.2, §14.5).

### M5.7 Diagnostics

- Diagnostics mapped through the source map, earlier blocks' dropped, shown under the input; session
  toggle (§14.5).

### M5.8 Completion merge

- `Canopy.Intel` merge of LSP and REPL candidates and ranking (§9.4); wired as tendril's `Completer`
  with kinds/types and docs side panel.

### M5.9 Hover, type bar, signature help

- LSP `hover` with REPL fallback; live type of expression under cursor; `signatureHelp` (§8.2).

### M5.10 Semantic tokens

- `semanticTokens` overlay on tree-sitter spans when revisions match (§9.1).

### M5.11 Session tests and nightly e2e

- Whole scripted sessions (late completion with an old revision dropped, etc.); nightly e2e with
  basedpyright and HLS at pinned versions. **M5 exit.**

---

## M6 — depth

Exit: feature table §8 complete. Steps are mostly independent of each other.

### M6.1 Vi keymap

- `Tendril.Keymap.Vi`, `ViNormal`/`ViInsert` commands, cursor shape per mode; tree-sitter text
  objects (`textobjects.scm`) from canopy.

### M6.2 Helix keymap and user keybindings

- `Tendril.Keymap.Helix`; user keybinding files decoded at startup with all mode errors at once (§5.4).

### M6.3 History database

- SQLite `History` interpreter with context per entry; prefix up-arrow per profile; Ctrl-R fuzzy
  popup ranked by recency/project/success (§8.8).

### M6.4 History browser

- Prompt-less list, multi-select and insert (§8.8).

### M6.5 Blocks

- Alt-↑/↓ selection, copy input/output/both, re-insert and re-run, fold with "N more lines", timing
  and exit/exception markers (§8.3, §8.4).

### M6.6 Search

- `Ghostty.Effectful.Search` (FFI + effectful) and transcript search including folded output.

### M6.7 Export

- Script / plain / HTML export via the formatter (§8.4).

### M6.8 Rewind and checkpoints

- Profile purity marks; rewind *k* blocks by restart + replay, asking before impure ones; named
  checkpoints (§8.4, §8.7).

### M6.9 Sessions

- Transcript journal, `--continue` replay (§8.7).

### M6.10 Snapshots

- `Ghostty.Effectful.Snapshot` (FFI + effectful); exact screen restore without re-running.

### M6.11 Paste-transcript cleaning

- `pastePrompts` stripping, output lines dropped, original one undo away (§8.5).

### M6.12 Input modes

- `InputMode`s with trigger, prompt, grammar, per-mode history and submission transform; generic `;`
  shell and `:canopy` modes (§8.9).

### M6.13 Editor conveniences and LSP actions

- `EditExternal` (`$EDITOR`, evaluate on save), history ghost suggestions accepted with →,
  format-before-submit, code actions mapped back through the source map, go-to-definition in
  `$EDITOR`, `?`/`??` help and source keys (§8.1, §8.2).

### M6.14 ghcitty parity for ghci

- `:core`/`:stg` out of process, Hoogle, typed-hole completion, binding explorer, `Canopy.Watch`
  auto-reload as a block (§8.2, §8.4).

### M6.15 Rich output

- Kitty graphics placements (render-state FFI, `kitty_graphics.h`, and `sys.h`'s PNG decoder hook;
  passthrough when the host supports it), OSC 8
  hyperlinks end to end, opt-in output-as-code highlighting per profile (§8.3).

### M6.16 Node, Julia, utop profiles

- `Canopy.Profile.Builtin.Node`, `.Julia`, `.Utop`; resolves §14.2's node and julia setup checks.

### M6.17 §8 audit

- Walk every row of §8's tables, link each to its test, fix gaps. **M6 exit.**

---

## M7 — tooling

Exit: a client library test drives a session.

### M7.1 Serve mode

- `Canopy.Serve`: JSON-RPC on a socket — submit inputs, read structured blocks, subscribe to events
  (§8.7).

### M7.2 Client library and test

- A small client library and a test that drives a full session through it against the fake REPL.
  **M7 exit.**
