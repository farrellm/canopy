# Canopy — Implementation plan

The milestones of DESIGN.md §13, broken into steps. **Each step is one CR** (M6.13 and M6.14 are
lists of them): it builds with
`-Werror`, passes every CI step, adds its `CHANGELOG.md` line, and updates `DESIGN.md` in the same
commit if it changes a decision (§2.9). Section references (§) are to DESIGN.md.

M0 is done (PR #1), and M1 (PRs #2–#13; the performance work of D18–D21 followed in PR #30). Steps
are numbered `M<n>.<k>`
and are in dependency order within a milestone.

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
- Split: the first CR (PR #4) bound render.h, screen.h, selection.h and formatter.h whole, and
  generated the by-value shims. The second CR bound the encoders, both parsers, unicode, and the
  rest of the partly bound headers and output types; `coverage.txt` defers nothing to M1.3 now.
  **Done.**

### M1.4 `Ghostty` effect and scoped terminals

- `Ghostty.Effectful`: the `Ghostty` effect, `runGhostty` with the version check returning
  `Either VersionMismatch` (§4.7), the owner-thread check on every operation (§4.6).
- `GhosttyError`; `Invariant` thrown for results the header says cannot happen.
- `Ghostty.Effectful.Terminal`: `withTerminal` (rank-2 region, `bracket` release), `resize`, `reset`.
- Tests: handle released on exception; use from a forked thread throws; version mismatch refuses.
- `Ghostty` is statically dispatched, with one interpreter (DESIGN.md §4.2). Later steps add their
  operations as plain functions in their own modules, each starting with `owned`. The pin reaches
  the version check as the generated `ghosttyPinnedCommit`. **Done.**

### M1.5 Typed keys

- `TerminalData a` / `TerminalOption a` GADTs, one constructor per header row, `Maybe` where
  `NO_VALUE` is possible (§4.4); `getT`/`setT`.
- Tracked grid refs in the high-level API (blocks need them in M3).
- Test enumerates `[Some TerminalData]` (kept exhaustive by an incomplete-pattern match) and calls
  each on a fresh terminal. **Half of M1's exit condition.**
- Every header row has a constructor except the callback options and `USERDATA` (M1.6),
  `SELECTION` (M6.5) and `KITTY_GRAPHICS` (M6.15), and a test holds that list against the header.
  `ModeKey r` is indexed by whether the library accepts a reset default for the mode. Tracked refs
  are owned by the terminal's region, with an idempotent `untrack`. **Done.**

### M1.6 Events and `feed`

- C callbacks installed by the binding that only append to the terminal's `IORef` buffer (§4.5).
  The callback options and `USERDATA` stay out of `TerminalOption` (M1.5's key-coverage test lists
  them); the query answers become `TerminalOption` constructors here.
- `feed` over `vt_write_until … BOUNDARY_OSC_END`, attaching cursor position to each
  `SemanticPrompt`.
- `TerminalConfig` answers queries (colour scheme, DA, ENQ, XTVERSION, size); updatable via `setT`.
- Tests: event order across mixed OSC 133 / bell / title streams; query replies arrive as `WritePty`.
- Every callback is installed but `CLIPBOARD_READ` (reads answered empty). Whether clipboard writes
  are accepted is a seventh answer (`SetClipboardWrite`), since the header needs a reply before the
  callback returns. `resize` and `reset` return the events they cause. ENQ and XTVERSION answer in
  C. Capturing a render hold's frame is left to M1.7 (DESIGN.md §4.5). **Done.** (Since D20 and D21
  `feed` is one `vt_write` with no stops, and each callback reads what it needs inside itself.)

### M1.7 Render frames

- `Ghostty.Effectful.Render`: render state handle, `withFrame` (frame tag `f`), `dirtyRows`,
  `cells` copying into `Cell`/`Style`/`Rgb`/`Hyperlink`, cursor and palette.
- Tests: only dirty rows reported after a partial update; copied cells survive the next update.
- `withRenderState` is its own rank-2 region inside the terminal's. Frame and row data are typed
  keys (`FrameData`, `RowData`) held against the header, as M1.5's are. A frame that returns
  consumes its dirty state. A synchronized-output hold is captured by a C `RENDER_HOLD` callback,
  answering §4.5's question; hyperlink URIs are read from the terminal only while it is the frame's
  moment. Overscan is never requested (DESIGN.md §4.3). **Done.**

### M1.8 Formatter and goldens

- `Ghostty.Effectful.Format`: plain / VT / HTML of a terminal, a selection, a row range.
- tasty-golden goldens over canned VT streams (colours, wide chars, wraps, hyperlinks).
  **The other half of M1's exit condition.**
- A formatter is not a handle: `format` (the active screen, scrollback included) and `formatRange`
  (`Span`, `Block`, `WholeRows` in any coordinate system) each make, use and free one, so a range's
  untracked grid references cannot go stale (DESIGN.md §4.2). "A selection" is a span or block of
  caller's points; the terminal's own selection waits for M6.5. Goldens show control bytes visibly.
  Found on the way: a `History` point past the scrollback resolved to an active row, now refused by
  `track`, `moveRef` and `formatRange` (regression 6, DESIGN.md §4.4). **Done.**

### M1.9 Encoders, parsers, unicode

- `Ghostty.Effectful.Encode` (configured from a terminal's modes), `.Parse`, `.Unicode`.
- `paste` returns `Either Rejected` (§4.7).
- Unit tests per encoder; these are M2's oracles.
- Key and mouse encoders are scoped handles, each owning the event handle it fills in, configured
  by an options record or from a terminal; their inputs are plain values in
  `Ghostty.Effectful.Input` (`Key` held against the header, `Mods` with a side only for a held
  modifier). Reports, `encodePaste` and the parsers make and free what they use in the call
  (DESIGN.md §4.2). `paste` reads through a C MIME reader and returns its bytes as a `WritePty`
  event. Found on the way: the Kitty encoder finds a text key by its unshifted character, so M2.8's
  generator must set it (DESIGN.md §11.2). **Done.**

### M1.10 First benchmark and CI step 8

- `tasty-bench` `feed` throughput benchmark in `ghostty-effectful`.
- Committed allocation baseline; CI step 8 gates allocation, reports time (§12.3, §2.8).
- Five 1 MiB streams (plain, SGR, wide and combining text, an OSC 133 transcript, plain in 4 KiB
  writes) and an empty terminal's life, each iteration a whole `runGhostty`-open-feed-free.
  `scripts/check-allocation.py` is cassini's, with a 4 KiB slack: the streams that allocate nothing
  per byte move by up to 1.3 KB between runs. The OSC 133 transcript costs about 3.2 µs and 2.9 KB
  per stop, recorded in DESIGN.md §12.3. **Done.**

### M1.11 libghostty-vt 1.0.0

- Move the pin from commit `c7f68a7f` to libghostty-vt's first stable release, `1.0.0`
  (`ghostty-vt-ffi/LIBGHOSTTY_VT_VERSION`), built offline from the minisign-verified release
  tarball and its deps tarball (DESIGN.md §3.1, D14); `prefetch-zig-deps.py` and the git build go.
  `--release` builds for the baseline CPU (D17).
- Regenerate the binding against 1.0's ABI freeze: `INVALID = 0` in every key enum, setters that
  return results, one `uint8_t` buffer shape, frozen `GridRef`/`Selection`/`Style` and callback
  payloads, keyed formatter options, the merged `GhosttyCursorStyle` and `GhosttyImageLayer`, the
  renamed modes. Six shims go, two come (13 bound). Every bound function's doc was re-read for
  `safe`/`unsafe`; no mode changed.
- The version check compares `ghostty_version()`, the headers' `GHOSTTY_VT_VERSION` and every
  optional feature with the pin, exactly (D15, D-deferred-11).
- The silent changes, each a regression case that failed first: the OSC title is a
  `GhosttyString` (regression 8), a cell's absent colour is `NO_VALUE` (9), a new formatter's
  defaults are not canopy's (10), and a null scrollback byte limit restores the 10,000-byte default
  (11). `ShortStruct` goes with the sized payloads; `ResultUnsupported` arrives. Public renames
  follow upstream (D16). Goldens unchanged; allocation within the gate. **Done; M1 complete.**

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
- `type role Offset nominal`, and a test that coercing one unit into another does not type-check
  (§10.1, D30). The same for every later type with a parameter that carries a guarantee.
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

### M2.11 Editor core

- `Tendril.Editor`: `EditorState`, `Plugins` with its defaults, `Request`, `Answer`, and
  `step :: Plugins -> EditorInput -> EditorState app -> (EditorState app, [Request app])` (§5.5,
  §5.8, D25). Pure: it comes before the widgets and the driver, which are built over it.
- Tests, with no thread: an answer for an old revision is dropped; after a `NotReady`, later input
  is held and replayed in order when `Validated` arrives.

### M2.12 T3 widgets

- `Tendril.Widget.Prompt`, `.Menu`, `.Hint`, `.Popup`, `.Search` (incremental history search), and
  the renderer from an `EditorState` to the live region's `Doc`.
- Screen goldens per widget.

### M2.13 Terminal I/O

- `Tendril.Term.Tty`: raw mode under `bracket`, restored on `SIGTSTP`/`SIGCONT` and fatal signals;
  size and SIGWINCH.
- `Tendril.Term.Caps`: probing with DA1 sentinel, kitty keyboard, 2026, 2027 (§5.7).

### M2.14 Driver, effects and history

- `Completer` and `History` effects with trivial interpreters; append-only file history (§5.5).
- `Tendril.Effect` (`LineEditor`), `Tendril.Run` (`runLineEditor`: `step` driven from the keyboard,
  answering its requests through the effects, the completer on its own thread), `Surface` with
  `live`/`commits` (§5.6).
- Test: a scripted completer that answers late.

### M2.15 `tendril-demo`

- A usable readline: Emacs keys, history file, word completion, ghost hints, commits from a timer to
  show the external printer.
- Manual check written up in the CR: used as a daily readline. **M2 exit.**

---

## M3 — canopy, generic

Exit: rlwrap parity and better — output while typing, progress bars, `help()` pager, Ctrl-C; §12.1's
budgets met, or each miss recorded (§13).

### M3.1 C0 vocabulary

- `Canopy.Optics` (`use`, `.=`, `%=`, `zoomMaybe` over static local state), `Canopy.Units`
  (`Pos space u`, `Space`).

### M3.2 PTY

- `cbits/` shim + `Canopy.Ffi.Pty` + `Canopy.Child.Pty`: `posix_openpt`, `posix_spawn` with
  `POSIX_SPAWN_SETSID`, winsize, `tcgetattr` from the master (§6.2).
- A side-channel pipe when the launch asks for one: its read end is the child's fd 3 (§6.2, §7.5).
- The spawn resets signal dispositions and the mask (`SETSIGDEF`, `SETSIGMASK`), sets the working
  directory with `addchdir_np`, and opens the master `O_CLOEXEC` (§6.2).
- Tests: child is session leader with a controlling tty; termios changes visible from the master;
  the child reads a frame from fd 3; the child has `SIGPIPE` at its default, an empty mask and the
  launch's working directory.

### M3.3 Child effect and fake REPL

- `Child` effect (`Send`, `SendUnechoed`, `Submit`); `runChildPty`, `runChildScripted` (§6.3). A
  reader and a writer thread for each child: no write on the owner's thread (D28).
- `canopy-fake-repl` test executable: echo REPL emitting OSC 133, reading its inputs from the
  terminal or, with a flag, from fd 3 (§11.4).
- Tests: real PTY path against the fake REPL; a submission larger than the PTY's buffer, sent
  while the child is writing, does not deadlock.

### M3.4 App core and shell

- `Canopy.App.Event`, `.Command`, `.Model`, `.Update` (pure), `ChildState` with its `InputFocus`
  (§6.4). The model holds no region-tagged value (D25): tracked refs stay in `Canopy.App.Interpret`.
- `Canopy.App.Run`: feeder threads on one bounded STM queue; the event loop written against a row
  with neither `Concurrent` nor `IOE` (§4.6). `Canopy.App.Interpret`, `Canopy.Cli`.
- hlint rule: no wildcard `_ ->` in `Canopy.App.Update` (§10.4), with fixtures. (§2.6's rules 2 and
  4 for the core and the view, and their fixtures, landed with D25.)
- Test: `update` on hand-written event sequences.

### M3.5 Profiles and OSC 133 framing

- `Canopy.Profile` type (§7.1) and `Canopy.Profile.Builtin.Generic`; `Detector`s, and choosing a
  profile from the command and the project in `Canopy.Cli`.
- A block's status, from `D` or a profile's recogniser, `Unknown` otherwise (§7.3, D32).
- `Canopy.Session.Frame`, `.Block`, `.Store`, `.Transcript`: the commit watermark, one tracked ref;
  a block's content captured as it is committed (§7.3, D23). `C` and `D` are taken from a child that
  sends them and derived otherwise; deriving `C` from the echo is finished in M3.8.
- Scripted-child tests: blocks delimited correctly across chunk boundaries; a finished block is
  unchanged by a later `clear`, a `reset` and the scrollback's pruning.

### M3.6 View

- `Canopy.App.View`: `view :: Model -> ChildFrame -> Surface`, ghostty cells → tendril `Doc` with
  palette resolution; the running block's active-area rows in the live region (§8.3).
- Capture after every feed, from the watermark, committed with the next frame; a PTY read fed in
  slices that cannot outrun the child's scrollback (§12.2).
- Frame coalescing (~8 ms) and `RenderHold` suppression (§12.2).
- Property: a block's `UNWRAP` capture, fed to a fresh terminal, gives back its cells; and a flood
  of newlines loses no row. Resolves §14.2's two capture checks.
- A throughput spike: 100 MB through the fake REPL, against §12.1's budget, reported in the CR. A
  large miss changes the design here, before the rest of M3 is built on it.
- Screen goldens: output while typing; a `\r` progress bar animates in place and is committed once.

### M3.7 Submission, queueing and input focus

- `SubmitStyle`: `Channel` (one frame on fd 3), `Lines`, `Bracketed` (auto when child set 2004, via
  the paste encoder), `Wrapped`, `BlankLineTerminated`, `Custom` (§7.5).
- Input queued in `Starting`/`Busy` as type-ahead, shown dimmed.
- `InputFocus` (§6.4, D26): the quiet timer and its termios read; `ToProgram` on a cursor left
  mid-row, `Masked` when the child cleared `ECHO`; the focus in the prompt and its toggle key;
  masked input kept out of history.
- Scripted-child tests: a program that reads a line in the middle of a block; a password prompt.

### M3.8 Echo suppression

- `NoEcho` for `Channel`; `CookedChild` through `SendUnechoed` (clear `ECHO`, set `ECHONL`, write,
  restore); `ChildEditorEchoes` (bytes sent + end-of-input sequence) (§7.4); runtime fallback with
  a one-time notice.
- Resolves §14.2's open check on changing the echo from the master around the write.

### M3.9 Setup, python and ghci profiles

- Typed `SetupStep`s run in `Starting`, hidden startup block shown on failure; refused inputs
  (`:set prompt`) (§7.2).
- `Canopy.Profile.Builtin.Python`: a bootstrap that reads its submissions from fd 3, sends `C` and
  `D;<status>` itself, and discards unread terminal input at each prompt. Resolves §14.2's check on
  it.
- `Canopy.Profile.Builtin.Ghci`.

### M3.10 Regex and sentinel framing

- `PromptRegex` matched on child-terminal rows; `Sentinel` (§7.3).
- **Answer D-deferred-10**, so that canopy has one regex engine: compile every `#match?` pattern in
  Helix's and nvim-treesitter's queries for the built-in languages with each candidate (the patterns
  need no grammar), and record the choice in DESIGN.md §15. M4.9 confirms it.
- Exact-block features unavailable for `PromptRegex` profiles by type (§14.4).

### M3.11 Interrupts, exit, resize

- Ctrl-C → `VINTR` when busy / clear input when editing; Ctrl-D → `VEOF` on empty input (§6.2).
- `Exited` with restart offered, transcript kept. Host resize propagated to child and child terminal.

### M3.12 Passthrough

- `Paging Alternate`: host alternate screen, child render state mirrored, host input decoded by
  tendril and re-encoded by libghostty from the child's modes; escape chord (§8.6).
- The host's modes follow the child's while it pages (mouse tracking, focus, bracketed paste, kitty
  keyboard flags, cursor style and visibility), and are restored on leaving.
- `Paging Primary` (D31): raw mode on the primary screen passes input through and keeps the inline
  display; it starts only after the quiet timer. Test: haskeline going raw before its prompt does
  not start it.
- Paste into a full-screen child through the safety check; `Rejected` asks (§8.5).

### M3.13 Transcript view

- The store's blocks replayed into a second libghostty terminal and drawn on the host's alternate
  screen, over M3.12's machinery (§8.4, D24): scrolling, and a return to the inline prompt that
  leaves the host's scrollback as it was. Output that arrives meanwhile is captured, and committed
  on return.
- Screen goldens: a session's blocks in the view are what was committed, at the width they ran at
  and at a narrower one.

### M3.14 User profiles in TOML

- `Canopy.Config.Codec`: `toml-parser` codec to `Profile`, applicative validation reporting every
  error with locations; `extends` via `Profile Partial` (§7.1, §10.5).

### M3.15 Benchmarks and e2e harness

- Keystroke-to-frame and child-throughput benchmarks against §12.1's budgets: each met, or its miss
  recorded there with its cause.
- `canopy-e2e` with the host side attached to a headless libghostty terminal (§11.5); nightly
  workflow with pinned ghci and python. Scenarios: output while typing, progress bar, `help()` pager,
  Ctrl-C. **M3 exit.**
- Resolves §14.2's musl spawn check, if a musl job is added here; otherwise it stays open.

---

## M4 — tree-sitter

Exit: §5B.7 passes; built-in profiles' grammars bind and their queries compile under `Strict`;
highlighting, completeness and indentation goldens for the built-in profiles (§13).

### M4.1 `tree-sitter-ffi` complete

- `TreeSitter.FFI.Api` (every scalar/pointer function, `safe`/`unsafe` per §5A.3 on each import),
  `TreeSitter.FFI.Types` (all structs and enums), shims complete.
- Vendored `tree-sitter-json` at a pinned tag in the test suite (§5B.7).
- Tests: struct sizes vs `sizeof`; each shim against the direct C call (a C test helper calls both).
- A signature check like `check-ghostty-signatures.py` (§3.3, §5A.2): `capi` does not catch integer
  widths or pointers to Haskell types.

### M4.2 Budget, input reader, dynamic language

- `tsffi_budget_progress` / `TsffiBudget`, `tsffi_buffer_read`, `TreeSitter.FFI.Budget`,
  `TreeSitter.FFI.Language` (`capi "dynamic"`) (§5A.3, §5A.4).
- Tests: deadline and cancel word stop a parse.

### M4.3 Trees, nodes, pure reads

- `Tree g`, `Node g` (ForeignPtr-kept), `TreeSitter.Internal.Pure` with its purity argument in the
  header (§5B.2); every node read and `toSExpr`; `nodeText`.
- Reads that hold C memory use `unsafePerformIO` under `bracket`, the others
  `unsafeDupablePerformIO` (D29). Nominal roles on `Language`, `Tree` and `Node` (D30).
- Unsafety-audit `grep` updated (§10.6).

### M4.4 Parse effect

- `Parse` effect, `runParse` with owner-thread check, `runParseIsolated`, `Budget`/`CancelToken`,
  `Aborted` (§5B.3).

### M4.5 Edits and incremental parsing

- `EditedTree` carrying its edited source, `edit` (copy-then-edit) over `splice`s, `parse` from a
  `Source`, `changedRanges` (§5B.2, §5B.3, D29).
- Properties: incremental equals fresh; persistence; `edit`'s points agree with naive ones, and its
  text is the old one with the splices applied (§5B.7).

### M4.6 Concurrency stress test

- Reads of one `Tree` from every capability under `-threaded +RTS -N` while another thread edits
  copies, checked against a single-threaded baseline, with every cursor made freed (§5B.7). Backs
  §14.2's purity row.

### M4.7 Grammars

- `loadGrammar` (dlopen, ABI range, `LoadError`, a fresh `'Dynamic d` for each load, D29),
  `deriveGrammar` TH with `addDependentFile`,
  `bindGrammar`, `kindOf`, `fieldOf` (§5B.4).
- Tests on json; `bindGrammar` fails with the right names on a modified `node-types.json`.

### M4.8 Queries

- `Query g c`, `Capture`, `compileQuery`, `QueryError` with line/column; pure `matches`/`captures`
  that take no deadline and return `Found`, with `matchesWithin`/`capturesWithin` in `Parse` for a
  budget (§5B.5, D29).

### M4.9 Predicates

- All listed predicates and directives, decoded at compile time; `PredicatePolicy`.
- **Confirm D-deferred-10's answer** (M3.10) by running Helix's and nvim-treesitter's queries for
  the built-in languages and counting mismatches; record in DESIGN.md §15.
- That needs the built-in grammars, so this step adds `canopy/grammars/pins.toml`,
  `scripts/build-grammars.sh` and CI's cached grammar build (§2.8 step 1). They are data and a
  script, so the binding half still needs nothing of canopy's code.
- Nightly cross-check against the `tree-sitter query` CLI where installed.

### M4.10 Highlighter and benchmark

- `TreeSitter.Highlight` with injections (cached per site) and locals (§5B.6).
- Benchmark: incremental reparse + highlight of a one-character edit (§12.3).

### M4.11 Grammar pins and `canopy grammar`

- Over M4.9's `pins.toml`: committed `node-types.json` per built-in grammar,
  `Canopy.Syntax.Grammars` splices; `canopy grammar fetch|build` (§9.1), which also installs the
  grammar repository's own queries (D27). `Canopy.Child.Process` for the build (§6.3).

### M4.12 Grammar store

- `Canopy.Syntax.GrammarStore`: Helix layout, `; inherits:`, compile every query at startup with
  line/column errors.
- Built-in queries: each grammar repository's own highlights, injections and locals, and canopy's
  indents and text objects (`canopy/queries/<lang>/`, D27).
- Test: every built-in profile's grammar binds and its queries compile under `Strict`.
- Resolves §14.2's Helix-query-collection check.

### M4.13 Edit adapter and the tree in the model

- `Canopy.Syntax.Edit` (tendril edits → `splice`s); property across the adapter (§11.3).
- Current input's `Tree` in `Model`, parse as a `Command` with a few-ms budget, idle retry on `Aborted`.

### M4.14 Highlighting

- `Canopy.Syntax.Highlight`: closed `HighlightCapture`, total theme table incl. Helix capture names,
  over `changedRanges`; plugged in as `Plugins.highlight` (§5.5).
- Themes: a TOML table from capture names to styles, decoded at startup, with a built-in default
  (§9.1).

### M4.15 Completeness

- `Canopy.Syntax.Complete` as `Plugins.validate`, `NotReady` while the tree is behind the buffer
  (§5.5, §9.1): ERROR/MISSING at end, unclosed brackets; profile rules
  (`:{`, trailing `\`).

### M4.16 Indent and structure

- `Canopy.Syntax.Indent` (`indents.scm`, closed `IndentCapture`), `Canopy.Syntax.Structure` (expand/
  shrink selection, bracket matching, auto-pairs), plugged in as `Plugins.enclosing`.
- Goldens for the built-in profiles: highlighting, completeness and indentation. **M4 exit.**

---

## M5 — LSP

Exit: scripted-LSP session tests; nightly e2e with pyright/basedpyright and HLS.

### M5.1 Transport

- First, a probe (§14.2): does HLS type-check a file that is in no component against the project's
  packages? Its answer may change §9.2 before M5.3 and M5.6 are built on it.
- `Canopy.Lsp.Transport`: `Content-Length` JSON-RPC over stdio, as an effect with a scripted
  interpreter (expected requests, canned responses, fail on unexpected) (§11.4).

### M5.2 Client and capabilities

- `Canopy.Lsp.Client`: `SMethod`-indexed requests that return a `RequestId`, their responses
  arriving as events (§10.3, D25); initialize/shutdown, `$/cancelRequest` on superseded revisions. `Canopy.Lsp.Capabilities`: `Supports m` tokens, for one
  server session, with no dynamic registration declared (§10.3).
- `Canopy.Lsp.Rows` if any `row-types` field is touched.

### M5.3 Source maps and projections

- `SourceMap from to` (§10.2), `Shadow`, `Projection`; Generic and Python projections (python's of
  the inputs that compiled).
- Properties: source maps round-trip where they map. Goldens for python shadow documents.

### M5.4 Shadow document lifecycle

- Server spawn per profile (`Canopy.Child.Process`), `didOpen`/`didChange`, on-disk `.canopy/session-<id>.<ext>` and generated
  `.gitignore` when the profile requires it; server crash → disabled with notice, restart offered.

### M5.5 REPL introspection

- `Introspection` hidden queries when `Ready`, purity declared, cache per (query, transcript length)
  (§9.3); ghci and python.
- `Querying` (§6.4, D32): a submission made while a query is out waits for it. Test: python's `_`
  is unchanged by a query.

### M5.6 ghci projection

- Tree-sitter classification (imports hoisted, declarations renamed on shadowing, `canopy_it_N`,
  `x <- e` with type from `:type`); per-construct goldens (§9.2, §14.5).

### M5.7 Diagnostics

- Diagnostics mapped through the source map, earlier blocks' dropped, shown under the input; session
  toggle (§14.5).

### M5.8 Completion merge

- `Canopy.Intel` merge of LSP and REPL candidates and ranking (§9.4); answering tendril's completion
  request (§5.5), with kinds/types and docs side panel.

### M5.9 Hover, type bar, signature help

- LSP `hover` with REPL fallback; live type of expression under cursor; `signatureHelp` (§8.2).

### M5.10 Semantic tokens

- `semanticTokens` overlay on tree-sitter spans when revisions match (§9.1).

### M5.11 Session tests and nightly e2e

- Whole scripted sessions (late completion with an old revision dropped, etc.); nightly e2e with
  basedpyright and HLS at pinned versions. **M5 exit.**

---

## M6 — depth

Exit: feature table §8 complete. Steps are mostly independent of each other. M6.13 and M6.14 are
lists: each feature in them is a CR of its own.

### M6.1 Vi keymap

- `Tendril.Keymap.Vi`, `ViNormal`/`ViInsert` commands, cursor shape per mode; tree-sitter text
  objects (`textobjects.scm`) from canopy.

### M6.2 Helix keymap and user keybindings

- `Tendril.Keymap.Helix`; user keybinding files decoded at startup with all mode errors at once (§5.4).

### M6.3 History database

- SQLite history, answering tendril's history requests (§5.5), with context per entry; prefix up-arrow per profile; Ctrl-R fuzzy
  popup ranked by recency/project/success (§8.8).

### M6.4 History browser

- Prompt-less list, multi-select and insert (§8.8).

### M6.5 Blocks

- The terminal's `SELECTION` data key and option, as `TerminalData`/`TerminalOption` constructors
  over the selection API's value (left out of M1.5).
- In the transcript view (M3.13): Alt-↑/↓ selection, copy input/output/both, re-insert and re-run,
  expanding a fold. Inline: output committed folded with "N more lines", timing and exit/exception
  markers (§8.3, §8.4).
- Result references (`_3`) where the profile's introspection provides them (§8.4).

### M6.6 Search

- `Ghostty.Effectful.Search` (FFI + effectful) and transcript search including folded output, over
  the transcript view's terminal (M3.13, §8.4).
- At libghostty-vt 1.0 the search's selection moves with `ghostty_search_select_next`/`_prev` (the
  `OPT_SELECT_NEXT`/`PREV` options are gone), and its data and option keys start at `INVALID = 0`.

### M6.7 Export

- Script / plain / HTML export from the store, through the formatter (§8.4).

### M6.8 Rewind and checkpoints

- Profile purity marks; rewind *k* blocks by restart + replay, asking before impure ones; named
  checkpoints (§8.4, §8.7).

### M6.9 Sessions

- The block store as a journal on disk; `--continue` commits its blocks again and replays their
  inputs (§8.7, D23).

### M6.10 Snapshots (optional)

- Optional since D23: the journal restores a session's display (M6.9), and §8's audit (M6.17) does
  not need this step. Build it only if the child terminal's exact state proves to be wanted.
- `Ghostty.Effectful.Snapshot` (FFI + effectful); exact screen restore without re-running.
- The snapshot byte format is outside libghostty-vt's 1.x ABI promise, so a stored snapshot must be
  allowed to fail to decode after an upgrade: the session falls back to replay (DESIGN.md §8.7).
  `snapshot.h`'s two by-value functions get shims then.

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

- The `KITTY_GRAPHICS` data key (a borrowed handle, left out of M1.5).
- Kitty graphics placements (render-state FFI, `kitty_graphics.h`, and `sys.h`'s PNG decoder hook;
  passthrough when the host supports it), OSC 8
  hyperlinks end to end, opt-in output-as-code highlighting per profile (§8.3).
- At libghostty-vt 1.0: the placement iterator's `LAYER` option is a bitmask of the
  `GHOSTTY_IMAGE_LAYER_MASK_*` bits (`Ghostty.FFI.Types`, 0 for no filter); the image data keys are
  `GHOSTTY_KITTY_GRAPHICS_IMAGE_DATA_*`, with `PIXELS`/`EXPECTED_LEN` for `DATA_PTR`/`DATA_LEN`;
  `sys.h`'s options start at `INVALID = 0`; the oldest image is evicted by transmission.
- Consider binding `GRAPHEMES_BUF` (into the already bound `GhosttyCodepointBuffer`) if M2's width
  oracle wants codepoints without a UTF-8 round trip.

### M6.16 Node, Julia, utop profiles

- `Canopy.Profile.Builtin.Node`, `.Julia`, `.Utop`; resolves §14.2's node and julia setup checks.
- Node and julia join the nightly e2e (§11.1).

### M6.17 §8 audit

- Walk every row of §8's tables, link each to its test, fix gaps. **M6 exit.**

---

## M7 — tooling

Exit: a client library test drives a session; the release job's static binary passes the version
check.

### M7.1 Serve mode

- `Canopy.Serve`: JSON-RPC on a socket — submit inputs, read structured blocks, subscribe to events
  (§8.7).

### M7.2 Client library and test

- A small client library and a test that drives a full session through it against the fake REPL.

### M7.3 Release build

- A release workflow: `build-libghostty.sh --release` (baseline CPU, D17), the `static` flag on
  (D11), and a `canopy` binary with no runtime library dependency, which runs the version check
  (§3.4) and the fake-REPL session before it is published. **M7 exit.**
