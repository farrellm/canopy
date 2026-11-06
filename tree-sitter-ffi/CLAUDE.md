# tree-sitter-ffi

Raw bindings over a **vendored** tree-sitter runtime (`DESIGN.md` §5A). Only `TreeSitter.**` and
`Test.TreeSitter.**` may import it (§2.6 rule 5).

| Path | What it is |
| :--- | :--- |
| `TREE_SITTER_VERSION` | The pinned tag. `scripts/vendor-tree-sitter.sh <tag>` replaces `vendor/`. |
| `vendor/tree-sitter/` | `lib/src`, `lib/include` and `LICENSE` at that tag. Never edit it by hand. |
| `cbits/tsffi_shims.c`, `include/tsffi_shims.h` | **Generated** by `scripts/gen-ts-shims.py`. Never edit them. CI runs `--check`. |
| `src/TreeSitter/FFI/` | `Types` (`hsc2hs`), `Api` (scalar and pointer functions), `Shims` (by-value functions). |

- Every import is `capi`, against `tree_sitter/api.h` or `tsffi_shims.h`, so the C compiler checks
  each signature.
- `safe` is only for calls that can run long (parse, query exec and next, query compile). Everything
  else is `unsafe`, and no tree-sitter call re-enters Haskell (§5A.3).
- Bumping the tag means running the vendor script and the shim generator, then fixing what moved,
  all in one commit that says why. The ABI-range test (`13..15`) moves with it.
