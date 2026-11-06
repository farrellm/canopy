# tree-sitter-effectful

The high-level tree-sitter interface (`DESIGN.md` §5B): persistent trees with pure node reads, typed
node kinds and captures, and a confined `Parse` effect. `TreeSitter.Internal.Pure` will be the only
module with an unsafe primitive (§10.6). M0 is a skeleton (`supportedAbi`). The interface arrives in
M4.
