# canopy

A language-agnostic REPL frontend for the terminal, in Haskell. Canopy runs an existing REPL (ghci,
python, node, julia, utop, …) as a child process behind a pseudo-terminal and puts a modern editing
and display layer over it: tree-sitter for syntax, LSP for semantics, the REPL itself for runtime
facts, and libghostty-vt as the terminal emulator for everything the child prints.

**Status: M0, infrastructure.** The seven packages exist and are wired, and nothing user-facing
exists yet. [`DESIGN.md`](./DESIGN.md) is the architecture, and §13 lists the milestones.

## Building

GHC 9.12.4 and cabal 3.16.1.0. libghostty-vt has no distribution packages yet, so it is built at
the pinned commit first:

```sh
eval "$(scripts/build-libghostty.sh)"   # fetches Zig 0.17.1 if no zig >=0.17 is on PATH
cabal build all
cabal test all
```

## Licence

BSD-3-Clause. The vendored tree-sitter runtime (`tree-sitter-ffi/vendor/tree-sitter/`) is MIT.
