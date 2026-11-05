# tendril

A readline-style line editor (`DESIGN.md` §5), plus `tendril-demo`. **The library does not depend on
ghostty or tree-sitter.** Only `tendril-test` depends on `ghostty-effectful`, which serves as the
oracle (§1.3, §11.2), and `.hlint.yaml` rule 2 enforces that by module name. M0 is a skeleton. The
editor arrives in M2.
