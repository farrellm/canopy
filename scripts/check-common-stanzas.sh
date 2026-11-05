#!/usr/bin/env bash
# Checks that every package's .cabal file carries the same `common warnings` and
# `common extensions` stanzas (DESIGN.md §2.4). Cabal cannot import a stanza
# from another file, and ormolu reads default-extensions from each package's
# own .cabal, so the stanzas are copied; this keeps the copies equal. Run from
# the repository root.
set -euo pipefail

# A stanza runs from its `common` line to the next blank line.
stanzas() {
  awk '/^common (warnings|extensions)$/ { on = 1 } on { print } /^$/ { on = 0 }' "$1"
}

files=(*/*.cabal)
reference=${files[0]}
want=$(stanzas "$reference")
if [[ -z $want ]]; then
  echo "FAIL    no common stanzas in $reference" >&2
  exit 1
fi
status=0
for f in "${files[@]:1}"; do
  got=$(stanzas "$f")
  if [[ $got == "$want" ]]; then
    echo "ok      $f matches $reference"
  else
    echo "FAIL    $f differs from $reference:"
    diff <(echo "$want") <(echo "$got") || true
    status=1
  fi
done
exit $status
