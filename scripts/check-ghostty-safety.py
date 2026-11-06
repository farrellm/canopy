#!/usr/bin/env python3
"""Keeps each `safe`/`unsafe` decision in ghostty-vt-ffi tied to the header
text it was made from (DESIGN.md §3.3).

Whether a call is `safe` is decided by reading the header: an `unsafe` call
that runs a Haskell callback crashes, and one that runs long holds up every
other Haskell thread at the next GC. No compiler checks that. What can be
checked is that the decision was made against the header as it is now. So
ghostty-vt-ffi/safety.txt records, for every C function a `foreign import
capi` binds, the import's mode and a fingerprint of the function's doc comment
and prototype in the pinned headers:

    <function>  <safe|unsafe>  <fingerprint>

A shim (`gvtffi_x`) is recorded as the function it wraps (`ghostty_x`).

This fails when a bound function has no line, when a line names nothing
bound, when a line's mode is not its import's, and when the header text no
longer matches the fingerprint, as after a pin bump that changed what the
function documents. Then re-read the function's documentation in the header,
check the import's mode and its comment against it, and record the result
with `--update`, which rewrites the file from the imports and the headers. The
line's diff is what the reviewer reads.

Run from the repository root. The headers are read from
`_deps/ghostty/<GHOSTTY_COMMIT>/include` (scripts/build-libghostty.sh), or
from `--include`.
"""

import argparse
import hashlib
import re
import sys
from pathlib import Path

PKG = Path("ghostty-vt-ffi")
LIST = PKG / "safety.txt"
HEADER = """\
# The safe/unsafe decision for every C function ghostty-vt-ffi binds, and a
# fingerprint of the header text it was decided from (DESIGN.md §3.3).
# scripts/check-ghostty-safety.py fails when the header text changes, as at a
# pin bump: re-read the function's documentation, check the import's mode and
# comment, then run it with --update. Do not edit by hand.
#
# <function>  <safe|unsafe>  <fingerprint of its doc comment and prototype>
"""


def imports():
    """C function -> (mode, where), from every `foreign import capi`."""
    out, failures = {}, []
    for p in sorted((PKG / "src").rglob("*.hs*")):
        for m in re.finditer(r'^foreign import capi (safe|unsafe) "\S+ (\w+)"', p.read_text(), re.M):
            fn = re.sub(r"^gvtffi_", "ghostty_", m.group(2))
            if fn in out and out[fn][0] != m.group(1):
                failures.append(f"{p}: {fn} is imported both safe and unsafe")
            out[fn] = (m.group(1), p)
    return out, failures


def fingerprints(root: Path):
    """C function -> fingerprint of its doc comment and prototype."""
    out = {}
    for p in sorted(root.rglob("*.h")):
        text = p.read_text()
        for m in re.finditer(r"(?:GHOSTTY_API|static inline)\s[^;{}]*?\b(ghostty_\w+)\s*\(", text):
            proto_end = min(i for i in (text.find(";", m.start()), text.find("{", m.start())) if i >= 0)
            doc = re.search(r"(/\*\*(?:(?!\*/).)*\*/)\s*$", text[: m.start()], re.S)
            body = (doc.group(1) if doc else "") + text[m.start() : proto_end]
            out[m.group(1)] = hashlib.sha256(" ".join(body.split()).encode()).hexdigest()[:16]
    return out


def read_list():
    entries, failures = {}, []
    if not LIST.exists():
        return entries, [f"{LIST} does not exist; run with --update"]
    for n, line in enumerate(LIST.read_text().splitlines(), 1):
        line = line.split("#", 1)[0].strip()
        if not line:
            continue
        parts = line.split()
        if len(parts) != 3 or parts[1] not in ("safe", "unsafe"):
            failures.append(f"{LIST}:{n}: want '<function> <safe|unsafe> <fingerprint>'")
            continue
        entries[parts[0]] = (parts[1], parts[2])
    return entries, failures


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--update", action="store_true", help=f"rewrite {LIST} after re-reading")
    parser.add_argument("--include", type=Path, help="the libghostty-vt include directory")
    args = parser.parse_args()
    commit = (PKG / "GHOSTTY_COMMIT").read_text().strip()
    root = (args.include or Path("_deps/ghostty") / commit / "include") / "ghostty" / "vt"
    if not root.is_dir():
        sys.exit(f"check-ghostty-safety: no headers at {root}; run scripts/build-libghostty.sh")

    bound, failures = imports()
    prints = fingerprints(root)
    for fn in sorted(set(bound) - set(prints)):
        failures.append(f"{bound[fn][1]}: no header declares {fn}")

    if args.update:
        if failures:
            for f in failures:
                print(f"FAIL    {f}")
            return 1
        width = max(len(fn) for fn in bound)
        lines = [f"{fn:<{width}}  {bound[fn][0]:<6}  {prints[fn]}" for fn in sorted(bound)]
        LIST.write_text(HEADER + "\n" + "\n".join(lines) + "\n")
        print(f"wrote   {LIST}: {len(lines)} functions")
        return 0

    entries, list_failures = read_list()
    failures += list_failures
    for fn, (mode, where) in sorted(bound.items()):
        if fn not in entries:
            failures.append(f"{where}: {fn} is bound but not in {LIST}: decide safe or unsafe from its header docs")
            continue
        recorded_mode, recorded_print = entries[fn]
        if recorded_mode != mode:
            failures.append(f"{where}: {fn} is imported {mode}, but {LIST} records {recorded_mode}")
        if fn in prints and recorded_print != prints[fn]:
            failures.append(
                f"{where}: {fn}'s header doc or prototype changed since it was decided {recorded_mode}:"
                " re-read it, check the import, then --update"
            )
    for fn in sorted(set(entries) - set(bound)):
        failures.append(f"{LIST}: {fn} is not bound; run with --update")

    for f in failures:
        print(f"FAIL    {f}")
    if not failures:
        safe = sum(1 for m, _ in bound.values() if m == "safe")
        print(f"ok      {len(bound)} bound functions ({safe} safe) were decided against the current headers")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
