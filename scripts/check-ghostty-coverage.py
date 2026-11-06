#!/usr/bin/env python3
"""Checks that ghostty-vt-ffi covers the pinned libghostty-vt headers
(DESIGN.md §3.3): every function and every type the headers declare is either
bound or listed in ghostty-vt-ffi/coverage.txt with the PLAN.md step that
binds it. So a pin bump that adds a function or a type to a header the binding
covers fails here, naming it.

- A function is bound when a `foreign import capi` names it, or names its
  pointer shim (`gvtffi_x` for `ghostty_x`, include/gvtffi_shims.h).
- A type is bound when the binding declares a Haskell type of the same name
  (data, newtype or type). A tagged union's value union, `TValue`, is bound
  when its struct `T` is: the binding reads it as a sum (§3.3).

coverage.txt lists what is not bound yet, one entry per line:

    <header>  <step>  [why]   everything in that header not bound yet
    <symbol>  <step>  [why]   one function or type

where <step> is a PLAN.md step (M1.3) or `never`, and `#` starts a comment. An
entry that defers nothing, because what it names is bound or does not exist, is
a failure too, so the list shrinks as the binding grows.

Run from the repository root. The headers are read from
`_deps/ghostty/<GHOSTTY_COMMIT>/include` (scripts/build-libghostty.sh), or
from `--include`.
"""

import argparse
import re
import sys
from pathlib import Path

PKG = Path("ghostty-vt-ffi")
LIST = PKG / "coverage.txt"
STEP = re.compile(r"^(M\d+\.\d+|never)$")


def header_symbols(root: Path):
    """header -> (functions, types) it declares."""
    out = {}
    for p in sorted(root.rglob("*.h")):
        s = re.sub(r"/\*.*?\*/", "", p.read_text(), flags=re.S)
        fns = set(re.findall(r"GHOSTTY_API\s+[^;(]*?\b(ghostty_\w+)\s*\(", s))
        fns |= set(re.findall(r"static inline [^(;]*?\b(ghostty_\w+)\s*\(", s))
        types = set(re.findall(r"\}\s*(Ghostty\w+)\s*;", s))
        types |= set(re.findall(r"typedef [^;{(]*?\b(Ghostty\w+)\s*;", s))
        types |= set(re.findall(r"\(\s*\*\s*(Ghostty\w+)\s*\)", s))
        types |= set(re.findall(r"^struct (Ghostty\w+)\s*\{", s, re.M))
        out[str(p.relative_to(root))] = (fns, types)
    return out


def bound_symbols():
    fns, types = set(), set()
    for p in sorted((PKG / "src").rglob("*.hs*")):
        s = p.read_text()
        for name in re.findall(r'foreign import capi \w+ "\S+ (\w+)"', s):
            fns.add(re.sub(r"^gvtffi_", "ghostty_", name))
        types |= set(re.findall(r"^(?:data|newtype|type) (Ghostty\w+)", s, re.M))
    return fns, types


def read_list():
    entries, failures = {}, []
    for n, line in enumerate(LIST.read_text().splitlines(), 1):
        line = line.split("#", 1)[0].strip()
        if not line:
            continue
        parts = line.split(maxsplit=2)
        if len(parts) < 2 or not STEP.match(parts[1]):
            failures.append(f"{LIST}:{n}: want '<header or symbol> <step> [why]', step M<n>.<k> or never")
            continue
        if parts[0] in entries:
            failures.append(f"{LIST}:{n}: {parts[0]} is listed twice")
        entries[parts[0]] = (n, parts[1])
    return entries, failures


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--include", type=Path, help="the libghostty-vt include directory")
    args = parser.parse_args()
    commit = (PKG / "GHOSTTY_COMMIT").read_text().strip()
    root = (args.include or Path("_deps/ghostty") / commit / "include") / "ghostty" / "vt"
    if not root.is_dir():
        sys.exit(f"check-ghostty-coverage: no headers at {root}; run scripts/build-libghostty.sh")

    headers = header_symbols(root)
    bound_fns, bound_types = bound_symbols()
    entries, failures = read_list()

    def is_bound(sym):
        if sym.startswith("ghostty_"):
            return sym in bound_fns
        return sym in bound_types or (sym.endswith("Value") and sym.removesuffix("Value") in bound_types)

    used = set()
    counts = {"bound": 0, "deferred": 0}
    for header, (fns, types) in headers.items():
        for sym in sorted(fns | types):
            if is_bound(sym):
                counts["bound"] += 1
            elif sym in entries:
                used.add(sym)
                counts["deferred"] += 1
            elif header in entries:
                used.add(header)
                counts["deferred"] += 1
            else:
                kind = "function" if sym.startswith("ghostty_") else "type"
                failures.append(f"{header}: {kind} {sym} is neither bound nor listed in {LIST}")

    for name, (n, _) in sorted(entries.items(), key=lambda e: e[1][0]):
        if name in used:
            continue
        if name in headers:
            failures.append(f"{LIST}:{n}: everything in {name} is bound now; remove the line")
        elif is_bound(name):
            failures.append(f"{LIST}:{n}: {name} is bound now; remove the line")
        else:
            failures.append(f"{LIST}:{n}: no header declares {name}")

    for f in failures:
        print(f"FAIL    {f}")
    if not failures:
        print(f"ok      {counts['bound']} symbols bound, {counts['deferred']} deferred by {LIST}")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
