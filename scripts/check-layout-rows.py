#!/usr/bin/env python3
"""Checks that every type ghostty-vt-ffi marshals has a row in its layout test
(DESIGN.md §3.4), so a struct or enum cannot be bound without being checked
against the linked library.

- Every `instance Storable T` written in the binding (a struct) must export a
  descriptor `tLayout :: CStruct`, listed as `StructRow tLayout` in the test.
- Every `newtype T = T <C integer>` (an enum, §3.3) must export a descriptor
  `tEnum :: CEnum`, listed as `EnumRow tEnum` in the test.
- Every descriptor the binding defines must be listed in the test.

Run from the repository root. Exits non-zero, naming each gap, on failure.
"""

import re
import sys
from pathlib import Path

SRC = Path("ghostty-vt-ffi/src")
TEST = Path("ghostty-vt-ffi/test/Test/Ghostty/FFI/Layout.hs")

STRUCT = re.compile(r"^instance Storable (\w+) where", re.M)
ENUM = re.compile(r"^newtype (\w+) = \1 (?:C[A-Z]\w*|Word\d+|Int\d+)\b", re.M)
DESCRIPTOR = re.compile(r"^(\w+(?:Layout|Enum)) :: (?:CStruct|CEnum)$", re.M)


def lower_first(name: str) -> str:
    return name[:1].lower() + name[1:]


def main() -> int:
    sources = {p: p.read_text() for p in sorted(SRC.rglob("*.hs*"))}
    test = TEST.read_text()
    rows = set(re.findall(r"\b(?:StructRow|EnumRow) (\w+)", test))
    defined = {d for text in sources.values() for d in DESCRIPTOR.findall(text)}

    failures = []
    for path, text in sources.items():
        for kind, pattern, suffix in (
            ("struct", STRUCT, "Layout"),
            ("enum", ENUM, "Enum"),
        ):
            for t in pattern.findall(text):
                want = lower_first(t) + suffix
                if want not in defined:
                    failures.append(f"{path}: {kind} {t} has no descriptor {want}")
                elif want not in rows:
                    failures.append(f"{TEST}: {kind} {t} has no row for {want}")
    for d in sorted(defined - rows):
        failures.append(f"{TEST}: descriptor {d} is not listed")

    for f in failures:
        print(f"FAIL    {f}")
    if not failures:
        print(f"ok      {len(defined)} descriptors, each with a row in {TEST}")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
