#!/usr/bin/env python3
"""Fail if any module's Haddock coverage is below a floor (DESIGN.md §2.7-§2.8).

Reads `cabal haddock` output on stdin and echoes it. Canopy.Prelude is
excluded: it re-exports relude, whose documentation is relude's.

    cabal haddock lib:<package> 2>&1 | check-haddock.py [--floor PERCENT]
"""

import argparse
import re
import sys

LINE = re.compile(r"^\s*(\d+)% \(\s*(\d+) /\s*(\d+)\) in '([^']+)'")
EXCLUDED = {"Canopy.Prelude"}


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--floor", type=float, default=100.0)
    args = parser.parse_args()

    seen = 0
    failed = []
    for line in sys.stdin:
        sys.stdout.write(line)
        m = LINE.match(line)
        if not m or m.group(4) in EXCLUDED:
            continue
        seen += 1
        documented, total = int(m.group(2)), int(m.group(3))
        if total and 100.0 * documented / total < args.floor:
            failed.append(f"{m.group(4)}: {documented}/{total}")
    if seen == 0:
        sys.exit("check-haddock: no coverage lines read; did haddock run?")
    for f in failed:
        print(f"below {args.floor:g}%: {f}")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
