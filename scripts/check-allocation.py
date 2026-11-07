#!/usr/bin/env python3
"""Fails if any benchmark allocates more than its committed baseline allows
(DESIGN.md §2.8 step 8, §12.3). Copied from cassini's.

tasty-bench's --fail-if-slower thresholds time only, so the allocation gate is
this diff of the CSV's Allocated column. Allocation depends on the compiler,
the flags and the library's optimization level, not on the machine, so a
baseline from any machine holds for the same GHC if it was built the same way:
`cabal bench <suite>` with CI's flags, whose library is at cabal's default
optimization. Allocation is the Haskell side's only; what a C library
allocates is not counted.

    check-allocation.py BASELINE.csv CURRENT.csv [--threshold PERCENT] [--slack BYTES] [--only PREFIX]

A benchmark fails only when it is over by more than the threshold *and* by more
than the slack, so a near-zero baseline does not fail on a few bytes of noise.
A benchmark in the baseline but not in the run fails; one in the run but not in
the baseline is reported, and gated once a baseline that has it is committed.
Faster or leaner is never a failure: regenerate the baseline deliberately, with
a commit saying why.

Run from the repository root. Exits non-zero on failure.
"""

import argparse
import csv
import sys


def allocations(path):
    with open(path, newline="") as f:
        return {row["Name"]: int(row["Allocated"]) for row in csv.DictReader(f)}


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("baseline")
    parser.add_argument("current")
    parser.add_argument("--threshold", type=float, default=10.0, help="percent over baseline allowed")
    parser.add_argument("--slack", type=int, default=1024, help="bytes over baseline always allowed")
    parser.add_argument("--only", default="", help="check only benchmarks whose names start with this")
    args = parser.parse_args()

    baseline = {k: v for k, v in allocations(args.baseline).items() if k.startswith(args.only)}
    current = {k: v for k, v in allocations(args.current).items() if k.startswith(args.only)}
    if not baseline:
        print(f"no baseline benchmark starts with {args.only!r}")
        sys.exit(1)
    failed = False
    for name, base in sorted(baseline.items()):
        if name not in current:
            print(f"missing   {name}")
            failed = True
            continue
        now = current[name]
        change = 100.0 * (now - base) / base if base else (0.0 if now == 0 else float("inf"))
        over = change > args.threshold and now - base > args.slack
        failed |= over
        print(f"{'FAIL' if over else 'ok':8}  {change:+7.1f}%  {now:>14,}  {name}")
    for name in sorted(current.keys() - baseline.keys()):
        print(f"new       {name}")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
