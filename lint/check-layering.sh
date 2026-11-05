#!/usr/bin/env bash
# Checks .hlint.yaml's layering rules (DESIGN.md §2.6) against fixture modules:
# every import under lint/fixtures/reported must be reported, and none under
# lint/fixtures/allowed. Run from the repository root.
set -euo pipefail

command -v hlint >/dev/null || {
  echo "check-layering: hlint not found" >&2
  exit 2
}

status=0
restricted() {
  # hlint exits non-zero when it finds hints, so read its output, not its status.
  # A fixture hlint cannot parse reports nothing, which would pass an allowed
  # fixture vacuously, so a parse error stops the check.
  local out
  out=$(hlint --hint=.hlint.yaml "$1" 2>&1 || true)
  if [[ $out == *"Parse error"* ]]; then
    echo "ERROR   hlint could not parse $1:" >&2
    echo "$out" >&2
    exit 2
  fi
  # A `modules:` restriction reports as "Avoid restricted module" (within),
  # "Avoid restricted alias" (as/asRequired) or "should be imported
  # post-qualified" (importStyle and qualifiedStyle, rule 6).
  [[ $out == *"Avoid restricted module"* || $out == *"Avoid restricted alias"* ||
    $out == *"should be imported post-qualified"* ]]
}
for f in lint/fixtures/reported/*.hs; do
  if restricted "$f"; then
    echo "ok      reported: $f"
  else
    echo "FAIL    not reported: $f"
    status=1
  fi
done
for f in lint/fixtures/allowed/*.hs; do
  if restricted "$f"; then
    echo "FAIL    reported: $f"
    status=1
  else
    echo "ok      allowed: $f"
  fi
done
exit "$status"
