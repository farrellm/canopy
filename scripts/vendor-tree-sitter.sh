#!/usr/bin/env bash
# Replaces the vendored tree-sitter runtime with the given release tag
# (DESIGN.md §5A.1) and records the tag. A bump is one deliberate commit that
# also regenerates the shims (scripts/gen-ts-shims.py) and says why. Run from
# the repository root:
#
#   scripts/vendor-tree-sitter.sh v0.27.1
set -euo pipefail

tag=${1:?usage: vendor-tree-sitter.sh <tag>}
dest=tree-sitter-ffi/vendor/tree-sitter
[[ -d tree-sitter-ffi ]] || {
  echo "vendor-tree-sitter: run from the repository root" >&2
  exit 2
}

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
git -c advice.detachedHead=false clone -q --depth 1 --branch "$tag" https://github.com/tree-sitter/tree-sitter.git "$tmp/ts"

rm -rf "$dest"
mkdir -p "$dest/lib"
cp -r "$tmp/ts/lib/src" "$tmp/ts/lib/include" "$dest/lib/"
cp "$tmp/ts/LICENSE" "$dest/LICENSE"
echo "$tag" >tree-sitter-ffi/TREE_SITTER_VERSION
echo "vendor-tree-sitter: $dest is now $tag; regenerate the shims" >&2
