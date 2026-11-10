#!/usr/bin/env bash
# Builds libghostty-vt at the pinned release (DESIGN.md §3.1) into
# _deps/ghostty/<version>/ and prints the environment to export. Run from the
# repository root:
#
#   eval "$(scripts/build-libghostty.sh)"
#   eval "$(scripts/build-libghostty.sh --release)"   # -Dcpu=baseline (D-4)
#
# The source is upstream's release tarball and its deps tarball, each checked
# against the sha256 pinned here, so the build runs offline with `--system`.
# The release tarball carries LIB_VERSION, which makes `zig build` build the
# library (no -Demit-lib-vt) and report exactly the release version.
#
# Zig: a zig 0.17.x on PATH is used if there is one; otherwise Zig 0.17.1 is
# downloaded into _deps/zig/ and its sha256 checked. Progress goes to stderr,
# so stdout is only the exports. Re-running with the prefix present skips the
# build. A --release build goes to its own prefix, _deps/ghostty/<version>-baseline.
set -euo pipefail

release=0
case ${1:-} in
  "") ;;
  --release) release=1 ;;
  *)
    echo "usage: build-libghostty.sh [--release]" >&2
    exit 2
    ;;
esac

ZIG_VERSION=0.17.1
declare -A ZIG_SHA256=(
  [x86_64-linux]=3581863e3cbcfd0fce0efc682ce7e9e93da96668a977791d356a26d2a7e9b17a
)
# The sums of the release's two tarballs, as its minisign-signed SHA256SUMS
# lists them (checked against upstream's key when the pin moved; see
# DESIGN.md §3.1). A new pin changes these with it.
declare -A GHOSTTY_SHA256=(
  [libghostty-vt-1.0.0.tar.gz]=4a6ef758be721923e1969e7b81b443dcc92d8dd3fb5a39600c03a8817a753676
  [libghostty-vt-1.0.0-deps.tar.gz]=4d5ea5e448e4c7ea8fb55ccbaa8668f2a97d44f438cee831e2b7ecd162d85c29
)
MINISIGN_KEY=RWQlAjJC23149WL2sEpT/l0QKy7hMIFhYdQOFy0Z7z7PbneUgvlsnYcV

root=$(pwd)
[[ -f ghostty-vt-ffi/LIBGHOSTTY_VT_VERSION ]] || {
  echo "build-libghostty: run from the repository root" >&2
  exit 2
}
version=$(tr -d '[:space:]' <ghostty-vt-ffi/LIBGHOSTTY_VT_VERSION)
if ((release)); then
  prefix=$root/_deps/ghostty/$version-baseline
else
  prefix=$root/_deps/ghostty/$version
fi

emit() {
  echo "export PKG_CONFIG_PATH=\"$prefix/lib/pkgconfig\${PKG_CONFIG_PATH:+:\$PKG_CONFIG_PATH}\""
  echo "export LD_LIBRARY_PATH=\"$prefix/lib\${LD_LIBRARY_PATH:+:\$LD_LIBRARY_PATH}\""
}

if [[ -f $prefix/.complete ]]; then
  echo "build-libghostty: $version already built in $prefix" >&2
  emit
  exit 0
fi

# Zig 0.17.x: libghostty-vt 1.0 requires the major and minor of its
# minimum_zig_version (0.17.0) exactly.
zig_ok() {
  local v
  v=$("$1" version 2>/dev/null) || return 1
  [[ $v =~ ^0\.17\. ]]
}
zig=$(command -v zig || true)
if [[ -z $zig ]] || ! zig_ok "$zig"; then
  platform="$(uname -m)-$(uname -s | tr '[:upper:]' '[:lower:]')"
  sha=${ZIG_SHA256[$platform]:-}
  [[ -n $sha ]] || {
    echo "build-libghostty: no pinned Zig for $platform; install zig 0.17.x" >&2
    exit 2
  }
  zdir=$root/_deps/zig/$ZIG_VERSION
  zig=$zdir/zig
  if [[ ! -x $zig ]]; then
    echo "build-libghostty: fetching Zig $ZIG_VERSION" >&2
    # Unpacked beside $zdir and moved into place whole, so an interrupted
    # fetch never leaves a zig binary without its lib/ for the next run.
    mkdir -p "$root/_deps/zig"
    tarball=$root/_deps/zig/zig-$platform-$ZIG_VERSION.tar.xz
    curl -sSfL --retry 5 -o "$tarball" "https://ziglang.org/download/$ZIG_VERSION/zig-$platform-$ZIG_VERSION.tar.xz"
    echo "$sha  $tarball" | sha256sum -c --quiet - >&2
    rm -rf "$zdir" "$zdir.partial"
    mkdir -p "$zdir.partial"
    tar -xJf "$tarball" -C "$zdir.partial" --strip-components=1
    mv "$zdir.partial" "$zdir"
    rm -f "$tarball"
  fi
fi
echo "build-libghostty: using $zig ($("$zig" version))" >&2

# The two tarballs, each checked against its pinned sum and unpacked side by
# side into _deps/ghostty/src/ (libghostty-vt-<V>/ and libghostty-vt-<V>-deps/).
# Each is unpacked beside its directory and moved into place whole.
src_root=$root/_deps/ghostty/src
mkdir -p "$src_root"
for name in "libghostty-vt-$version" "libghostty-vt-$version-deps"; do
  tarball=$name.tar.gz
  sha=${GHOSTTY_SHA256[$tarball]:-}
  [[ -n $sha ]] || {
    echo "build-libghostty: no pinned sha256 for $tarball" >&2
    exit 2
  }
  [[ -d $src_root/$name ]] && continue
  echo "build-libghostty: fetching $tarball" >&2
  curl -sSfL --retry 5 -o "$src_root/$tarball" "https://release.files.ghostty.org/libghostty-vt/$version/$tarball"
  echo "$sha  $src_root/$tarball" | sha256sum -c --quiet - >&2
  # The pinned sum is the required check. A signature check is extra, when
  # minisign is installed.
  if command -v minisign >/dev/null; then
    curl -sSfL --retry 5 -o "$src_root/$tarball.minisig" "https://release.files.ghostty.org/libghostty-vt/$version/$tarball.minisig"
    minisign -Vq -P "$MINISIGN_KEY" -m "$src_root/$tarball" >&2
    rm -f "$src_root/$tarball.minisig"
  fi
  rm -rf "$src_root/$name" "$src_root/$name.partial"
  mkdir -p "$src_root/$name.partial"
  tar -xzf "$src_root/$tarball" -C "$src_root/$name.partial" --strip-components=1
  mv "$src_root/$name.partial" "$src_root/$name"
  rm -f "$src_root/$tarball"
done

# A new, empty Zig cache for each build, under _deps/ so the user's
# ~/.cache/zig is neither read nor written. Upstream's packaging guide: with
# Zig 0.17, a cache holding a package archive can crash a --system build with
# "reached unreachable code".
export ZIG_GLOBAL_CACHE_DIR=$root/_deps/zig-cache/$(basename "$prefix")
rm -rf "$ZIG_GLOBAL_CACHE_DIR"

build_opts=(--system "$src_root/libghostty-vt-$version-deps" -Doptimize=ReleaseFast)
((release)) && build_opts+=(-Dcpu=baseline)

log=$root/_deps/ghostty/build.log
echo "build-libghostty: building libghostty-vt $version ($prefix)" >&2
if ! (cd "$src_root/libghostty-vt-$version" && "$zig" build "${build_opts[@]}" --prefix "$prefix") >"$log" 2>&1; then
  cat "$log" >&2
  echo "build-libghostty: zig build failed (log: $log)" >&2
  exit 1
fi
rm -rf "$ZIG_GLOBAL_CACHE_DIR"
touch "$prefix/.complete"
emit
