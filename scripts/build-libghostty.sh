#!/usr/bin/env bash
# Builds libghostty-vt at the pinned commit (DESIGN.md §3.1) into
# _deps/ghostty/<commit>/ and prints the environment to export. Run from the
# repository root:
#
#   eval "$(scripts/build-libghostty.sh)"
#
# Zig: a zig >=0.17 on PATH is used if there is one; otherwise Zig 0.17.1 is
# downloaded into _deps/zig/ and its sha256 checked. Progress goes to stderr,
# so stdout is only the exports. Re-running with the prefix present skips the
# build.
set -euo pipefail

ZIG_VERSION=0.17.1
declare -A ZIG_SHA256=(
  [x86_64-linux]=3581863e3cbcfd0fce0efc682ce7e9e93da96668a977791d356a26d2a7e9b17a
)

root=$(pwd)
[[ -f ghostty-vt-ffi/GHOSTTY_COMMIT ]] || {
  echo "build-libghostty: run from the repository root" >&2
  exit 2
}
commit=$(tr -d '[:space:]' <ghostty-vt-ffi/GHOSTTY_COMMIT)
prefix=$root/_deps/ghostty/$commit

emit() {
  echo "export PKG_CONFIG_PATH=\"$prefix/share/pkgconfig:$prefix/lib/pkgconfig\${PKG_CONFIG_PATH:+:\$PKG_CONFIG_PATH}\""
  echo "export LD_LIBRARY_PATH=\"$prefix/lib\${LD_LIBRARY_PATH:+:\$LD_LIBRARY_PATH}\""
}

if [[ -f $prefix/.complete ]]; then
  echo "build-libghostty: $commit already built in $prefix" >&2
  emit
  exit 0
fi

# Zig >=0.17, the minimum build.zig.zon declares.
zig_ok() {
  local v
  v=$("$1" version 2>/dev/null) || return 1
  [[ $v =~ ^0\.([0-9]+)\. ]] && ((BASH_REMATCH[1] >= 17))
}
zig=$(command -v zig || true)
if [[ -z $zig ]] || ! zig_ok "$zig"; then
  platform="$(uname -m)-$(uname -s | tr '[:upper:]' '[:lower:]')"
  sha=${ZIG_SHA256[$platform]:-}
  [[ -n $sha ]] || {
    echo "build-libghostty: no pinned Zig for $platform; install zig >=0.17" >&2
    exit 2
  }
  zdir=$root/_deps/zig/$ZIG_VERSION
  zig=$zdir/zig
  if [[ ! -x $zig ]]; then
    echo "build-libghostty: fetching Zig $ZIG_VERSION" >&2
    mkdir -p "$zdir"
    tarball=$root/_deps/zig/zig-$platform-$ZIG_VERSION.tar.xz
    curl -sSfL -o "$tarball" "https://ziglang.org/download/$ZIG_VERSION/zig-$platform-$ZIG_VERSION.tar.xz"
    echo "$sha  $tarball" | sha256sum -c --quiet - >&2
    tar -xJf "$tarball" -C "$zdir" --strip-components=1
    rm -f "$tarball"
  fi
fi
echo "build-libghostty: using $zig ($("$zig" version))" >&2

src=$root/_deps/ghostty/src
if [[ ! -d $src/.git ]]; then
  git init -q "$src"
  git -C "$src" remote add origin https://github.com/ghostty-org/ghostty.git
fi
if [[ $(git -C "$src" rev-parse -q --verify HEAD 2>/dev/null) != "$commit" ]]; then
  echo "build-libghostty: fetching ghostty $commit" >&2
  git -C "$src" fetch -q --depth 1 origin "$commit"
  git -C "$src" checkout -q --detach FETCH_HEAD
fi

# Zig's package cache is kept under _deps/, so this build neither reads nor
# writes the user's ~/.cache/zig.
export ZIG_GLOBAL_CACHE_DIR=$root/_deps/zig-cache

# zig build fetches its package dependencies; fetched packages are cached, so
# a retry after a transient network failure makes progress, and only the last
# attempt's failure is fatal. Retries do not cure one failure seen in a
# sandboxed shell: Zig's TLS client failed with TlsInitializationFailed on the
# same few dependencies every time, while curl and a lone `zig fetch` of the
# same URLs succeeded. Run the build outside such a sandbox. (Don't prefetch
# with `zig fetch` to work around it: in Zig 0.17.1 its cache entries made the
# build report hash mismatches for every package.)
# Stamp the pin into the library (DESIGN.md §3.4). libghostty-vt's build info
# reports its own version (build.zig's lib_version, e.g. 0.1.0-dev) with no
# build metadata unless one is given, so the commit goes in as semver build
# metadata through -Dlib-version-string. The .pc file's Version carries it too.
lib_version=$(sed -nE 's/^const lib_version = "([^"+]+)(\+[^"]*)?";$/\1/p' "$src/build.zig")
[[ -n $lib_version ]] || {
  echo "build-libghostty: no lib_version in build.zig; has the build changed?" >&2
  exit 1
}

echo "build-libghostty: building libghostty-vt $lib_version+$commit" >&2
for attempt in 1 2 3 4 5; do
  if (cd "$src" && "$zig" build -Demit-lib-vt -Doptimize=ReleaseFast \
    -Dlib-version-string="$lib_version+$commit" --prefix "$prefix") >&2; then
    break
  elif ((attempt == 5)); then
    echo "build-libghostty: zig build failed $attempt times" >&2
    exit 1
  fi
  echo "build-libghostty: zig build failed; retrying ($attempt)" >&2
done
touch "$prefix/.complete"
emit
