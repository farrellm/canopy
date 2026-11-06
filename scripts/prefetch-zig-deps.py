#!/usr/bin/env python3
"""Fill Zig's package cache for a ghostty checkout without Zig's HTTP client.

Usage: prefetch-zig-deps.py <ghostty-src> <zig-global-cache-dir> <url>...

Zig 0.17.1's fetcher fails inside `zig build` with TlsInitializationFailed
against deps.files.ghostty.org, on some machines every time, while curl and a
lone `zig fetch` of the same URL succeed. And a `zig fetch` entry can't be
used: it keeps the archive's top-level directory, which `zig build` strips, so
the build reports a hash mismatch. So this downloads the packages at the given
URLs, each of which must be listed in ghostty's build.zig.zon.json, with curl
(or git, for git+ URLs) and writes each as p/<hash>.tar.gz in the layout
`zig build` writes: one directory named by the hash, holding the archive's
contents with a lone top-level directory stripped.

The URLs are the ones `zig build` failed to fetch (build-libghostty.sh),
so only what the libghostty-vt build needs is downloaded, not ghostty's lazy
dependencies for the app (GTK, imgui, fonts, ...).

Nothing here is trusted: `zig build` recomputes every package's hash from
these files and fails on a mismatch. Existing entries are left alone.
"""

import json
import shutil
import subprocess
import sys
import tarfile
import tempfile
from pathlib import Path


def run(*args: str, cwd: Path | None = None) -> None:
    subprocess.run(args, cwd=cwd, check=True, stdout=sys.stderr)


def fetch_archive(url: str, into: Path, scratch: Path) -> None:
    name = url.rsplit("/", 1)[-1]
    archive = scratch / name
    run("curl", "-sSfL", "--retry", "5", "-o", str(archive), url)
    unpacked = scratch / "unpacked"
    unpacked.mkdir()
    # GNU tar detects gz, xz and zst (through zstd) on its own.
    run("tar", "-xf", str(archive), "-C", str(unpacked))
    entries = list(unpacked.iterdir())
    root = entries[0] if len(entries) == 1 and entries[0].is_dir() else unpacked
    root.rename(into)


def fetch_git(url: str, into: Path) -> None:
    repo, _, rev = url.removeprefix("git+").partition("#")
    run("git", "init", "-q", str(into))
    run("git", "-C", str(into), "fetch", "-q", "--depth", "1", repo, rev)
    run("git", "-C", str(into), "checkout", "-q", "--detach", "FETCH_HEAD")
    shutil.rmtree(into / ".git")


def main() -> None:
    if len(sys.argv) < 4:
        sys.exit(__doc__)
    src, cache, urls = Path(sys.argv[1]), Path(sys.argv[2]), set(sys.argv[3:])
    listed = json.loads((src / "build.zig.zon.json").read_text())
    packages = {h: p for h, p in listed.items() if p["url"] in urls}
    unknown = urls - {p["url"] for p in packages.values()}
    if unknown:
        sys.exit(f"prefetch-zig-deps: not in build.zig.zon.json: {sorted(unknown)}")
    store = cache / "p"
    store.mkdir(parents=True, exist_ok=True)
    for hash_, pkg in sorted(packages.items()):
        target = store / f"{hash_}.tar.gz"
        if target.exists() or (store / hash_).is_dir():
            continue
        url = pkg["url"]
        print(f"prefetch-zig-deps: {pkg['name']} <- {url}", file=sys.stderr)
        # Scratch lives beside p/, not in it, so an interrupted run leaves
        # nothing in the store but whole entries.
        with tempfile.TemporaryDirectory(dir=cache) as tmp:
            scratch = Path(tmp)
            into = scratch / hash_
            if url.startswith("git+"):
                fetch_git(url, into)
            else:
                fetch_archive(url, into, scratch)
            partial = scratch / "out.tar.gz"
            with tarfile.open(partial, "w:gz") as tar:
                tar.add(into, arcname=hash_)
            partial.rename(target)


if __name__ == "__main__":
    main()
