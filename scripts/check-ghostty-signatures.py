#!/usr/bin/env python3
"""Checks ghostty-vt-ffi's Haskell types against the pinned libghostty-vt
headers' C types (DESIGN.md §3.3): every `foreign import capi`'s parameters
and result, and every field of every record that marshals a C struct.

`capi` alone does not do this. GHC compiles each call through a C stub whose
parameter types come from the Haskell signature, and the C compiler converts
integers silently: a `Word16` declared for a `uint32_t` parameter, or a
`Word8` result for a `size_t` one, compiles without a warning and truncates
at run time. A `Ptr` to a Haskell-defined type (every struct, and the handles,
which are newtypes over one) reaches C as `void *`, which converts to any
pointer, so one handle passed for another compiles too. hsc2hs's `#peek` and
`#poke` read and write at the Haskell field's width, which nothing compares
with the C field's. (Checked with GHC 9.12.4 and GCC 16.)

Callback types are compared too: a `FunPtr` synonym against the C function
pointer typedef of its name, since a `wrapper` built at the wrong width
corrupts the same way.

So this compares types by name. A Haskell type maps to a C type (`Word16` to
`uint16_t`, `CSize` to `size_t`, `Ptr T` to `T *`, `[T]` to an array of `T`,
and a `Ghostty…` type to the C type of the same name); `const` is ignored.
Where the names differ, a Haskell `type` synonym and a C scalar `typedef` are
each resolved to what they name (`GhosttyCell` is `Word64` is `uint64_t`), and
every such synonym must name what the typedef of its name does.
Enums and handles are compared by name only, never resolved to `int` or a
pointer, so passing the wrong one is a failure.

Run from the repository root. The headers are read from
`_deps/ghostty/<GHOSTTY_COMMIT>/include` (scripts/build-libghostty.sh), or
from `--include`. Exits non-zero, naming each mismatch, on failure.
"""

import argparse
import re
import sys
from pathlib import Path

PKG = Path("ghostty-vt-ffi")
SHIMS = PKG / "include" / "gvtffi_shims.h"

PRIM = {
    "Word8": "uint8_t",
    "Word16": "uint16_t",
    "Word32": "uint32_t",
    "Word64": "uint64_t",
    "Int8": "int8_t",
    "Int16": "int16_t",
    "Int32": "int32_t",
    "Int64": "int64_t",
    "CSize": "size_t",
    "CBool": "bool",
    "CInt": "int",
    "CChar": "char",
    "CDouble": "double",
    "CFloat": "float",
    "CIntPtr": "intptr_t",
    "()": "void",
}

C_SCALARS = set(PRIM.values())


# * C


def strip_comments(text: str) -> str:
    return re.sub(r"//[^\n]*", "", re.sub(r"/\*.*?\*/", "", text, flags=re.S))


def c_norm(t: str) -> str:
    """A C type without qualifiers or spacing: `const uint8_t *` is `uint8_t*`."""
    t = re.sub(r"\b(const|struct)\b", "", t)
    return re.sub(r"\s+", "", t)


def split_param(p: str):
    """`const uint8_t* data` -> (`uint8_t*`, `data`); an unnamed one keeps its type."""
    p = " ".join(p.split())
    m = re.match(r"(.*?[\s\*])(\w+)$", p)
    if not m or c_norm(m.group(1)) == "":
        return c_norm(p), None
    return c_norm(m.group(1)), m.group(2)


class CHeaders:
    def __init__(self, root: Path):
        self.functions = {}  # name -> (result, [param types])
        self.structs = {}  # name -> [(field, type)]
        self.unions = {}  # name -> [(member, type)]
        self.callbacks = {}  # function pointer typedef -> (result, [param types])
        self.typedefs = {}  # scalar typedef name -> its scalar
        texts = [p.read_text() for p in sorted(root.rglob("*.h"))]
        for text in texts + [SHIMS.read_text()]:
            self.read(strip_comments(text))

    def read(self, s: str):
        for m in re.finditer(
            r"(?:GHOSTTY_API|static inline)\s+([^;(){}#]*?)\b((?:ghostty|gvtffi)_\w+)\s*\(([^)]*)\)", s
        ):
            params = [p for p in m.group(3).split(",") if p.strip() not in ("", "void")]
            self.functions[m.group(2)] = (c_norm(m.group(1)), [split_param(p)[0] for p in params])
        for m in re.finditer(r"typedef\s+([\w\s\*]+?)\s*\(\s*\*\s*(Ghostty\w+)\s*\)\s*\(([^)]*)\)\s*;", s):
            params = [p for p in m.group(3).split(",") if p.strip() not in ("", "void")]
            self.callbacks[m.group(2)] = (c_norm(m.group(1)), [split_param(p)[0] for p in params])
        for m in re.finditer(r"typedef\s+(\w+)\s+(Ghostty\w+)\s*;", s):
            if m.group(1) in C_SCALARS:
                self.typedefs[m.group(2)] = m.group(1)
        for m in re.finditer(r"(?:typedef struct\s*|^struct (Ghostty\w+)\s*)\{", s, re.M):
            end = matching_brace(s, m.end() - 1)
            name = m.group(1)
            if not name:
                n = re.match(r"\s*(Ghostty\w+)\s*;", s[end + 1 :])
                if not n:
                    continue
                name = n.group(1)
            self.structs[name] = struct_fields(s[m.end() : end])
        for m in re.finditer(r"typedef union\s*\{", s):
            end = matching_brace(s, m.end() - 1)
            n = re.match(r"\s*(Ghostty\w+)\s*;", s[end + 1 :])
            if n:
                self.unions[n.group(1)] = struct_fields(s[m.end() : end])

    def resolve(self, t: str) -> str:
        base, stars = t.rstrip("*"), t[len(t.rstrip("*")) :]
        return self.typedefs.get(base, base) + stars


def matching_brace(s: str, i: int) -> int:
    depth = 0
    for j in range(i, len(s)):
        depth += {"{": 1, "}": -1}.get(s[j], 0)
        if depth == 0:
            return j
    sys.exit("check-ghostty-signatures: unbalanced braces")


def struct_fields(body: str):
    """The top-level declarations of a struct body, as (name, type); an array
    field's type ends in `[]`. A nested union or struct, and a function pointer
    written inline, are one field each, which no Haskell type matches."""
    fields, depth, start = [], 0, 0
    for i, ch in enumerate(body):
        depth += {"{": 1, "}": -1}.get(ch, 0)
        if ch == ";" and depth == 0:
            decl = " ".join(body[start:i].split())
            start = i + 1
            if "{" in decl:
                fields.append((decl.rsplit(None, 1)[-1], "<aggregate>"))
                continue
            fp = re.search(r"\(\s*\*\s*(\w+)\s*\)\s*\(", decl)
            if fp:
                fields.append((fp.group(1), "<function pointer>"))
                continue
            m = re.match(r"(.*?)(\w+)\s*(\[[^\]]*\])?$", decl)
            fields.append((m.group(2), c_norm(m.group(1)) + ("[]" if m.group(3) else "")))
    return fields


# * Haskell


def split_arrows(sig: str):
    """A signature's top-level `->`-separated parts."""
    parts, depth, cur = [], 0, ""
    i = 0
    while i < len(sig):
        ch = sig[i]
        depth += {"(": 1, ")": -1, "[": 1, "]": -1}.get(ch, 0)
        if depth == 0 and sig.startswith("->", i):
            parts.append(cur.strip())
            cur, i = "", i + 2
            continue
        cur += ch
        i += 1
    return parts + [cur.strip()]


def unparen(t: str) -> str:
    t = t.strip()
    while t.startswith("(") and t.endswith(")") and t != "()":
        inner, depth = t[1:-1], 0
        for ch in inner:
            depth += {"(": 1, ")": -1}.get(ch, 0)
            if depth < 0:
                return t
        t = inner.strip()
    return t


def hs_to_c(t: str) -> str:
    t = unparen(t)
    if t.startswith("IO "):
        t = unparen(t[3:])
    m = re.match(r"(?:Ptr|ConstPtr)\s+(.*)$", t)
    if m:
        inner = unparen(m.group(1))
        return ("void" if inner == "()" else hs_to_c(inner)) + "*"
    if t.startswith("[") and t.endswith("]"):
        return hs_to_c(t[1:-1]) + "[]"
    return PRIM.get(t, t)


class HsSources:
    def __init__(self):
        self.imports = []  # (file, haskell name, C name, [param types], result)
        self.records = []  # (file, type, [(field, type)])
        self.synonyms = {}  # Haskell type synonym -> its C type
        self.callbacks = []  # (file, FunPtr synonym, [param types], result)
        for p in sorted((PKG / "src").rglob("*.hs*")):
            self.read(p, p.read_text())

    def read(self, path: Path, s: str):
        for m in re.finditer(r"^type (Ghostty\w+) = (.+)$", s, re.M):
            fp = re.match(r"FunPtr \((.*)\)$", m.group(2))
            if fp:
                parts = split_arrows(fp.group(1))
                self.callbacks.append((path, m.group(1), parts[:-1], parts[-1]))
            else:
                self.synonyms[m.group(1)] = hs_to_c(m.group(2))
        for m in re.finditer(
            r'^foreign import capi \w+ "\S+ (\w+)"\n\s+(\w+) :: (.*?)(?=\n\s*\n|\n\S|\Z)', s, re.M | re.S
        ):
            parts = split_arrows(" ".join(m.group(3).split()))
            self.imports.append((path, m.group(2), m.group(1), parts[:-1], parts[-1]))
        for m in re.finditer(r"^(?:data|newtype) (Ghostty\w+) = \1\s*\{(.*?)\n\s*\}", s, re.M | re.S):
            body = re.sub(r"--[^\n]*", "", m.group(2))
            fields = re.findall(r"(\w+) :: ([^,\n]+)", body)
            self.records.append((path, m.group(1), [(f, t.strip()) for f, t in fields]))

    def resolve(self, t: str) -> str:
        base, stars = t.rstrip("*"), t[len(t.rstrip("*")) :]
        return self.synonyms.get(base, base) + stars


def field_name(c: str) -> str:
    parts = c.split("_")
    n = parts[0] + "".join(p[:1].upper() + p[1:] for p in parts[1:])
    return {"data": "data_"}.get(n, n)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--include", type=Path, help="the libghostty-vt include directory")
    args = parser.parse_args()
    commit = (PKG / "GHOSTTY_COMMIT").read_text().strip()
    root = (args.include or Path("_deps/ghostty") / commit / "include") / "ghostty" / "vt"
    if not root.is_dir():
        sys.exit(f"check-ghostty-signatures: no headers at {root}; run scripts/build-libghostty.sh")
    c, hs = CHeaders(root), HsSources()

    def agree(h: str, ct: str) -> bool:
        hc = hs_to_c(h)
        return hc == ct or hs.resolve(hc) == c.resolve(ct)

    failures = []
    # A type synonym matched by name stands for what it names, so it must name
    # what the C typedef does (GhosttyCell is uint64_t).
    for name, h in sorted(hs.synonyms.items()):
        if name in c.typedefs and h != c.typedefs[name]:
            failures.append(f"{PKG}/src: type {name} is {h}, C's typedef is {c.typedefs[name]}")
        elif name not in c.typedefs:
            failures.append(f"{PKG}/src: type {name} has no C scalar typedef to check it against")
    calls = [(p, n, cn, ps, r, c.functions) for p, n, cn, ps, r in hs.imports]
    calls += [(p, n, n, ps, r, c.callbacks) for p, n, ps, r in hs.callbacks]
    for path, name, cname, params, result, protos in calls:
        if cname not in protos:
            failures.append(f"{path}: {name}: no C prototype for {cname}")
            continue
        cres, cparams = protos[cname]
        where = f"{path}: {name}" + ("" if name == cname else f" ({cname})")
        if len(params) != len(cparams):
            failures.append(f"{where}: {len(params)} parameters, C has {len(cparams)}")
            continue
        for i, (h, ct) in enumerate(zip(params, cparams), 1):
            if not agree(h, ct):
                failures.append(f"{where}: parameter {i} is {h}, C has {ct}")
        if not agree(result, cres):
            failures.append(f"{where}: result is {result}, C has {cres}")

    checked = 0
    for path, name, fields in hs.records:
        if name not in c.structs:
            continue
        checked += 1
        cfields = c.structs[name]
        if len(fields) != len(cfields):
            failures.append(f"{path}: {name} has {len(fields)} fields, C has {len(cfields)}")
            continue
        for (f, h), (cf, ct) in zip(fields, cfields):
            # A union whose arms all have one type is read as a record field
            # named after the arm (§3.3: GhosttyPoint's value.coordinate).
            arms = dict((field_name(a), t) for a, t in c.unions.get(ct, []))
            if f == field_name(cf) and ct not in c.unions:
                ok = ct not in ("<aggregate>", "<function pointer>") and agree(h, ct)
                if not ok:
                    failures.append(f"{path}: {name}.{f} is {h}, C has {ct}")
            elif f in arms:
                if not agree(h, arms[f]):
                    failures.append(f"{path}: {name}.{f} is {h}, C's {cf}.{f} is {arms[f]}")
            else:
                failures.append(f"{path}: {name}.{f} is not C's field {cf}")

    for f in failures:
        print(f"FAIL    {f}")
    if not failures:
        print(
            f"ok      {len(hs.imports)} capi imports, {len(hs.callbacks)} callback types and"
            f" {checked} struct records agree with the headers"
        )
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
