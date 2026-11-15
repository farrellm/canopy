#!/usr/bin/env python3
"""Generate tendril's Unicode tables from the UCD at a pinned version (DESIGN.md §5.3).

Writes `Tendril.Text.Unicode.Tables`, which answers, for every code point:

- its Grapheme_Cluster_Break, Extended_Pictographic and Indic_Conjunct_Break,
  the properties UAX #29's segmentation reads;
- its width in cells, as libghostty-vt computes it (below);
- whether it is the base of an emoji variation sequence, which decides
  whether a following VS15 or VS16 changes a cluster's width;
- whether it adds no width inside a cluster, libghostty-vt's
  `width_zero_in_grapheme`, which its cluster width reads;
- whether it is an Emoji_Modifier or an Emoji_Modifier_Base, which
  libghostty-vt's tailoring of UAX #29 reads: a modifier continues a cluster
  only straight after a base (uucode's `isExtendNoControl`).

and `Test.Tendril.Text.Unicode.Expected`, a digest of each property over all
of 0..10FFFF, which the test suite recomputes through the public lookups: the
`--check` below compares text, and the digests check what GHC decodes from it.
It also writes `Test.Tendril.Text.Grapheme.Conformance`, the cases of the
UCD's `GraphemeBreakTest.txt`, which `Tendril.Text.Grapheme` must pass.

**The width rule is libghostty-vt's**, because width is the terminal's
decision (§5.3). libghostty-vt 1.0.0 takes it from uucode
(`uucode/src/components.zig`, `Wcwidth`) and folds uucode's two widths into
one (`src/build/uucode_config.zig`):

    standalone = 0 for Cc, Cs, Zl, Zp; 1 for U+00AD;
                 0 for other Default_Ignorable_Code_Point;
                 2 for U+2E3A and U+2E3B (uucode's 3, capped);
                 2 for East_Asian_Width W or F, and for Regional_Indicator;
                 1 otherwise; and 2 for U+20E3
    zero_in_grapheme = standalone (before U+20E3's) is 0, or Emoji_Modifier,
                 Mn, Me, or Grapheme_Cluster_Break V, T or Prepend
    width      = 0 if zero_in_grapheme and not Emoji_Modifier and not Prepend,
                 else standalone

The UCD version is libghostty-vt's: its uucode carries copies of the 18.0.0
files, byte for byte the ones whose sums are pinned here. A libghostty-vt
bump that moves its Unicode moves this pin with it, in the same commit.

The UCD files are downloaded from unicode.org into `_deps/ucd/<version>/` and
checked against the sums here; a file already there is checked, not fetched.

Run from the repository root. With --check, nothing is written, and the exit
status is 1 if the committed modules are not what the pinned UCD generates.
"""

import argparse
import difflib
import hashlib
import sys
import urllib.request
from pathlib import Path

UCD_VERSION = "18.0.0"
UCD_URL = f"https://www.unicode.org/Public/{UCD_VERSION}/ucd"
UCD_DIR = Path("_deps/ucd") / UCD_VERSION
UCD_SHA256 = {
    "auxiliary/GraphemeBreakProperty.txt": "0839dcb79e4ac639ecd538b1abf7c9d22e3f9dd265b7e182d33627aa4d75b45a",
    "emoji/emoji-data.txt": "80d00f8e616a0ef27fd6b8de3b758c06383b5d917e2977709578e68baf733bf1",
    "emoji/emoji-variation-sequences.txt": "ff1707564aa1f1b2fcf4ec92d609d4cb26940bc0d4dcf07f5328ad6879e84da3",
    "extracted/DerivedEastAsianWidth.txt": "f4c7bc4537a71e12452f6347ecbbc71c31cba21b5190c92be1ae296b0ef32b09",
    "extracted/DerivedGeneralCategory.txt": "d6b151d2d40ee9b1876d26f417980f45ffae47b6055ccf7203cb31f07a030f94",
    "DerivedCoreProperties.txt": "09c928886a178fcafd93c29e4bd59073a058e5a100b716d425cb563ab50f68c9",
    "auxiliary/GraphemeBreakTest.txt": "b0cf047ee94485bbdc846de2b902f5f8a815f6b674f9d04223cddadd91c9df31",
}

OUT = Path("tendril/src/Tendril/Text/Unicode/Tables.hs")
OUT_EXPECTED = Path("tendril/test/Test/Tendril/Text/Unicode/Expected.hs")
OUT_CONFORMANCE = Path("tendril/test/Test/Tendril/Text/Grapheme/Conformance.hs")

N = 0x110000

# Grapheme_Cluster_Break values: (UCD name, Haskell constructor, Haddock).
# The order is the constructors' and so the digest's.
GCB = [
    ("Other", "Other", "Any other code point."),
    ("CR", "CR", "Carriage return, U+000D."),
    ("LF", "LF", "Line feed, U+000A."),
    ("Control", "Control", "Other controls, separators and format characters."),
    ("Extend", "Extend", "Combining marks and other extenders."),
    ("ZWJ", "ZWJ", "Zero-width joiner, U+200D."),
    ("Regional_Indicator", "RegionalIndicator", "A regional indicator symbol (flags)."),
    ("Prepend", "Prepend", "A prefix that joins the following character."),
    ("SpacingMark", "SpacingMark", "A spacing combining mark."),
    ("L", "HangulL", "A Hangul leading consonant jamo."),
    ("V", "HangulV", "A Hangul vowel jamo."),
    ("T", "HangulT", "A Hangul trailing consonant jamo."),
    ("LV", "HangulLV", "A precomposed Hangul LV syllable."),
    ("LVT", "HangulLVT", "A precomposed Hangul LVT syllable."),
]

# Indic_Conjunct_Break values, in the constructors' order.
INCB = [
    ("None", "IncbNone", "Not part of an Indic conjunct."),
    ("Linker", "IncbLinker", "A virama that links consonants into a conjunct."),
    ("Consonant", "IncbConsonant", "A consonant a conjunct can link."),
    ("Extend", "IncbExtend", "A mark that may sit between a consonant and a linker."),
]

# The two-stage table's block size, as a shift: the smallest total for 18.0.0
# (see `--sizes`).
SHIFT = 7


def fetch(rel):
    path = UCD_DIR / rel
    if not path.exists():
        path.parent.mkdir(parents=True, exist_ok=True)
        print(f"gen-unicode-tables: fetching {UCD_URL}/{rel}", file=sys.stderr)
        with urllib.request.urlopen(f"{UCD_URL}/{rel}", timeout=60) as r:
            data = r.read()
        tmp = path.with_suffix(".part")
        tmp.write_bytes(data)
        tmp.rename(path)
    data = path.read_bytes()
    got = hashlib.sha256(data).hexdigest()
    if got != UCD_SHA256[rel]:
        sys.exit(
            f"gen-unicode-tables: {path}: sha256 {got}, pinned {UCD_SHA256[rel]}; "
            "delete it to fetch it again"
        )
    return data.decode("utf-8")


def records(text, missing=None):
    """Yield (first, last, fields) for every data line; with `missing`, call it
    on each `# @missing:` line's (first, last, fields) first, in file order."""
    for line in text.splitlines():
        if line.startswith("# @missing:"):
            if missing is not None:
                cps, *fields = [f.strip() for f in line[len("# @missing:") :].split(";")]
                missing(*span(cps), fields)
            continue
        line = line.split("#", 1)[0].strip()
        if not line:
            continue
        cps, *fields = [f.strip() for f in line.split(";")]
        yield (*span(cps), fields)


def span(cps):
    a, _, b = cps.partition("..")
    return int(a, 16), int(b or a, 16)


def fill(table, first, last, value):
    table[first : last + 1] = [value] * (last + 1 - first)


def load():
    gcb_index = {name: i for i, (name, _, _) in enumerate(GCB)}
    gcb = [0] * N
    for a, b, (v,) in records(fetch("auxiliary/GraphemeBreakProperty.txt")):
        fill(gcb, a, b, gcb_index[v])

    gc = ["Cn"] * N
    for a, b, (v,) in records(fetch("extracted/DerivedGeneralCategory.txt")):
        fill(gc, a, b, v)

    # The @missing lines give the defaults (Neutral, and Wide for the CJK
    # blocks), in order, before the data lines override them.
    eaw = ["N"] * N

    def eaw_missing(a, b, fields):
        fill(eaw, a, b, {"Neutral": "N", "Wide": "W"}[fields[0]])

    for a, b, (v,) in records(fetch("extracted/DerivedEastAsianWidth.txt"), eaw_missing):
        fill(eaw, a, b, v)

    ignorable = [False] * N
    incb_index = {name: i for i, (name, _, _) in enumerate(INCB)}
    incb = [0] * N
    for a, b, fields in records(fetch("DerivedCoreProperties.txt")):
        if fields[0] == "Default_Ignorable_Code_Point":
            fill(ignorable, a, b, True)
        elif fields[0] == "InCB":
            fill(incb, a, b, incb_index[fields[1]])

    ext_pict = [False] * N
    modifier = [False] * N
    modifier_base = [False] * N
    for a, b, (v,) in records(fetch("emoji/emoji-data.txt")):
        if v == "Extended_Pictographic":
            fill(ext_pict, a, b, True)
        elif v == "Emoji_Modifier":
            fill(modifier, a, b, True)
        elif v == "Emoji_Modifier_Base":
            fill(modifier_base, a, b, True)

    vs_base = [False] * N
    for line in fetch("emoji/emoji-variation-sequences.txt").splitlines():
        line = line.split("#", 1)[0].strip()
        if line:
            vs_base[int(line.split(";")[0].split()[0], 16)] = True

    widths = [ghostty_width(cp, gc[cp], eaw[cp], gcb[cp], ignorable[cp], modifier[cp]) for cp in range(N)]
    return {
        "graphemeBreak": gcb,
        "indicConjunctBreak": incb,
        "extendedPictographic": [int(x) for x in ext_pict],
        "width": [w for w, _ in widths],
        "emojiVariationBase": [int(x) for x in vs_base],
        "zeroWidthInCluster": [int(z) for _, z in widths],
        "emojiModifier": [int(x) for x in modifier],
        "emojiModifierBase": [int(x) for x in modifier_base],
    }


def ghostty_width(cp, gc, eaw, gcb, ignorable, modifier):
    """The module docstring's rule, uucode's `Wcwidth` and ghostty's fold of it:
    the width, and `zero_in_grapheme`."""
    if gc in ("Cc", "Cs", "Zl", "Zp"):
        w = 0
    elif cp == 0x00AD:
        w = 1
    elif ignorable:
        w = 0
    elif cp in (0x2E3A, 0x2E3B):
        w = 2
    elif eaw in ("W", "F") or GCB[gcb][0] == "Regional_Indicator":
        w = 2
    else:
        w = 1
    standalone = 2 if cp == 0x20E3 else w
    g = GCB[gcb][0]
    zero_in_grapheme = w == 0 or modifier or gc in ("Mn", "Me") or g in ("V", "T", "Prepend")
    if zero_in_grapheme and not modifier and g != "Prepend":
        return 0, zero_in_grapheme
    return standalone, zero_in_grapheme


def classes(props):
    """Each code point's class: an index into the distinct property tuples."""
    keys = list(props)
    index, rows, cls = {}, [], [0] * N
    for cp in range(N):
        t = tuple(props[k][cp] for k in keys)
        if t not in index:
            index[t] = len(rows)
            rows.append(t)
        cls[cp] = index[t]
    return keys, rows, cls


def two_stage(cls, shift):
    size = 1 << shift
    blocks, stage1, stage2 = {}, [], []
    for start in range(0, N, size):
        block = tuple(cls[start : start + size])
        if block not in blocks:
            blocks[block] = len(blocks)
            stage2.extend(block)
        stage1.append(blocks[block])
    return stage1, stage2


def fnv1a(values):
    """64-bit FNV-1a over one byte per value; Test.Tendril.Text.Unicode's
    `digest` is the same function."""
    h = 0xCBF29CE484222325
    for v in values:
        h = ((h ^ v) * 0x100000001B3) & 0xFFFFFFFFFFFFFFFF
    return h


def literal(name, data, doc):
    out = [f"-- | {doc}", f"{name} :: ByteString", f"{name} ="]
    chunks = [data[i : i + 24] for i in range(0, len(data), 24)] or [[]]
    for i, chunk in enumerate(chunks):
        body = "".join(f"\\x{b:02x}" for b in chunk)
        first = '"' if i == 0 else "\\"
        last = '"' if i == len(chunks) - 1 else "\\"
        out.append(f"  {first}{body}{last}")
    return "\n".join(out) + "\n"


def enum_decl(name, doc, values, deriving):
    out = [f"-- | {doc}", f"data {name}"]
    for i, (ucd, con, cdoc) in enumerate(values):
        lead = "=" if i == 0 else "|"
        out.append(f"  {lead} -- | {cdoc} (@{ucd}@)")
        out.append(f"    {con}")
    out.append(f"  deriving stock ({deriving})")
    return "\n".join(out) + "\n"


def decode(name, typ, values, field, doc):
    out = [f"-- | {doc}", f"{name} :: Char -> {typ}", f"{name} c = case {field} (classOf c) of"]
    for i, (_, con, _) in enumerate(values):
        out.append(f"  {i} -> {con}")
    out.append(f"  _ -> {values[0][1]}")
    return "\n".join(out) + "\n"


def generate():
    props = load()
    keys, rows, cls = classes(props)
    stage1, stage2 = two_stage(cls, SHIFT)
    if max(stage1) > 0xFF or len(rows) > 0x100:
        sys.exit("gen-unicode-tables: a stage no longer fits in a byte; widen it")
    col = {k: [r[i] for r in rows] for i, k in enumerate(keys)}
    flags = [
        e | (v << 1) | (z << 2) | (m << 3) | (b << 4)
        for e, v, z, m, b in zip(
            col["extendedPictographic"],
            col["emojiVariationBase"],
            col["zeroWidthInCluster"],
            col["emojiModifier"],
            col["emojiModifierBase"],
            strict=True,
        )
    ]

    hs = f"""\
-- | Unicode {UCD_VERSION}'s character properties that segmentation and width
-- read (DESIGN.md §5.3).
--
-- __Generated__ by @scripts/gen-unicode-tables.py@ from the UCD at the version
-- it pins, which is libghostty-vt's. Do not edit; regenerate, and CI's
-- @--check@ fails if this file is not what the pinned UCD generates.
--
-- The tables cover every code point, 0 to 10FFFF, so every lookup is a
-- table's answer; the fallbacks that keep the lookups total are unreachable.
module Tendril.Text.Unicode.Tables
  ( unicodeVersion,
    GraphemeBreak (..),
    graphemeBreak,
    IndicConjunctBreak (..),
    indicConjunctBreak,
    extendedPictographic,
    emojiVariationBase,
    width,
    zeroWidthInCluster,
    emojiModifier,
    emojiModifierBase,
  )
where

import Data.Bits (shiftR, testBit, (.&.))
import Data.ByteString qualified as BS

-- | The version of the Unicode Character Database these tables are from.
--
-- >>> unicodeVersion
-- "{UCD_VERSION}"
unicodeVersion :: Text
unicodeVersion = "{UCD_VERSION}"

{enum_decl("GraphemeBreak", "A code point's @Grapheme_Cluster_Break@ (UAX #29).", GCB, "Eq, Ord, Show, Enum, Bounded")}
{decode("graphemeBreak", "GraphemeBreak", GCB, "graphemeBreakOf", "A code point's @Grapheme_Cluster_Break@.")}
{enum_decl("IndicConjunctBreak", "A code point's @Indic_Conjunct_Break@ (UAX #44), which UAX #29's rule GB9c reads.", INCB, "Eq, Ord, Show, Enum, Bounded")}
{decode("indicConjunctBreak", "IndicConjunctBreak", INCB, "incbOf", "A code point's @Indic_Conjunct_Break@.")}
-- | Whether a code point is @Extended_Pictographic@ (UTS #51), which UAX #29's
-- rule GB11 reads.
--
-- >>> map extendedPictographic "a\\x1F600\\x00A9"
-- [False,True,True]
extendedPictographic :: Char -> Bool
extendedPictographic c = testBit (flagsOf (classOf c)) 0

-- | Whether a code point is the base of an emoji variation sequence
-- (@emoji-variation-sequences.txt@), so that a VS15 or VS16 after it chooses
-- text or emoji presentation, and with that the cluster's width.
--
-- >>> map emojiVariationBase "\\x231B\\x1F46C\\&a"
-- [True,False,False]
emojiVariationBase :: Char -> Bool
emojiVariationBase c = testBit (flagsOf (classOf c)) 1

-- | The cells a code point takes, as libghostty-vt computes it: 0, 1 or 2.
-- The rule is in the generator's header; a probe held it against
-- libghostty's for every code point, and M2.3's test keeps it there.
--
-- >>> map width "a\\x6F22\\x0301\\x1F600\\x0007"
-- [1,2,0,2,0]
width :: Char -> Int
width c = widthOf (classOf c)

-- | Whether a code point adds no width when it continues a grapheme cluster
-- (libghostty-vt's @width_zero_in_grapheme@): a zero-width code point, a
-- nonspacing or enclosing mark, a Hangul V or T jamo, a prepend, or an emoji
-- modifier. Any other code point after a cluster's first makes it two cells
-- wide, in the terminal's grapheme-cluster mode (2027).
--
-- >>> map zeroWidthInCluster "a\\x0301\\x1F3FB\\x6F22"
-- [False,True,True,False]
zeroWidthInCluster :: Char -> Bool
zeroWidthInCluster c = testBit (flagsOf (classOf c)) 2

-- | Whether a code point is an @Emoji_Modifier@ (UTS #51): the five skin
-- tones. UAX #29 makes it @Extend@, but libghostty-vt continues a cluster
-- with one only straight after an 'emojiModifierBase', as UTS #51's ED-13
-- says, and so does "Tendril.Text.Grapheme".
--
-- >>> map emojiModifier "\\x1F3FB\\x1F44D\\&a"
-- [True,False,False]
emojiModifier :: Char -> Bool
emojiModifier c = testBit (flagsOf (classOf c)) 3

-- | Whether a code point is an @Emoji_Modifier_Base@ (UTS #51), which an
-- 'emojiModifier' may follow in one cluster.
--
-- >>> map emojiModifierBase "\\x1F44D\\x1F600\\&a"
-- [True,False,False]
emojiModifierBase :: Char -> Bool
emojiModifierBase c = testBit (flagsOf (classOf c)) 4

-- A code point's class: stage 1 maps its block (the code point shifted right
-- by {SHIFT}) to a block number, and stage 2 the block's code points to their
-- classes. Each class has one value of each property.
classOf :: Char -> Int
classOf c = fromMaybe 0 $ do
  let cp = ord c
  block <- BS.indexMaybe stage1 (cp `shiftR` {SHIFT})
  cls <- BS.indexMaybe stage2 (fromIntegral block * {1 << SHIFT} + cp .&. {(1 << SHIFT) - 1})
  pure (fromIntegral cls)

byteAt :: ByteString -> Int -> Int
byteAt bs i = maybe 0 fromIntegral (BS.indexMaybe bs i)

graphemeBreakOf, incbOf, widthOf, flagsOf :: Int -> Int
graphemeBreakOf = byteAt classGraphemeBreak
incbOf = byteAt classIncb
widthOf = byteAt classWidth
flagsOf = byteAt classFlags

{literal("stage1", stage1, f"Stage 1: each block of {1 << SHIFT} code points' block number.")}
{literal("stage2", stage2, "Stage 2: the blocks, each code point's class.")}
{literal("classGraphemeBreak", col["graphemeBreak"], "Each class's 'GraphemeBreak', by constructor index.")}
{literal("classIncb", col["indicConjunctBreak"], "Each class's 'IndicConjunctBreak', by constructor index.")}
{literal("classWidth", col["width"], "Each class's width.")}
{literal("classFlags", flags, "Each class's flags: bit 0 'extendedPictographic', bit 1 'emojiVariationBase', bit 2 'zeroWidthInCluster', bit 3 'emojiModifier', bit 4 'emojiModifierBase'.")}"""

    items = [f'("{k}", 0x{fnv1a(props[k]):016x})' for k in keys]
    digests = "  [ " + ",\n    ".join(items) + "\n  ]"
    expected = f"""\
-- | __Generated__ by @scripts/gen-unicode-tables.py@ with
-- "Tendril.Text.Unicode.Tables": what the tables say, digested, so that the
-- test suite checks what GHC decoded from the table literals, not only their
-- text.
module Test.Tendril.Text.Unicode.Expected (digests) where

-- | Each lookup's 64-bit FNV-1a digest over code points 0 to 10FFFF, one
-- byte per code point: a constructor's index, a width, or 0 or 1.
digests :: [(Text, Word64)]
digests =
{digests}
"""
    return hs, expected, conformance()


def conformance():
    """`Test.Tendril.Text.Grapheme.Conformance`: each case of
    `GraphemeBreakTest.txt` as its line number and its clusters."""
    items = []
    for n, line in enumerate(fetch("auxiliary/GraphemeBreakTest.txt").splitlines(), 1):
        line = line.split("#", 1)[0].strip()
        if not line:
            continue
        # "÷ 0061 × 0308 ÷ 0062 ÷": a break at each ÷, none at each ×.
        clusters = [c.replace("×", " ").split() for c in line.strip("÷ ").split("÷")]
        hs = ", ".join("[" + ", ".join(f"'\\x{cp}'" for cp in cl) + "]" for cl in clusters)
        items.append(f"({n}, [{hs}])")
    body = "  [ " + ",\n    ".join(items) + "\n  ]"
    return f"""\
-- | __Generated__ by @scripts/gen-unicode-tables.py@ from the UCD's
-- @GraphemeBreakTest.txt@ at {UCD_VERSION}: UAX #29's conformance cases, which
-- "Tendril.Text.Grapheme" must segment as they do.
module Test.Tendril.Text.Grapheme.Conformance (cases) where

-- | Each case's line in the file, and its grapheme clusters in order.
cases :: [(Int, [[Char]])]
cases =
{body}
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--check", action="store_true")
    ap.add_argument("--sizes", action="store_true", help="print the table size for each block shift")
    args = ap.parse_args()

    if args.sizes:
        _, _, cls = classes(load())
        for shift in range(4, 11):
            s1, s2 = two_stage(cls, shift)
            print(f"shift {shift}: stage1 {len(s1)} (max {max(s1)}) + stage2 {len(s2)} = {len(s1) + len(s2)}")
        return 0

    hs, expected, cases = generate()
    stale = []
    for path, text in ((OUT, hs), (OUT_EXPECTED, expected), (OUT_CONFORMANCE, cases)):
        old = path.read_text() if path.exists() else ""
        if old == text:
            continue
        if args.check:
            stale.append(path)
            sys.stdout.writelines(
                difflib.unified_diff(
                    old.splitlines(True), text.splitlines(True), str(path), "generated", n=1
                )
            )
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text)
            print(f"wrote {path}", file=sys.stderr)
    if stale:
        print(
            f"gen-unicode-tables: {', '.join(map(str, stale))} not current; "
            "run python3 scripts/gen-unicode-tables.py",
            file=sys.stderr,
        )
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
