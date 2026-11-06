# canopy-prelude

One module, `Canopy.Prelude`: relude, minus the names that collide with `effectful` (`DESIGN.md`
§2.3). Every stanza of every other package builds against it in place of `base`'s `Prelude`.

## Wiring

Each consuming stanza carries the same two `mixins` lines, with the bare package name in both
`build-depends` and `mixins`:

```cabal
  build-depends:    base, canopy-prelude, ...
  mixins:
    base hiding (Prelude),
    canopy-prelude (Canopy.Prelude as Prelude)
```

This package itself depends on `relude` alone, through `relude (Relude as Prelude)` and a bare
`relude`.

## Traps

- **relude re-exports mtl's `State`/`Reader` vocabulary** (`get`, `put`, `ask`, `local`, …), which
  collides name-for-name with `effectful`. That is resolved here once by subtraction, not per module
  by qualification. A future collision (with `optics`, say) is subtracted here too.
- **relude withholds `unsafePerformIO`, which is a feature.** It makes the unsafety audit a `grep`
  (§10.6).
- `scripts/check-haddock.py` excludes `Canopy.Prelude`, whose exports are relude's documentation.
