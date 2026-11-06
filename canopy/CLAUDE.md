# canopy

The application (`DESIGN.md` §6–§9): the library `Canopy.*`, the `canopy` executable, `canopy-test`,
and `canopy-e2e`, which runs nightly against real REPLs and is empty until M3. The pure core
(`Canopy.App.{Model,Update,Event,Command}`) may not import the IO subsystems (§2.6 rule 4). M0 is a
skeleton: `canopy` prints each component's version, which exercises every package edge.
