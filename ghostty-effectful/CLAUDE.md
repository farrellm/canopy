# ghostty-effectful

The safe, `effectful` interface to libghostty-vt (`DESIGN.md` §4): scoped handles, typed keys, and
events instead of callbacks. No export may expose a pointer. M0 is a skeleton (`buildMetadata`). The
interface arrives in M1.

Its doctest runs the real library, so `LD_LIBRARY_PATH` must come from `build-libghostty.sh`.
