/*
 * gvtffi_shims.h: what ghostty-vt-ffi needs from C besides the library's own
 * headers (DESIGN.md §3.3).
 *
 * GHC's FFI cannot pass a struct by value, and five libghostty-vt functions
 * take one. Each gets a static inline shim here that takes a pointer instead
 * and passes the struct on unchanged. The bindings call them with
 * `foreign import capi`, so the C compiler still checks each Haskell signature
 * against these prototypes, and these against the library's.
 *
 * The shims are written by hand: there are five, against tree-sitter's 56
 * (§5A.2). A new by-value function at a pin bump fails to compile where it is
 * bound, since capi checks the prototype.
 */

#ifndef GVTFFI_SHIMS_H
#define GVTFFI_SHIMS_H

#include <ghostty/vt.h>

/* hsc2hs helpers: a field's size, and the length of an array field. */
#define GVTFFI_FIELD_SIZE(t, f) sizeof(((t *)0)->f)
#define GVTFFI_ARRAY_LEN(t, f) (sizeof(((t *)0)->f) / sizeof(((t *)0)->f[0]))

static inline void gvtffi_terminal_scroll_viewport(
    GhosttyTerminal terminal,
    const GhosttyTerminalScrollViewport *behavior) {
  ghostty_terminal_scroll_viewport(terminal, *behavior);
}

static inline GhosttyResult gvtffi_terminal_grid_ref(
    GhosttyTerminal terminal,
    const GhosttyPoint *point,
    GhosttyGridRef *out_ref) {
  return ghostty_terminal_grid_ref(terminal, *point, out_ref);
}

static inline GhosttyResult gvtffi_terminal_grid_ref_track(
    GhosttyTerminal terminal,
    const GhosttyPoint *point,
    GhosttyTrackedGridRef *out_ref) {
  return ghostty_terminal_grid_ref_track(terminal, *point, out_ref);
}

static inline GhosttyResult gvtffi_tracked_grid_ref_set(
    GhosttyTrackedGridRef ref,
    GhosttyTerminal terminal,
    const GhosttyPoint *point) {
  return ghostty_tracked_grid_ref_set(ref, terminal, *point);
}

static inline GhosttyResult gvtffi_terminal_continuation_write(
    GhosttyTerminal terminal,
    const GhosttyWriter *writer) {
  return ghostty_terminal_continuation_write(terminal, *writer);
}

#endif /* GVTFFI_SHIMS_H */
