/*
 * gveff_paste.h: the MIME reader ghostty-effectful pastes through.
 *
 * A GhosttyMimeReaderFn receives a GhosttyString and a GhosttyWriter by value,
 * which no Haskell `wrapper` can take (DESIGN.md §3.3), so the reader is C. Its
 * userdata is a `const GhosttyString *`, the text to paste, which Haskell owns
 * for the duration of the ghostty_terminal_paste call. It reads nothing but
 * that, and runs no Haskell.
 */
#ifndef GVEFF_PASTE_H
#define GVEFF_PASTE_H

#include <stdbool.h>
#include <ghostty/vt.h>

/* Write the userdata's string to the writer; true unless the writer refuses. */
bool gveff_paste_reader(void *userdata, GhosttyString mime, GhosttyWriter writer);

#endif
