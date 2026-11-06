/*
 * gveff_terminal.h: what ghostty-effectful does for a terminal in C, and the
 * terminal's USERDATA, which that C reads.
 *
 * ENQUIRY and XTVERSION return a GhosttyString by value, which no Haskell
 * `wrapper` can (DESIGN.md §3.3, §4.5). Each answers from the USERDATA's
 * `answers`, two GhosttyStrings that Haskell owns and rewrites when the answer
 * is set: [0] the ENQ reply, [1] the XTVERSION name.
 *
 * RENDER_HOLD captures the frame a program wants left on screen: when a hold
 * begins, the header asks for each render state to be updated inside the
 * callback, before the rest of the write is processed (DESIGN.md §4.3). That
 * is a C call, so the callback is C: it updates every render state attached to
 * the terminal, marks each held, and then calls `notify`, the Haskell callback
 * that appends the event. A capture that fails is recorded in `failure` for
 * `notify` to report. None of this runs Haskell beyond `notify`.
 *
 * Every function here is called between writes, except the two callbacks, so
 * nothing here locks: libghostty-vt's handles are used from one thread (§4.6).
 */
#ifndef GVEFF_TERMINAL_H
#define GVEFF_TERMINAL_H

#include <stdbool.h>
#include <stddef.h>
#include <ghostty/vt.h>

/* How many strings `answers` holds, and which is which. */
#define GVEFF_ANSWER_ENQUIRY 0
#define GVEFF_ANSWER_XTVERSION 1
#define GVEFF_ANSWER_COUNT 2

/* A render state attached to a terminal, whether it holds a captured frame,
 * and whether a frame of it is open, which a capture must not update under. */
typedef struct {
  GhosttyRenderState state;
  bool held;
  bool framing;
} GveffCapture;

/* A terminal's USERDATA. `answers` is first, so a pointer to the struct is a
 * pointer to the array. */
typedef struct {
  GhosttyString answers[GVEFF_ANSWER_COUNT];
  GhosttyTerminalRenderHoldFn notify;
  GveffCapture *captures;
  size_t count;
  size_t capacity;
  GhosttyResult failure;
} GveffUserdata;

/* A zeroed USERDATA, or NULL if it cannot be allocated. */
GveffUserdata *gveff_userdata_new(void);

/* Free the USERDATA and its capture list, not the answers' bytes, which are
 * Haskell's. NULL is ignored. */
void gveff_userdata_free(GveffUserdata *ud);

/* Set the Haskell callback a hold reports to. */
void gveff_set_hold_notify(GveffUserdata *ud, GhosttyTerminalRenderHoldFn notify);

/* Capture frames into a render state from now on, unheld; false if the list
 * cannot grow. */
bool gveff_capture_attach(GveffUserdata *ud, GhosttyRenderState state);

/* Stop capturing into a render state. One not attached is ignored. */
void gveff_capture_detach(GveffUserdata *ud, GhosttyRenderState state);

/* Whether a render state holds a frame captured when a hold began, and the
 * hold has not ended since. */
bool gveff_capture_held(const GveffUserdata *ud, GhosttyRenderState state);

/* Drop a render state's captured frame, for a hold its owner ended itself. */
void gveff_capture_release(GveffUserdata *ud, GhosttyRenderState state);

/* Mark a frame of the render state open or closed. While one is open, a hold
 * that begins does not capture into it: its rows are being read, and an update
 * would invalidate them. */
void gveff_capture_framing(GveffUserdata *ud, GhosttyRenderState state, bool framing);

/* The first capture that failed since this was last called, or
 * GHOSTTY_SUCCESS; and reset it. */
GhosttyResult gveff_capture_failure(GveffUserdata *ud);

/* The callbacks. */
GhosttyString gveff_enquiry(GhosttyTerminal terminal, void *userdata);
GhosttyString gveff_xtversion(GhosttyTerminal terminal, void *userdata);
void gveff_render_hold(GhosttyTerminal terminal, void *userdata, bool held);

#endif
