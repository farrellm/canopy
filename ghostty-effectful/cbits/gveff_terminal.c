/* See gveff_terminal.h. */
#include "gveff_terminal.h"

#include <stdlib.h>

_Static_assert(offsetof(GveffUserdata, answers) == 0,
               "answers must be first: Haskell reads them through the USERDATA pointer");

GveffUserdata *gveff_userdata_new(void) {
  GveffUserdata *ud = calloc(1, sizeof *ud);
  if (ud != NULL) ud->failure = GHOSTTY_SUCCESS;
  return ud;
}

void gveff_userdata_free(GveffUserdata *ud) {
  if (ud == NULL) return;
  free(ud->captures);
  free(ud);
}

void gveff_set_hold_notify(GveffUserdata *ud, GhosttyTerminalRenderHoldFn notify) {
  ud->notify = notify;
}

bool gveff_capture_attach(GveffUserdata *ud, GhosttyRenderState state) {
  if (ud->count == ud->capacity) {
    size_t capacity = ud->capacity == 0 ? 2 : ud->capacity * 2;
    GveffCapture *grown = realloc(ud->captures, capacity * sizeof *grown);
    if (grown == NULL) return false;
    ud->captures = grown;
    ud->capacity = capacity;
  }
  ud->captures[ud->count++] = (GveffCapture){.state = state, .held = false, .framing = false};
  return true;
}

void gveff_capture_detach(GveffUserdata *ud, GhosttyRenderState state) {
  for (size_t i = 0; i < ud->count; i++) {
    if (ud->captures[i].state == state) {
      ud->captures[i] = ud->captures[--ud->count];
      return;
    }
  }
}

bool gveff_capture_held(const GveffUserdata *ud, GhosttyRenderState state) {
  for (size_t i = 0; i < ud->count; i++)
    if (ud->captures[i].state == state) return ud->captures[i].held;
  return false;
}

void gveff_capture_release(GveffUserdata *ud) {
  for (size_t i = 0; i < ud->count; i++) ud->captures[i].held = false;
}

void gveff_capture_framing(GveffUserdata *ud, GhosttyRenderState state, bool framing) {
  for (size_t i = 0; i < ud->count; i++)
    if (ud->captures[i].state == state) ud->captures[i].framing = framing;
}

GhosttyResult gveff_capture_failure(GveffUserdata *ud) {
  GhosttyResult r = ud->failure;
  ud->failure = GHOSTTY_SUCCESS;
  return r;
}

GhosttyString gveff_enquiry(GhosttyTerminal terminal, void *userdata) {
  (void)terminal;
  return ((const GveffUserdata *)userdata)->answers[GVEFF_ANSWER_ENQUIRY];
}

GhosttyString gveff_xtversion(GhosttyTerminal terminal, void *userdata) {
  (void)terminal;
  return ((const GveffUserdata *)userdata)->answers[GVEFF_ANSWER_XTVERSION];
}

/* The header's recipe: when a hold begins, the terminal holds exactly the
 * frame the program wants left on screen, so update each render state from it
 * now. A render state whose frame is open is skipped, since its rows are
 * being read: it goes on showing that frame, and updates live after. A render
 * state whose update fails is left unheld, so its next frame
 * shows the terminal live rather than a partial capture. When the hold ends,
 * every render state goes back to updating. */
void gveff_render_hold(GhosttyTerminal terminal, void *userdata, bool held) {
  GveffUserdata *ud = userdata;
  for (size_t i = 0; i < ud->count; i++) {
    GveffCapture *c = &ud->captures[i];
    if (!held) {
      c->held = false;
      continue;
    }
    if (c->framing) continue;
    GhosttyResult r = ghostty_render_state_update(c->state, terminal);
    c->held = r == GHOSTTY_SUCCESS;
    if (r != GHOSTTY_SUCCESS && ud->failure == GHOSTTY_SUCCESS) ud->failure = r;
  }
  if (ud->notify != NULL) ud->notify(terminal, userdata, held);
}
