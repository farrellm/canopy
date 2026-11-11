/* See gveff_terminal.h. */
#include "gveff_terminal.h"

#include <stdlib.h>
#include <string.h>

_Static_assert(offsetof(GveffUserdata, answers) == 0,
               "answers must be first: Haskell reads them through the USERDATA pointer");

GveffUserdata *gveff_userdata_new(void) {
  GveffUserdata *ud = calloc(1, sizeof *ud);
  if (ud != NULL) {
    ud->failure = GHOSTTY_SUCCESS;
    ud->mark_failure = GHOSTTY_SUCCESS;
  }
  return ud;
}

void gveff_userdata_free(GveffUserdata *ud) {
  if (ud == NULL) return;
  gveff_marks_clear(ud);
  free(ud->marks);
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

bool gveff_marks_pending(const GveffUserdata *ud) {
  return ud->mark_count != 0 || ud->mark_failure != GHOSTTY_SUCCESS;
}

GhosttyTerminalSemanticPrompt *gveff_marks(GveffUserdata *ud) { return ud->marks; }

size_t gveff_mark_count(const GveffUserdata *ud) { return ud->mark_count; }

void gveff_marks_clear(GveffUserdata *ud) {
  for (size_t i = 0; i < ud->mark_count; i++) {
    free((void *)ud->marks[i].command.ptr);
    free((void *)ud->marks[i].error.ptr);
  }
  ud->mark_count = 0;
}

GhosttyResult gveff_marks_failure(GveffUserdata *ud) {
  GhosttyResult r = ud->mark_failure;
  ud->mark_failure = GHOSTTY_SUCCESS;
  return r;
}

/* A copy of a borrowed string, or an empty one; false if it cannot be made. */
static bool copy_string(GhosttyString *s) {
  if (s->len == 0) {
    *s = (GhosttyString){.ptr = NULL, .len = 0};
    return true;
  }
  uint8_t *p = malloc(s->len);
  if (p == NULL) return false;
  memcpy(p, s->ptr, s->len);
  s->ptr = p;
  return true;
}

/* Copy the event onto the marks. A copy that cannot be made drops the mark
 * and records the failure, which Haskell throws after the write. */
void gveff_semantic_prompt(GhosttyTerminal terminal, void *userdata,
                           const GhosttyTerminalSemanticPrompt *event) {
  (void)terminal;
  GveffUserdata *ud = userdata;
  if (ud->mark_count == ud->mark_capacity) {
    size_t capacity = ud->mark_capacity == 0 ? 4 : ud->mark_capacity * 2;
    GhosttyTerminalSemanticPrompt *grown = realloc(ud->marks, capacity * sizeof *grown);
    if (grown == NULL) goto out_of_memory;
    ud->marks = grown;
    ud->mark_capacity = capacity;
  }
  GhosttyTerminalSemanticPrompt m = *event;
  if (!copy_string(&m.command)) goto out_of_memory;
  if (!copy_string(&m.error)) {
    free((void *)m.command.ptr);
    goto out_of_memory;
  }
  ud->marks[ud->mark_count++] = m;
  return;
out_of_memory:
  if (ud->mark_failure == GHOSTTY_SUCCESS) ud->mark_failure = GHOSTTY_OUT_OF_MEMORY;
}
