/* See gveff_answers.h. */
#include "gveff_answers.h"

GhosttyString gveff_enquiry(GhosttyTerminal terminal, void *userdata) {
  (void)terminal;
  return ((const GhosttyString *)userdata)[GVEFF_ANSWER_ENQUIRY];
}

GhosttyString gveff_xtversion(GhosttyTerminal terminal, void *userdata) {
  (void)terminal;
  return ((const GhosttyString *)userdata)[GVEFF_ANSWER_XTVERSION];
}
