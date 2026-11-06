/*
 * gveff_answers.h: the two terminal callbacks ghostty-effectful answers in C.
 *
 * ENQUIRY and XTVERSION return a GhosttyString by value, which no Haskell
 * `wrapper` can (DESIGN.md §3.3, §4.5). Each answers from the terminal's
 * USERDATA, an array of two GhosttyStrings that Haskell owns and rewrites when
 * the answer is set: [0] the ENQ reply, [1] the XTVERSION name. Neither runs
 * any Haskell.
 */
#ifndef GVEFF_ANSWERS_H
#define GVEFF_ANSWERS_H

#include <ghostty/vt.h>

/* How many strings the USERDATA array holds, and which is which. */
#define GVEFF_ANSWER_ENQUIRY 0
#define GVEFF_ANSWER_XTVERSION 1
#define GVEFF_ANSWER_COUNT 2

GhosttyString gveff_enquiry(GhosttyTerminal terminal, void *userdata);
GhosttyString gveff_xtversion(GhosttyTerminal terminal, void *userdata);

#endif
