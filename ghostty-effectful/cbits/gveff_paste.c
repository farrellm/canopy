/* See gveff_paste.h. */
#include "gveff_paste.h"

bool gveff_paste_reader(void *userdata, GhosttyString mime, GhosttyWriter writer) {
  (void)mime;
  const GhosttyString *text = userdata;
  /* A writer is never given an empty slice. */
  return text->len == 0 || writer.write(writer.userdata, text->ptr, text->len);
}
