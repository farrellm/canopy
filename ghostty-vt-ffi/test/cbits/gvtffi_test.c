/*
 * gvtffi_test.c: C the ghostty-vt-ffi tests need and Haskell cannot be.
 *
 * A GhosttyMimeReaderFn receives a GhosttyString and a GhosttyWriter by value,
 * which no Haskell `wrapper` can take (DESIGN.md §3.3), so the paste test's
 * reader is this: it writes its userdata, a NUL-terminated string, to the
 * writer it is given.
 */

#include <string.h>

#include <ghostty/vt.h>

bool gvtffi_test_mime_reader(void *userdata, GhosttyString mime, GhosttyWriter writer) {
  (void)mime;
  const char *text = userdata;
  size_t len = strlen(text);
  return len == 0 || writer.write(writer.userdata, (const uint8_t *)text, len);
}
