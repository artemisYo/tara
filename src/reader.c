#ifndef READERC
#define READERC
#include "str.c"
typedef struct {
  Str* backing;
  unsigned long int head;
} StrReader;

static unsigned long int reader_position(StrReader *in) { return in->head; }
static unsigned long int reader_underlying_len(StrReader *in) {
  return in->backing->len;
}
static void increment(StrReader *in) {
  if (in->head < in->backing->len) {
    in->head++;
  }
}
static void shift(StrReader *in, int n) {
  if (in->head+n <= in->backing->len) {
    in->head += n;
  }
}
static StrReader reader_make(Str* owned) {
  StrReader out = {
    owned,
    0
  };
  return out;
}
static const char *reader_deref(StrReader *in) {
  if (in->head < in->backing->len) {
    return in->backing->str + in->head;
  }
  return in->backing->str + in->head-1;
}
static char reader_char(StrReader *in) {
  if (in->head < in->backing->len) {
    return *(in->backing->str + in->head);
  }
  return *(in->backing->str + in->head-1);
}

const struct {
  unsigned long int (*position)(StrReader*);
  unsigned long int (*length)(StrReader*);
  void (*increment)(StrReader*);
  void (*shift)(StrReader*, int);
  StrReader (*make)(Str*);
  const char* (*deref)(StrReader*);
  char (*get_char)(StrReader*);
} Reader = {
    reader_position, reader_underlying_len, increment,   shift,
    reader_make,     reader_deref,          reader_char,
};
#endif
