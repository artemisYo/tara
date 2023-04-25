#ifndef STRC
#define STRC
#include <string.h>
#include <stdlib.h>
typedef struct {
  char* str;
  unsigned long int len;
} Str;

static Str str_make(char* str) {
  Str out = {
    str,
    strlen(str),
  };
  return out;
}

static void str_drop(Str str) {
  free(str.str);
}

const struct {
  Str (*make)(char*);
  void (*drop)(Str);
} String = {
    str_make,
    str_drop,
};
#endif
