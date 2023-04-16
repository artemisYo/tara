#ifndef STRC
#define STRC
#include <string.h>
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

const struct {
  Str (*make)(char*);
} String = {
    str_make,
};
#endif
