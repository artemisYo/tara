#include "reader.c"
#include "str.c"
#include "tokens.c"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
char *strdupnw(const char *s, long int l) {
  long int h = 0;
  char *out = calloc(sizeof(char), l);
  for (long int i = 0; i < l; i++) {
    char c = s[i];
    if (c != ' ' && c != '\t' && c != '\n') {
      out[h] = c;
      h++;
    }
  }
  return out;
}

struct KeywTokenPair {
  char *str;
  int len;
  TokenType tok;
};

const struct KeywTokenPair keywords[KEYWCOUNT] = {
    {"fn", 2, Fn},
};

int keyw_find(const char *in) {
  for (int i = 0; i < KEYWCOUNT; i++) {
    if (strncmp(in, keywords[i].str, keywords[i].len) == 0) {
      return i;
    }
  }
  return -1;
}

struct PunctTokenPair {
  char punct;
  TokenType tok;
};

const struct PunctTokenPair punctuation[PUNCTCOUNT] = {
    {' ', Space},   {'\t', Tab},    {'\n', LF},     {'(', OpPar},
    {')', ClPar},   {'{', OpBrace}, {'}', ClBrace}, {'[', OpBrack},
    {']', ClBrack}, {'<', OpKet},   {'>', ClKet}};

#define IGNORECOUNT 3
const TokenType ignore[IGNORECOUNT] = {
    Space,
    Tab,
    LF,
};

int ignore_find(TokenType in) {
  for (int i = 0; i < IGNORECOUNT; i++) {
    if (in == ignore[i]) {
      return i;
    }
  }
  return -1;
}

int punct_find(char in) {
  for (int i = 0; i < PUNCTCOUNT; i++) {
    if (in == punctuation[i].punct) {
      return i;
    }
  }
  return -1;
}

#define POCOUNT 4
char *predefined_ops[POCOUNT] = {"->", ":", ".", ","};

Str *ops = NULL;
int ops_cap, ops_count;

void ops_init() {
  if (ops == NULL) {
    ops = calloc(sizeof(Str), POCOUNT + 1);
    for (int i = 0; i < POCOUNT; i++) {
      ops[i] = String.make(predefined_ops[i]);
    }
    ops_cap = POCOUNT + 1;
    ops_count = POCOUNT;
  }
}

void ops_deinit() {
  if (ops != NULL) {
    for (int i = POCOUNT; i < ops_count; i++) {
      String.drop(ops[i]);
    }
    free(ops);
    ops_cap = 0;
    ops_count = 0;
  }
}

int ops_find(const char *in) {
  for (int i = 0; i < ops_count; i++) {
    if (strncmp(in, ops[i].str, ops[i].len) == 0) {
      return i;
    }
  }
  return -1;
}

Token tokenize_punct(StrReader *input) {
  int i = punct_find(Reader.get_char(input));
  if (i > -1) {
    Token out = {
        punctuation[i].tok,
        Reader.deref(input),
        1,
    };
    return out;
  }
  Token out = {0, 0, 0};
  return out;
}

int tokenize_opreg(StrReader *input) {
  if (strncmp(Reader.deref(input), "operator", sizeof("operator") - 1) == 0) {
    long int l = strcspn(Reader.deref(input) + 8, ".");
    Str op = String.make(strdupnw(Reader.deref(input) + 8, l));

    if (ops_count >= ops_cap) {
      ops_cap *= 1.2;
      ops = realloc(ops, sizeof(Str) * ops_cap);
    }
    ops[ops_count] = op;
    ops_count++;
    return l + 9;
  }
  return 0;
}

Token tokenize_op(StrReader *input) {
  int i = ops_find(Reader.deref(input));
  if (i > -1) {
    Token out = {
        Operator,
        Reader.deref(input),
        ops[i].len,
    };
    return out;
  }
  Token out = {0, 0, 0};
  return out;
}

Token tokenize_keyword(StrReader *input) {
  int i = keyw_find(Reader.deref(input));
  if (i > -1) {
    int l = keywords[i].len;
    const char *post = Reader.deref(input) + l;
    if (punct_find(*post) > -1 || ops_find(post) > -1) {
      Token out = {
          keywords[i].tok,
          Reader.deref(input),
          l,
      };
      return out;
    }
  }
  Token out = {0, 0, 0};
  return out;
}

Token tokenize_ident(StrReader *input) {
  StrReader temp = *input;
  unsigned long int i = 0;
  for (; i < Reader.length(&temp); i++) {
    if (punct_find(Reader.get_char(&temp)) > -1 ||
        ops_find(Reader.deref(&temp)) > -1) {
      printf("Identifier with length [%lu]\n", i);
      break;
    }
    Reader.increment(&temp);
  }
  Token out = {
      Ident,
      Reader.deref(input),
      i,
  };
  return out;
}

TokenStream tokenize_run(char *input) {
  TokenStream out = {
      calloc(sizeof(Token), 50),
      0,
      50,
  };
  ops_init();
  Str s = String.make(input);
  StrReader reader = Reader.make(&s);
  Token (*funcs[4])(StrReader *) = {
      tokenize_punct,
      tokenize_op,
      tokenize_keyword,
      tokenize_ident,
  };
  while (Reader.position(&reader) < Reader.length(&reader)) {
    int l = tokenize_opreg(&reader);
    Reader.shift(&reader, l);
    for (int i = 0; i < 4; i++) {
      Token res = funcs[i](&reader);
      if (res.len > 0) {
        Reader.shift(&reader, res.len);
        if (ignore_find(res.type) == -1) {
          if (out.len >= out.cap) {
            out.cap *= 1.2;
            out.stream = realloc(out.stream, sizeof(Token) * out.cap);
          }
          out.stream[out.len] = res;
          out.len++;
        }
        break;
      }
    }
  }
  ops_deinit();
  return out;
}

TokenStream tokenize(char *input) { return tokenize_run(input); }

/* int main() { */
/*   TokenStream res = tokenize_run("operator -.\nfn main<>() {\n\tdamn -
 * fuck\n}"); */
/*   printf("%s\n----\n", "operator -.\nfn main<>() {\n\tdamn - fuck\n}"); */
/*   for (int i = 0; i < res.len; i++) { */
/*     Token *cur = &res.stream[i]; */
/*     printf("[%s]: %i, %i\n", type_to_name(cur->type), cur->location,
 * cur->len); */
/*   } */
/*   free(res.stream); */
/*   return 0; */
/* } */
