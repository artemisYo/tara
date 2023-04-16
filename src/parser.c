#include <stdlib.h>
#include "tokenizer.c"

// TODO:
//   1. Actually write all of the boilerplate needed to effectively write this
//   parser, as generics or ADTs don't exist here

typedef struct Derivations Deriv;
typedef struct Results Res;

typedef struct LazyInitFromToken {
  int descriminant;
  union {
    Deriv* (*f)(TokenReader);
    Deriv* (*g)(Deriv*);
  };
  union {
    TokenReader tr_input;
    Deriv* de_input;
  };
  Deriv* final;
} LazyF;
Deriv *get(LazyF *field) {
  if (field->final == NULL) {
    switch (field->descriminant) {
    case 0: field->final = field->f(field->tr_input);
      break;
    case 1: field->final = field->g(field->de_input);
      break;
    default: return NULL;
    }
  }
  return field->final;
}

struct Results {
  LazyF next;
  Token parsed;
};

struct Derivations{
  Res token;
};

Deriv* parse(TokenReader input);

Res parse_token(TokenReader input) {
  Res out = {
    .next = {
      .f = parse,
      .tr_input = TReader.push(input),
      .final = NULL,
    },
    .parsed = TReader.get_token(&input),
  };
  return out;
}

Res parse_fn(Deriv *d) {
  if (d->token.parsed.type == Fn) {
    Res out = {
      .next = {.final = get(&d->token.next)},
      .parsed = d->token.parsed,
    };
    return out;
  }
}

Deriv* parse(TokenReader input) {
  Deriv* out = malloc(sizeof(Deriv));
  out->token = parse_token(input);
  return out;
}

int main() {
  TokenStream res = tokenize_run("operator -.\nfn main<>() {\n\tdamn - fuck\n}");
  printf("%s\n----\n", "operator -.\nfn main<>() {\n\tdamn - fuck\n}");
  for (int i = 0; i < res.len; i++) {
    Token *cur = &res.stream[i];
    printf("[%s]: %i, %i\n", type_to_name(cur->type), cur->location, cur->len);
  }
  printf("----\n");
  Deriv* d = parse(TReader.make(&res));
  Deriv* iter = d;
  while (iter->token.next.tr_input.head <= iter->token.next.tr_input.backing->len) {
    Token *cur = &iter->token.parsed;
    printf("[%s]: %i, %i\n", type_to_name(cur->type), cur->location, cur->len);
    iter = get(&iter->token.next);
  }
  free(res.stream);
  return 0;
}
