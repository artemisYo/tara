#include "tokenizer.c"
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#define assert_token(a, b)                                                     \
  if (a.type != b) {                                                           \
    printf("[%s : %i] assert_token failed: expected [%s] found [%s]\n",        \
           __FILE__, __LINE__, type_to_name(b), type_to_name(a.type));         \
    exit(1);                                                                   \
  }
#define assert_op(t, s)                                                        \
  if (strncmp(t.location, s, strlen(s)) == 0) {                                \
    printf("[%s : %i] assert_op failed: expected [%s] found [%.*s]\n",         \
           __FILE__, __LINE__, s, (int)strlen(s), t.location);                 \
    exit(1);                                                                   \
  }

typedef struct ASLeaf {
  const char *location;
  int len;
} ASLeaf;

typedef struct ASTree {
  enum { Root, Function, Params, Var } type;
  union {
    struct ASTree *node;
    struct ASLeaf leaf;
  } *next;
} ASTree;

ASTree parse_params(TokenReader *input) {
  ASTree out = {Params, NULL};
  assert_token(TReader.get_token(input), OpPar);
  TReader.step(input);
  Token *params = calloc(sizeof(Token), 10);
  int params_cap = 10, params_count = 0;
  while (TReader.get_token(input).type != ClPar) {
    Token name = TReader.get_token(input);
    assert_token(name, Ident);
    TReader.step(input);
    assert_op(TReader.get_token(input), ":");
    TReader.step(input);
    // parse monomorphic bare type
    // TODO: Extend this to also support
    // parametric polymorphism and pointers
    Token type = TReader.get_token(input);
    assert_token(type, Ident);
    TReader.step(input);
    if (params_count + 1 >= params_cap) {
      params_cap *= 1.2;
      params_cap += 2;
      params = realloc(params, sizeof(Token) * params_cap);
    }
    params[params_count] = name;
    params_count++;
    params[params_count] = type;
    params_count++;
    // if parameters ended, do not assert a comma
    if (TReader.get_token(input).type == ClPar)
      break;
    assert_op(TReader.get_token(input), ",");
  }
  assert_token(TReader.get_token(input), ClPar);
  TReader.step(input);
  out.next = calloc(sizeof(long), params_count / 2);
  for (int i = 0; i < params_count; i += 2) {
    ASTree var = {Var, calloc(sizeof(long), 2)};
    var.next[0].leaf = (ASLeaf){params[i].location, params[i].len};
    var.next[1].leaf = (ASLeaf){params[i + 1].location, params[i + 1].len};
    out.next[i / 2].node = malloc(sizeof(ASTree));
    *out.next[i / 2].node = var;
  }
  return out;
}
ASTree parse_return_type(TokenReader *input) {}
ASTree parse_block(TokenReader *input) {}

ASTree parse_fn(TokenReader *input) {
  ASTree out = {Function, calloc(sizeof(long), 4)};
  Token name = TReader.get_token(input);
  assert_token(name, Ident);
  out.next[0].leaf = (ASLeaf){name.location, name.len};
  TReader.step(input);
  ASTree params = parse_params(input);
  out.next[1].node = malloc(sizeof(ASTree));
  *out.next[1].node = params;
  ASTree return_type = parse_return_type(input);
  out.next[2].node = malloc(sizeof(ASTree));
  *out.next[2].node = return_type;
  assert_op(TReader.get_token(input), "->");
  ASTree block = parse_block(input);
  out.next[3].node = malloc(sizeof(ASTree));
  *out.next[3].node = block;
  return out;
}

ASTree parse_stream(TokenStream input) {
  TokenReader stream = TReader.make(&input);
  ASTree *nodes = calloc(sizeof(ASTree), 10);
  int nodes_count = 0, nodes_cap = 0;
  switch (TReader.get_token(&stream).type) {
  case Fn:
    TReader.step(&stream);
    if (nodes_count >= nodes_cap) {
      nodes_cap *= 1.2;
      nodes_cap++;
      nodes = realloc(nodes, sizeof(ASTree) * nodes_cap);
    }
    nodes[nodes_count] = parse_fn(&stream);
    nodes_count++;
    break;
  default:
    break;
  }
  ASTree out = {Root, calloc(sizeof(long), nodes_count)};
  for (int i = 0; i < nodes_count; i++) {
    out.next[i].node = malloc(sizeof(ASTree));
    *out.next[i].node = nodes[i];
  }
  return out;
}

int main() {
  char input[] =
      "operator +.\nfn main(args: Vec[String]): Int "
      "->\n\tString.parse_num(args[0])\n\t+ String.parse_num(args[1]).";
  TokenStream res = tokenize_run(input);
  TokenReader reader = TReader.make(&res);
  reader.head--;
  printf("%s", input);
  while (TReader.step(&reader)) {
    Token cur = TReader.get_token(&reader);
    printf("[%s]: %.*s, %i\n", type_to_name(cur.type), cur.len, cur.location,
           cur.len);
  }
  printf("----\n");
  parse_stream(res);
  free(res.stream);
  return 0;
}
