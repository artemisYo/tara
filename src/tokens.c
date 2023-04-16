#ifndef TOKENSC
#define TOKENSC
typedef enum {
  Ident = 0,
  Space = 1,
  Tab,
  LF,
  OpPar,
  ClPar,
  OpBrace,
  ClBrace,
  OpBrack,
  ClBrack,
  OpKet,
  ClKet,
  Operator,
  Fn,
} TokenType;
#define PUNCTCOUNT (ClKet)
#define KEYWCOUNT (Fn - Operator)

#define NAME(TYPE) #TYPE

char *type_to_name(TokenType type) {
  static char *lookup[] = {
      NAME(Ident), NAME(Space),   NAME(Tab),      NAME(LF),      NAME(OpPar),
      NAME(ClPar), NAME(OpBrace), NAME(ClBrace),  NAME(OpBrack), NAME(ClBrack),
      NAME(OpKet), NAME(ClKet),   NAME(Operator), NAME(Fn),
  };
  return lookup[type];
}

// if field `len` is 0 the
// token produced is an error
typedef struct {
  TokenType type;
  int location;
  int len;
} Token;

typedef struct {
  Token *stream;
  int len;
  int cap;
} TokenStream;

typedef struct {
  TokenStream* backing;
  int head;
} TokenReader;

static TokenReader tokenreader_make(TokenStream* in) {
  TokenReader out = {
    in,
    0
  };
  return out;
}
static Token tokenreader_token(TokenReader* in) {
  if (in->head < in->backing->len) {
    return *(in->backing->stream + in->head);
  }
  return *(in->backing->stream + in->backing->len - 1);
}
static TokenReader tokenreader_push(TokenReader in) {
  in.head++;
  return in;
}
static struct {
  TokenReader (*make)(TokenStream*);
  Token (*get_token)(TokenReader*);
  TokenReader (*push)(TokenReader);
} TReader = {
  tokenreader_make,
  tokenreader_token,
  tokenreader_push,
};
#endif
