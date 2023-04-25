#ifndef TOKENSC
#define TOKENSC
typedef enum {
  Ident = 0,
  //--Whitespace start
  Space = 1,
  Tab,
  LF,
  //--Whitespace end
  //--Delimiters start
  OpPar,
  ClPar,
  OpBrace,
  ClBrace,
  OpBrack,
  ClBrack,
  OpKet,
  ClKet,
  //--Delimiters end
  Operator,
  //--Keywords start
  Fn,
  //--Keywords end
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
  const char* location;
  int len;
} Token;

typedef struct {
  Token *stream;
  int len;
  int cap;
} TokenStream;

typedef struct {
  const TokenStream* backing;
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
static int tokenreader_step(TokenReader* in) {
  if (in->head < in->backing->len - 1) {
    in->head++;
    return 1;
  }
  return 0;
}
static struct {
  TokenReader (*make)(TokenStream*);
  Token (*get_token)(TokenReader*);
  int (*step)(TokenReader*);
} TReader = {
  tokenreader_make,
  tokenreader_token,
  tokenreader_step,
};
#endif
