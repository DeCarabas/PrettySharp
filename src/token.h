#ifndef TOKEN_H
#define TOKEN_H

#include "common.h"

enum TokenType {
#define TKN(id, txt, isid) TOKEN_##id,
#include "token.inc"
#undef TKN
};

struct Token {
  enum TokenType type;
  const char *start;
  int length;
  int line;
};

const char *token_text(enum TokenType type);
bool is_identifier_token(enum TokenType type);
enum TokenType keyword_type(const char *start, int len);

#endif
