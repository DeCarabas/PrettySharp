#ifndef LEXER_H
#define LEXER_H

#include "token.h"

struct TokenBuffer {
  int count;
  struct Token *tokens;
};

struct TokenBuffer scan_tokens(const char *source);
void free_token_buffer(struct TokenBuffer *buffer);
void dump_lex(const char *source);

#endif
