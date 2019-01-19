#ifndef LEXER_H
#define LEXER_H

#include "token.h"

void lexer_init(const char *source);
struct Token scan_token();

#endif
