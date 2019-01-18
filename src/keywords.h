#ifndef KEYWORD_H
#define KEYWORD_H

#include "lexer.h"

struct Keyword {
  const char *text;
  const int length;
};

#define KEYWORD(x) extern struct Keyword *kw_##x;
#include "keywords.inc"
#undef KEYWORD

#endif
