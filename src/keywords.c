#include "keywords.h"

#define KEYWORD(x) static struct Keyword _kw_##x = {#x, sizeof(#x) - 1};
#include "keywords.inc"
#undef KEYWORD

#define KEYWORD(x) struct Keyword *kw_##x = &_kw_##x;
#include "keywords.inc"
#undef KEYWORD
