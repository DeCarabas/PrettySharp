#include "csharp.h"
#include "lexer.h"

typedef struct {
  struct DocBuilder *builder;
  struct Token previous;
  struct Token current;
  bool had_error;
  bool panic_mode;
} Parser;

Parser parser;

static void verror_at(struct Token *token, const char *format, va_list args) {
  if (parser.panic_mode) {
    return;
  }
  parser.panic_mode = true;
  parser.had_error = true;

  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": ");
  vfprintf(stderr, format, args);
  fprintf(stderr, "\n");
}

static void error_at(struct Token *token, const char *format, ...) {
  va_list args;
  va_start(args, format);
  verror_at(token, format, args);
  va_end(args);
}

static void error(const char *format, ...) {
  va_list args;
  va_start(args, format);
  verror_at(&parser.previous, format, args);
  va_end(args);
}

static void error_at_current(const char *message) {
  error_at(&parser.current, message);
}

static void advance() {
  parser.previous = parser.current;
  for (;;) {
    parser.current = scan_token();

    if (parser.current.type == TOKEN_TRIVIA_BLOCK_COMMENT ||
        parser.current.type == TOKEN_TRIVIA_EOL ||
        parser.current.type == TOKEN_TRIVIA_LINE_COMMENT ||
        parser.current.type == TOKEN_TRIVIA_WHITESPACE) {
      // TODO: Handle trivia properly.
      continue;
    }

    if (parser.current.type != TOKEN_ERROR) {
      break;
    }

    error_at_current(parser.current.start);
  }
}

static void consume(enum TokenType type, const char *message) {
  if (parser.current.type == type) {
    advance();
  } else {
    error_at_current(message);
  }
}

static bool peek(enum TokenType type) { return parser.current.type == type; }

static void group() { doc_group(parser.builder); }
static void end() { doc_end(parser.builder); }

static void space() { doc_text(parser.builder, " ", 1); }

static void token(enum TokenType type, const char *message) {
  // N.B.: Because advance() processes the interstitial trivia we need to echo
  // to the output as soon as we can. This might write garbage.
  doc_text(parser.builder, parser.current.start, parser.current.length);
  consume(type, message);
}

static void identifier() { token(TOKEN_IDENTIFIER, "Expected an identifier"); }

static void extern_alias() {
  group();

  token(TOKEN_KW_EXTERN, "Expected 'extern'");
  space();
  token(TOKEN_KW_ALIAS, "Expected 'alias'");
  space();
  identifier();
  token(TOKEN_SEMICOLON, "Expected a semicolon");

  end();
}

static void extern_aliases() {
  while (peek(TOKEN_KW_EXTERN)) {
    extern_alias();
  }
}

static void compilation_unit() { extern_aliases(); }

bool format_csharp(struct DocBuilder *builder, const char *source) {
  parser.builder = builder;
  parser.had_error = false;
  parser.panic_mode = false;

  lexer_init(source);
  advance();

  compilation_unit();
  consume(TOKEN_EOF, "Expect end of compilation unit");

  return !parser.had_error;
}
