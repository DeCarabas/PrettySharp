#include "csharp.h"
#include "keywords.h"
#include "lexer.h"

struct TokenBuffer {
  int head;
  int tail;
  int count;
  int capacity;
  struct Token *tokens;
};

void init_token_buffer(struct TokenBuffer *buffer) {
  buffer->head = 0;
  buffer->tail = 0;
  buffer->count = 0;
  buffer->capacity = 16;
  buffer->tokens = malloc(sizeof(struct Token) * buffer->capacity);
}

void add_buffered_token(struct TokenBuffer *buffer, struct Token token) {
  if (buffer->count == buffer->capacity) {
    int new_capacity = buffer->capacity + buffer->capacity / 2;

    struct Token *new_buffer = malloc(sizeof(struct Token) * new_capacity);

    memcpy(new_buffer, buffer->tokens + buffer->head,
           (buffer->capacity - buffer->head) * sizeof(struct Token));
    memcpy(new_buffer + (buffer->capacity - buffer->head), buffer->tokens,
           buffer->head * sizeof(struct Token));

    buffer->tokens = new_buffer;
    buffer->capacity = new_capacity;
    buffer->head = 0;
    buffer->tail = buffer->count;
  }

  buffer->tokens[buffer->tail] = token;
  buffer->tail = (buffer->tail + 1) % buffer->capacity;
  buffer->count++;
}

bool remove_buffered_token(struct TokenBuffer *buffer, struct Token *token) {
  if (buffer->count == 0) {
    return false;
  }

  *token = buffer->tokens[buffer->head];
  buffer->head = (buffer->head + 1) % buffer->capacity;
  buffer->count--;

  return true;
}

bool last_buffered_token(struct TokenBuffer *buffer, struct Token *token) {
  if (buffer->count == 0) {
    return false;
  }

  int last = buffer->tail ? buffer->tail - 1 : buffer->capacity - 1;
  *token = buffer->tokens[last];
  return true;
}

struct Parser {
  struct DocBuilder *builder;
  struct Token previous;
  struct Token current;
  struct TokenBuffer buffer;
  bool had_error;
  bool panic_mode;
};

struct Parser parser;

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

static void verror_at_current(const char *format, va_list args) {
  verror_at(&parser.previous, format, args);
}

static void error_at_current(const char *format, ...) {
  va_list args;
  va_start(args, format);
  verror_at_current(format, args);
  va_end(args);
}

static void advance() {
  parser.previous = parser.current;
  for (;;) {
    if (!remove_buffered_token(&parser.buffer, &parser.current)) {
      parser.current = scan_token();
    }

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

    error_at_current("%s", parser.current.start);
  }
}

static bool peek_and_return_token(enum TokenType type,
                                  struct Token *token_out) {
  struct Token token;
  if (!last_buffered_token(&parser.buffer, &token)) {
    for (;;) {
      token = scan_token();
      add_buffered_token(&parser.buffer, token);

      // N.B.: This logic here mirrors the logic in 'advance' with regards to
      // skipping trivia and errors.
      if (token.type == TOKEN_TRIVIA_BLOCK_COMMENT ||
          token.type == TOKEN_TRIVIA_EOL ||
          token.type == TOKEN_TRIVIA_LINE_COMMENT ||
          token.type == TOKEN_TRIVIA_WHITESPACE) {
        continue;
      }

      if (token.type != TOKEN_ERROR) {
        break;
      }
    }
  }

  if (token.type == type) {
    *token_out = token;
    return true;
  } else {
    return false;
  }
}

static bool peek(enum TokenType type) {
  struct Token token;
  return peek_and_return_token(type, &token);
}

static void token(enum TokenType type, const char *format, ...) {
  if (parser.current.type == type) {
    doc_text(parser.builder, parser.current.start, parser.current.length);
    advance();
  } else {
    va_list args;
    va_start(args, format);
    verror_at_current(format, args);
    va_end(args);
  }
}

static bool check(enum TokenType type) { return parser.current.type == type; }

static bool match(enum TokenType type) {
  if (!check(type)) {
    return false;
  }
  doc_text(parser.builder, parser.current.start, parser.current.length);
  advance();
  return true;
}

static bool check_keyword_token(struct Keyword *keyword, struct Token *token) {
  return (token->type == TOKEN_IDENTIFIER && token->length == keyword->length &&
          memcmp(token->start, keyword->text, keyword->length) == 0);
}

static bool check_keyword(struct Keyword *keyword) {
  return check_keyword_token(keyword, &parser.current);
}

static void keyword(struct Keyword *keyword) {
  if (check_keyword(keyword)) {
    doc_text(parser.builder, parser.current.start, parser.current.length);
    advance();
  } else {
    error_at_current("Expected '%s'", keyword->text);
  }
}

static bool match_keyword(struct Keyword *keyword) {
  if (!check_keyword(keyword)) {
    return false;
  }
  doc_text(parser.builder, parser.current.start, parser.current.length);
  advance();
  return true;
}

static bool peek_keyword(struct Keyword *keyword) {
  struct Token token;
  if (peek_and_return_token(TOKEN_IDENTIFIER, &token)) {
    if (check_keyword_token(keyword, &token)) {
      return true;
    }
  }
  return false;
}

static void group() { doc_group(parser.builder); }
static void end() { doc_end(parser.builder); }
static void line() { doc_line(parser.builder); }
static void indent() { doc_indent(parser.builder); }
static void dedent() { doc_dedent(parser.builder); }
static void space() { doc_text(parser.builder, " ", 1); }

static void identifier() { token(TOKEN_IDENTIFIER, "Expected an identifier"); }

static void qualified_name() {
  while (check(TOKEN_DOT) || check(TOKEN_COLON_COLON)) {
    //
  }
}

static void extern_alias() {
  keyword(kw_extern);
  space();
  keyword(kw_alias);
  space();
  identifier();
  token(TOKEN_SEMICOLON, "Expected extern alias to end with a semicolon");
}

static void extern_alias_directives() {
  if (check_keyword(kw_extern)) {
    while (check_keyword(kw_extern)) {
      extern_alias();
      line();
    }
    line();
  }
}

static void name_equals() {
  identifier();
  space();
  token(TOKEN_EQUALS, "Expected '=' after name");
}

static void using_directive() {
  group();
  keyword(kw_using);
  space();

  if (check_keyword(kw_static)) {
    keyword(kw_static);
    space();
  }

  bool indented = false;
  if (check(TOKEN_IDENTIFIER) && peek(TOKEN_EQUALS)) {
    name_equals();

    indented = true;
    indent();
    line();
  }

  qualified_name();
  token(TOKEN_SEMICOLON, "Expected 'using' to end with a semicolon");

  if (indented) {
    dedent();
  }
  end();
}

static void using_directives() {
  if (check_keyword(kw_using)) {
    while (check_keyword(kw_using)) {
      using_directive();
      line();
    }
    line();
  }
}

static void compilation_unit() {
  extern_alias_directives();
  using_directives();
}

bool format_csharp(struct DocBuilder *builder, const char *source) {
  parser.builder = builder;
  parser.had_error = false;
  parser.panic_mode = false;
  init_token_buffer(&parser.buffer);

  lexer_init(source);
  advance();

  compilation_unit();
  token(TOKEN_EOF, "Expect end of compilation unit");

  return !parser.had_error;
}
