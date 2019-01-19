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

static bool check_next_and_return_token(enum TokenType type,
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

static bool check_next(enum TokenType type) {
  struct Token token;
  return check_next_and_return_token(type, &token);
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

static bool check_next_keyword(struct Keyword *keyword) {
  struct Token token;
  if (check_next_and_return_token(TOKEN_IDENTIFIER, &token)) {
    if (check_keyword_token(keyword, &token)) {
      return true;
    }
  }
  return false;
}

static void group() { doc_group(parser.builder); }
static void end() { doc_end(parser.builder); }
static void line() { doc_line(parser.builder); }
static void softline() { doc_softline(parser.builder); }
static void breakparent() { doc_breakparent(parser.builder); }
static void indent() { doc_indent(parser.builder); }
static void dedent() { doc_dedent(parser.builder); }
static void space() { doc_text(parser.builder, " ", 1); }

static void identifier() { token(TOKEN_IDENTIFIER, "Expected an identifier"); }

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

static void type_name();

static void optional_type_parameter_list() {
  group();
  if (match(TOKEN_LESSTHAN)) {
    indent();
    softline();

    type_name();
    while (match(TOKEN_COMMA)) {
      line();
      type_name();
    }

    dedent();
    softline();
    token(TOKEN_GREATERTHAN, "Expected > to close the type argument list");
  }
  end();
}

static void simple_name() {
  identifier();
  optional_type_parameter_list();
}

static void namespace_or_type_name() {
  simple_name();
  while (match(TOKEN_DOT) || match(TOKEN_COLON_COLON)) {
    simple_name();
  }
}

static void type_name() { namespace_or_type_name(); }
static void namespace_name() { namespace_or_type_name(); }

static bool check_name_equals() {
  return check(TOKEN_IDENTIFIER) && check_next(TOKEN_EQUALS);
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
  if (check_name_equals()) {
    name_equals();

    indented = true;
    indent();
    line();
  }

  namespace_or_type_name();
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

static void expression() {
  error_at_current("Expression not implemented");
  advance();
}

static void attribute_name() { type_name(); }

static void attribute_arguments() {
  token(TOKEN_OPENPAREN, "Exepct '(' to start argument list");
  indent();
  softline();

  while (!check(TOKEN_CLOSEPAREN) && !check(TOKEN_EOF)) {
    group();
    bool indented = false;
    if (check_name_equals()) {
      name_equals();

      indent();
      line();
      indented = true;
    }

    expression();

    if (indented) {
      dedent();
    }
    end();
  }

  dedent();
  softline();
  token(TOKEN_CLOSEPAREN, "Expect ')' to end argument list");
}

static void attribute() {
  group();
  attribute_name();
  attribute_arguments();
  end();
}

static void attribute_list() {
  attribute();
  while (match(TOKEN_COMMA) && !check(TOKEN_CLOSEBRACKET)) {
    line();
    attribute();
  }
}

static void attribute_section() {
  group();
  token(TOKEN_OPENBRACKET, "Expect '[' to start attribute list");
  indent();
  softline();

  if (check(TOKEN_IDENTIFIER) && check_next(TOKEN_COLON)) {
    identifier();
    token(TOKEN_COLON, "Expect ':' after attribute target");
    line();
  }

  attribute_list();

  dedent();
  softline();
  token(TOKEN_CLOSEBRACKET, "Expect ']' to end attribute list");
  end();
}

static bool attributes() {
  if (check(TOKEN_OPENBRACKET)) {
    breakparent();
    while (check(TOKEN_OPENBRACKET)) {
      attribute_section();
      line();
    }
    return true;
  }
  return false;
}

static void global_attributes() {
  if (attributes()) {
    line();
  }
}

static bool is_type_keyword() {
  return (check_keyword(kw_class) || check_keyword(kw_struct) ||
          check_keyword(kw_interface) || check_keyword(kw_enum) ||
          check_keyword(kw_delegate));
}

static void type_constraint() {
  if (match_keyword(kw_new)) {
    token(TOKEN_OPENPAREN, "Expect ( in constructor type constraint");
    token(TOKEN_CLOSEPAREN, "Expect ) in constructor type constraint");
  } else {
    identifier();
  }
}

static void type_declaration() {
  attributes();
  {
    group();
    while (check(TOKEN_IDENTIFIER) && !is_type_keyword()) {
      // public static partial virtual blah blah blah who cares
      identifier();
      line();
    }
    end();
  }

  // TODO: Switch on type type?
  token(TOKEN_IDENTIFIER, "Expect type keyword");
  space();
  identifier();

  // Type parameter list
  {
    group();
    if (match(TOKEN_LESSTHAN)) {
      indent();
      softline();

      attributes();
      identifier();
      while (match(TOKEN_COMMA)) {
        line();
        attributes();
        identifier();
      }

      dedent();
      softline();
      token(TOKEN_GREATERTHAN, "Expect type argument list to end with '>'");
    }
    end();
  }

  // Base types
  if (check(TOKEN_COLON)) {
    indent();
    line();
    {
      group();
      token(TOKEN_COLON, "Expect a ':' for base types");
      space();
      type_name();
      {
        indent();
        while (match(TOKEN_COMMA)) {
          line();
          type_name();
        }
        dedent();
      }
      end();
    }
    dedent();
  }

  // Type parameters constraints clauses
  if (check_keyword(kw_where)) {
    indent();
    line();
    {
      group();
      keyword(kw_where);
      space();
      identifier();
      token(TOKEN_COLON, "Expect a ':' after type identifier");
      {
        indent();
        line();
        type_constraint();
        while (match(TOKEN_COMMA)) {
          line();
          type_constraint();
        }
        dedent();
      }

      end();
    }
    dedent();
  }

  // Body.
  if (match(TOKEN_OPENBRACE)) {
    // Fuck.
    token(TOKEN_CLOSEBRACE, "Expected type to end");
  }

  match(TOKEN_SEMICOLON);
}

static void namespace_members() {
  bool first = true;
  for (;;) {
    if (!first) {
      // Blank lines between members.
      line();
      line();
    }
    first = false;

    if (match_keyword(kw_namespace)) {
      space();
      namespace_name();
      space();
      token(TOKEN_OPENBRACE, "Expect '{' in namespace declaration");
      indent();
      line();
      namespace_members();

      dedent();
      line();
      token(TOKEN_CLOSEBRACE, "Expect '}' to close namespace");
    } else if (check(TOKEN_IDENTIFIER)) {
      type_declaration();
    } else {
      break;
    }
  }
  // A namespace member is a namespace or a type.
  // Types are hard because they're buried under a bunch of modifiers.
}

static void compilation_unit() {
  extern_alias_directives();
  using_directives();
  global_attributes();
  namespace_members();
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
