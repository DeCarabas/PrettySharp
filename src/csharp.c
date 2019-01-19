#include "csharp.h"
#include "lexer.h"
#include "token.h"

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

int last_debug_line = -1;
static void vdebug(const char *format, va_list args) {
#ifdef PRINT_DEBUG_ENABLED
  if (parser.current.line != last_debug_line) {
    fprintf(stderr, "%4d ", parser.current.line);
    last_debug_line = parser.current.line;
  } else {
    fprintf(stderr, "   | ");
  }
  vfprintf(stderr, format, args);
  fprintf(stderr, "\n");
#else
  UNUSED(format);
  UNUSED(args);
#endif
}

static void debug(const char *format, ...) {
  va_list args;
  va_start(args, format);
  vdebug(format, args);
  va_end(args);
}

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

static void verror_at_current(const char *format, va_list args) {
  verror_at(&parser.current, format, args);
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

static void token(enum TokenType type) {
  if (parser.current.type == type) {
    doc_text(parser.builder, parser.current.start, parser.current.length);
    advance();
  } else {
    error_at_current("Expected '%s'", token_text(type));
  }
}

static bool check(enum TokenType type) {
  bool result = parser.current.type == type;
  debug("check: %d (%s) vs %d (%s) => %s", parser.current.type,
        token_text(parser.current.type), type, token_text(type),
        result ? "true" : "false");
  return result;
}

static bool check_any(enum TokenType *types, int count) {
  for (int i = 0; i < count; i++) {
    if (check(types[i])) {
      return true;
    }
  }
  return false;
}

static bool match(enum TokenType type) {
  if (!check(type)) {
    return false;
  }
  doc_text(parser.builder, parser.current.start, parser.current.length);
  advance();
  return true;
}

static bool match_any(enum TokenType *types, int count) {
  for (int i = 0; i < count; i++) {
    if (match(types[i])) {
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

static bool check_identifier() {
  return is_identifier_token(parser.current.type);
}

static void identifier() {
  if (check_identifier()) {
    doc_text(parser.builder, parser.current.start, parser.current.length);
    advance();
  } else {
    error_at_current("Expected an identifier");
  }
}

static void extern_alias() {
  token(TOKEN_KW_EXTERN);
  space();
  token(TOKEN_KW_ALIAS);
  space();
  identifier();
  token(TOKEN_SEMICOLON);
}

static void extern_alias_directives() {
  if (check(TOKEN_KW_EXTERN)) {
    while (check(TOKEN_KW_EXTERN)) {
      extern_alias();
      line();
    }
    line();
  }
}

static void name_equals() {
  identifier();
  space();
  token(TOKEN_EQUALS);
}

static void type_name();

static void optional_type_argument_list() {
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
    token(TOKEN_GREATERTHAN);
  }
  end();
}

static void simple_name() {
  identifier();
  optional_type_argument_list();
}

static void namespace_or_type_name() {
  simple_name();
  while (match(TOKEN_DOT) || match(TOKEN_COLON_COLON)) {
    simple_name();
  }
}

static void type_name() { namespace_or_type_name(); }

static bool check_name_equals() {
  return check(TOKEN_IDENTIFIER) && check_next(TOKEN_EQUALS);
}

static void using_directive() {
  group();
  token(TOKEN_KW_USING);
  space();

  if (match(TOKEN_KW_STATIC)) {
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
  token(TOKEN_SEMICOLON);

  if (indented) {
    dedent();
  }
  end();
}

static void using_directives() {
  if (check(TOKEN_KW_USING)) {
    while (check(TOKEN_KW_USING)) {
      using_directive();
      line();
    }
    line();
  }
}

static enum TokenType builtin_type_tokens[] = {
    TOKEN_KW_SBYTE, TOKEN_KW_BYTE,   TOKEN_KW_SHORT,   TOKEN_KW_USHORT,
    TOKEN_KW_INT,   TOKEN_KW_UINT,   TOKEN_KW_LONG,    TOKEN_KW_ULONG,
    TOKEN_KW_CHAR,  TOKEN_KW_FLOAT,  TOKEN_KW_DOUBLE,  TOKEN_KW_DECIMAL,
    TOKEN_KW_BOOL,  TOKEN_KW_OBJECT, TOKEN_KW_DYNAMIC, TOKEN_KW_STRING,
};

static bool check_type() {
  return check_any(builtin_type_tokens, ARRAY_SIZE(builtin_type_tokens)) ||
         check_identifier();
}
static void type() {
  if (!match_any(builtin_type_tokens, ARRAY_SIZE(builtin_type_tokens))) {
    type_name();
  }

  // Handle all the stuff at the end....
  while (check(TOKEN_OPENBRACKET) || check(TOKEN_QUESTION) ||
         check(TOKEN_ASTERISK)) {

    // Array ranks.
    if (match(TOKEN_OPENBRACKET)) {
      while (match(TOKEN_COMMA)) {
        ;
      }
      token(TOKEN_CLOSEBRACKET);
    }

    // Nullable.
    match(TOKEN_QUESTION);

    // Pointer.
    match(TOKEN_ASTERISK);
  }
}

static void expression() {
  error_at_current("Expression not implemented");
  advance();
}

static void attribute_name() { type_name(); }

static void attribute_arguments() {
  token(TOKEN_OPENPAREN);
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
  token(TOKEN_CLOSEPAREN);
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
  token(TOKEN_OPENBRACKET);
  indent();
  softline();

  if (check(TOKEN_IDENTIFIER) && check_next(TOKEN_COLON)) {
    identifier();
    token(TOKEN_COLON);
    line();
  }

  attribute_list();

  dedent();
  softline();
  token(TOKEN_CLOSEBRACKET);
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

static void return_type() {
  if (!match(TOKEN_KW_VOID)) {
    type();
  }
}

static enum TokenType parameter_modifier_tokens[] = {
    TOKEN_KW_IN, TOKEN_KW_OUT, TOKEN_KW_REF, TOKEN_KW_THIS, TOKEN_KW_PARAMS,
};

static bool check_formal_parameter() {
  return check(TOKEN_OPENBRACKET) ||
         check_any(parameter_modifier_tokens,
                   ARRAY_SIZE(parameter_modifier_tokens)) ||
         check_type();
}

static void formal_parameter_list() {
  token(TOKEN_OPENPAREN);
  {
    indent();
    softline();

    bool first = true;
    while (check_formal_parameter()) {
      if (!first) {
        token(TOKEN_COMMA);
        line();
      }
      first = false;

      attributes();
      if (match_any(parameter_modifier_tokens,
                    ARRAY_SIZE(parameter_modifier_tokens))) {
        space();
      }
      type();
      space();
      identifier();
      if (check(TOKEN_EQUALS)) {
        space();
        token(TOKEN_EQUALS);
        line();
        indent();
        expression();
        dedent();
      }
    }

    dedent();
  }
  token(TOKEN_CLOSEPAREN);
}

static void type_constraint() {
  if (match(TOKEN_KW_NEW)) {
    token(TOKEN_OPENPAREN);
    token(TOKEN_CLOSEPAREN);
  } else {
    identifier();
  }
}

static void optional_type_parameter_list() {
  group();
  if (match(TOKEN_LESSTHAN)) {
    indent();
    softline();

    attributes();
    if (match(TOKEN_KW_IN) || match(TOKEN_KW_OUT)) {
      space();
    }
    identifier();
    while (match(TOKEN_COMMA)) {
      line();
      attributes();
      if (match(TOKEN_KW_IN) || match(TOKEN_KW_OUT)) {
        space();
      }
      identifier();
    }

    dedent();
    softline();
    token(TOKEN_GREATERTHAN);
  }
  end();
}

static void base_types() {
  group();
  token(TOKEN_COLON);
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

static void type_parameter_constraint() {
  group();
  token(TOKEN_KW_WHERE);
  space();
  identifier();
  token(TOKEN_COLON);
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

static void type_parameter_constraint_clauses() {
  type_parameter_constraint();
  if (check(TOKEN_KW_WHERE)) {
    // MORE THAN ONE, UGH.
    breakparent();
    while (check(TOKEN_KW_WHERE)) {
      line();
      type_parameter_constraint();
    }
  }
}

static void class_declaration() {
  token(TOKEN_KW_CLASS);
  space();
  identifier();
  optional_type_parameter_list();

  if (check(TOKEN_COLON)) {
    indent();
    line();
    base_types();
    dedent();
  }

  if (check(TOKEN_KW_WHERE)) {
    indent();
    line();
    type_parameter_constraint_clauses();
    dedent();
  }

  match(TOKEN_SEMICOLON);
}

static void struct_declaration() {
  token(TOKEN_KW_STRUCT);
  space();
  identifier();
  optional_type_parameter_list();

  if (check(TOKEN_COLON)) {
    indent();
    line();
    base_types();
    dedent();
  }

  if (check(TOKEN_KW_WHERE)) {
    indent();
    line();
    type_parameter_constraint_clauses();
    dedent();
  }

  match(TOKEN_SEMICOLON);
}

static enum TokenType modifier_tokens[] = {
    TOKEN_KW_NEW,      TOKEN_KW_PUBLIC,  TOKEN_KW_PROTECTED,
    TOKEN_KW_INTERNAL, TOKEN_KW_PRIVATE, TOKEN_KW_ABSTRACT,
    TOKEN_KW_SEALED,   TOKEN_KW_STATIC,  TOKEN_KW_UNSAFE,
};

static bool check_modifier() {
  return check_any(modifier_tokens, ARRAY_SIZE(modifier_tokens));
}

static bool match_modifier() {
  return match_any(modifier_tokens, ARRAY_SIZE(modifier_tokens));
}

static bool check_member() { return false; }

enum MemberKind {
  MEMBERKIND_NONE,
  MEMBERKIND_GARBAGE_ATTRIBUTES,
  MEMBERKIND_CONSTANT,
  MEMBERKIND_FIELD,
  MEMBERKIND_METHOD,
  MEMBERKIND_PROPERTY,
  MEMBERKIND_EVENT,
  MEMBERKIND_INDEXER,
  MEMBERKIND_OPERATOR,
  MEMBERKIND_CONSTRUCTOR,
  MEMBERKIND_DESTRUCTOR,
  MEMBERKIND_STATIC_CONSTRUCTOR,
  MEMBERKIND_TYPE,
};

static enum MemberKind member(enum MemberKind last_member_kind) {
  // OK this is tough in a single-pass system like mine because I need to figure
  // out what I'm looking at before I decide what the inter-kind spacing is.

  return MEMBERKIND_NONE;
}

static void interface_declaration() {
  token(TOKEN_KW_INTERFACE);
  space();
  identifier();
  optional_type_parameter_list();

  if (check(TOKEN_COLON)) {
    indent();
    line();
    base_types();
    dedent();
  }

  if (check(TOKEN_KW_WHERE)) {
    indent();
    line();
    type_parameter_constraint_clauses();
    dedent();
  }

  line();
  token(TOKEN_OPENBRACE);
  {
    indent();
    line();

    enum MemberKind last_member_kind = MEMBERKIND_NONE;
    while (check_member()) {
      attributes();
      last_member_kind = member(last_member_kind);
    }

    dedent();
  }
  token(TOKEN_CLOSEBRACE);
  match(TOKEN_SEMICOLON);
}

static void enum_declaration() {
  token(TOKEN_KW_ENUM);
  space();
  identifier();

  if (check(TOKEN_COLON)) {
    indent();
    line();
    type();
    dedent();
  }

  line();
  token(TOKEN_OPENBRACE);
  {
    indent();
    line();

    bool first = true;
    while (check(TOKEN_OPENBRACKET) || check_identifier()) {
      if (!first) {
        token(TOKEN_COMMA);
        line();
      }
      first = false;

      identifier();
      if (check(TOKEN_EQUALS)) {
        space();
        token(TOKEN_EQUALS);
        indent();
        line();
        expression();
        dedent();
      }
    }

    dedent();
  }
  token(TOKEN_CLOSEBRACE);

  match(TOKEN_SEMICOLON);
}

static void delegate_declaration() {
  group();
  token(TOKEN_KW_DELEGATE);
  space();
  return_type();
  line();
  identifier();
  optional_type_parameter_list();

  formal_parameter_list();

  if (check(TOKEN_KW_WHERE)) {
    indent();
    line();
    type_parameter_constraint_clauses();
    dedent();
  }

  end();
}

static enum TokenType type_keyword_tokens[] = {
    TOKEN_KW_CLASS, TOKEN_KW_STRUCT,   TOKEN_KW_INTERFACE,
    TOKEN_KW_ENUM,  TOKEN_KW_DELEGATE,
};

static bool check_type_keyword() {
  return check_any(type_keyword_tokens, ARRAY_SIZE(type_keyword_tokens));
}

static bool check_type_declaration() {
  return check(TOKEN_OPENBRACKET) || check_modifier() || check_type_keyword();
}

static void type_declaration() {
  attributes();
  {
    group();
    while (match_modifier()) {
      line();
    }
    end();
  }

  if (match(TOKEN_KW_PARTIAL)) {
    space();
  }

  if (check(TOKEN_KW_CLASS)) {
    class_declaration();
  } else if (check(TOKEN_KW_STRUCT)) {
    struct_declaration();
  } else if (check(TOKEN_KW_INTERFACE)) {
    interface_declaration();
  } else if (check(TOKEN_KW_ENUM)) {
    enum_declaration();
  } else if (check(TOKEN_KW_DELEGATE)) {
    delegate_declaration();
  } else {
    error_at_current("Expected some kind of type keyword.");
  }
}

static void namespace_members();

static void namespace_body() {
  extern_alias_directives();
  using_directives();
  namespace_members();
}

static void qualified_identifier() {
  identifier();
  while (match(TOKEN_DOT)) {
    identifier();
  }
}

static void namespace_declaration() {
  token(TOKEN_KW_NAMESPACE);
  space();
  qualified_identifier();
  space();
  token(TOKEN_OPENBRACE);

  {
    indent();
    line();

    namespace_body();

    dedent();
    line();
  }

  token(TOKEN_CLOSEBRACE);
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

    if (check(TOKEN_KW_NAMESPACE)) {
      namespace_declaration();
    } else if (check_type_declaration()) {
      type_declaration();
    } else {
      break;
    }
  }
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
  token(TOKEN_EOF);

  return !parser.had_error;
}
