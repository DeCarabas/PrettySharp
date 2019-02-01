#include "csharp.h"
#include "lexer.h"
#include "token.h"
#include <assert.h>

struct Parser {
  struct DocBuilder *builder;
  struct TokenBuffer buffer;
  int index;

  struct Token current;
  struct Token previous;

  int trivia_index;
  bool has_trivia;

  bool had_error;
  bool panic_mode;
};

struct Parser parser;

// ============================================================================
// Error Reporting
// ============================================================================
// #define PRINT_DEBUG_ENABLED

#ifdef PRINT_DEBUG_ENABLED

int last_debug_line = -1;
static void vDEBUG(const char *format, va_list args) {
  if (parser.current.line != last_debug_line) {
    fprintf(stderr, "%4d ", parser.current.line);
    last_debug_line = parser.current.line;
  } else {
    fprintf(stderr, "   | ");
  }
  vfprintf(stderr, format, args);
  fprintf(stderr, "\n");
  UNUSED(format);
  UNUSED(args);
}

static void DEBUG_(const char *format, ...) {
  va_list args;
  va_start(args, format);
  vDEBUG(format, args);
  va_end(args);
}

#define DEBUG(x) (DEBUG_ x)

#else

#define DEBUG(x) ((void)0)

#endif

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

  // TODO: Fix all the bugs where we get stuck on parse errors.
  exit(27);
}

static void verror(const char *format, va_list args) {
  verror_at(&parser.current, format, args);
}

static void error(const char *format, ...) {
  va_list args;
  va_start(args, format);
  verror(format, args);
  va_end(args);
}

// We only have this to help tracking parts we haven't done yet.
static void notimplemented(const char *format, ...) {
  va_list args;
  va_start(args, format);
  verror(format, args);
  va_end(args);
}

// ============================================================================
// Trivia
// ============================================================================

// N.B.: Trivia is super hard to deal with. On the one hand, we don't want to
//       force ourselves to handle trivia explicitly before and after every
//       token, or to weave it through the productions: that's why it's
//       *trivia*. On the other hand, we can't just throw it on the floor,
//       because we need to at least save comments. What we do is a compromise:
//       we flush trivia just before we write a token or start a group. This is
//       correct enough, most of the time: we don't want to write the trivia
//       before we've had a chance to look at .current, because we might be
//       writing inter-production whitespace. And we don't want the trivia to be
//       part of a group started by the main code (in general). So, this.
static void flush_trivia() {
  if (!parser.has_trivia) {
    return;
  }

  for (; parser.trivia_index < parser.index; parser.trivia_index++) {
    struct Token trivia = parser.buffer.tokens[parser.trivia_index];
    if (trivia.type == TOKEN_TRIVIA_BLOCK_COMMENT) {
      DEBUG(("Handling block comment"));
      doc_text(parser.builder, trivia.start, trivia.length);
      doc_line(parser.builder);
    } else if (trivia.type == TOKEN_TRIVIA_LINE_COMMENT) {
      DEBUG(("Handling line comment"));
      doc_breakparent(parser.builder);
      doc_text(parser.builder, trivia.start, trivia.length);
      doc_line(parser.builder);
    } else {
      // TODO: Consecutive newlines?
    }
  }

  parser.has_trivia = false;
}

// ============================================================================
// Formatting
// ============================================================================

static void text(struct Token token) {
  flush_trivia();
  doc_text(parser.builder, token.start, token.length);
}
static void group() {
  flush_trivia();
  doc_group(parser.builder);
}
static void end() { doc_end(parser.builder); }
static void indent() { doc_indent(parser.builder); }
static void dedent() { doc_dedent(parser.builder); }
static void line() { doc_line(parser.builder); }
static void line_indent() {
  indent();
  line();
}
static void softline() { doc_softline(parser.builder); }
static void softline_indent() {
  indent();
  softline();
}
static void breakparent() { doc_breakparent(parser.builder); }
static void space() { doc_text(parser.builder, " ", 1); }

// ============================================================================
// Checking and Consuming Tokens
// ============================================================================

static void advance() {
  parser.previous = parser.current;
  parser.trivia_index = parser.index;
  for (;;) {
    if (parser.index == parser.buffer.count) {
      break;
    }
    parser.current = parser.buffer.tokens[parser.index];
    parser.index += 1;

    if (parser.current.type == TOKEN_TRIVIA_BLOCK_COMMENT ||
        parser.current.type == TOKEN_TRIVIA_LINE_COMMENT) {
      parser.has_trivia = true;
      continue;
    } else if (parser.current.type == TOKEN_TRIVIA_WHITESPACE ||
               parser.current.type == TOKEN_TRIVIA_EOL) {
      continue; // Skip!
    }

    if (parser.current.type != TOKEN_ERROR) {
      break;
    }

    error("%s", parser.current.start);
  }
}

static struct Token next_significant_token(int *index) {
  int cursor = *index;
  struct Token token = parser.buffer.tokens[*index];
  for (;;) {
    if (cursor == parser.buffer.count) {
      break;
    }
    token = parser.buffer.tokens[cursor];
    cursor += 1;

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
  *index = cursor;
  return token;
}

static bool check_next(enum TokenType type) {
  int i = parser.index;
  struct Token token = next_significant_token(&i);
  return token.type == type;
}

static bool check_next_identifier() {
  int i = parser.index;
  struct Token token = next_significant_token(&i);
  return is_identifier_token(token.type);
}

static void single_token() {
  text(parser.current);
  advance();
}

static void token(enum TokenType type, const char *where) {
  if (parser.current.type == type) {
    DEBUG(("Token %s %s", token_text(type), where));
    single_token();
  } else {
    error("Expected '%s' %s", token_text(type), where);
  }
}

static bool check_(enum TokenType type, int line) {
  bool result = parser.current.type == type;
  DEBUG(("%4d Check %s == %s -> %s", line, token_text(type),
         token_text(parser.current.type), result ? "true" : "false"));
  return result;
}

#define check(t) check_(t, __LINE__)

static bool check_is_any_(enum TokenType type, const enum TokenType *types,
                          int count, int line) {
  DEBUG(("%4d Check any %s == ", line, token_text(type)));
  for (int i = 0; i < count; i++) {
    bool result = type == types[i];
    DEBUG(
        ("        %s -> %s", token_text(types[i]), result ? "true" : "false"));
    if (result) {
      return true;
    }
  }
  return false;
}

#define check_is_any(t, ts, c) check_is_any_(t, ts, c, __LINE__)

static bool check_any(const enum TokenType *types, int count) {
  return check_is_any(parser.current.type, types, count);
}

static bool match(enum TokenType type) {
  if (!check(type)) {
    return false;
  }
  single_token();
  return true;
}

static bool match_any(const enum TokenType *types, int count) {
  for (int i = 0; i < count; i++) {
    if (match(types[i])) {
      return true;
    }
  }
  return false;
}

// ============================================================================
// Names
// ============================================================================

static bool check_identifier() {
  return is_identifier_token(parser.current.type);
}

static void identifier(const char *where) {
  if (check_identifier()) {
    DEBUG(("Identifier %s (%.*s) %s", token_text(parser.current.type),
           parser.current.length, parser.current.start, where));
    single_token();
  } else {
    error("Expected an identifier %s", where);
  }
}
static void name_equals(const char *where) {
  identifier(where);
  space();
  token(TOKEN_EQUALS, where);
}

static void type_name();

static void optional_type_argument_list() {
  group();
  if (match(TOKEN_LESSTHAN)) {
    {
      softline_indent();
      type_name();
      while (match(TOKEN_COMMA)) {
        line();
        type_name();
      }
      dedent();
    }
    softline();
    token(TOKEN_GREATERTHAN, "at the end of a type argument list");
  }
  end();
}

static void simple_name(const char *where) {
  identifier(where);
  optional_type_argument_list();
}

static void namespace_or_type_name(const char *where) {
  simple_name(where);
  while (match(TOKEN_DOT) || match(TOKEN_COLON_COLON)) {
    simple_name(where);
  }
}

static void type_name() { namespace_or_type_name("in type name"); }

static bool check_name_equals() {
  return check(TOKEN_IDENTIFIER) && check_next(TOKEN_EQUALS);
}

const static enum TokenType builtin_type_tokens[] = {
    TOKEN_KW_SBYTE, TOKEN_KW_BYTE,   TOKEN_KW_SHORT,   TOKEN_KW_USHORT,
    TOKEN_KW_INT,   TOKEN_KW_UINT,   TOKEN_KW_LONG,    TOKEN_KW_ULONG,
    TOKEN_KW_CHAR,  TOKEN_KW_FLOAT,  TOKEN_KW_DOUBLE,  TOKEN_KW_DECIMAL,
    TOKEN_KW_BOOL,  TOKEN_KW_OBJECT, TOKEN_KW_DYNAMIC, TOKEN_KW_STRING,
};

static bool check_is_type(enum TokenType token) {
  return check_is_any(token, builtin_type_tokens,
                      ARRAY_SIZE(builtin_type_tokens)) ||
         is_identifier_token(token);
}

static bool check_type() { return check_is_type(parser.current.type); }

static void non_array_type() {
  if (!match_any(builtin_type_tokens, ARRAY_SIZE(builtin_type_tokens))) {
    type_name();
  }

  while (check(TOKEN_QUESTION) || check(TOKEN_ASTERISK)) {
    // Nullable.
    match(TOKEN_QUESTION);

    // Pointer.
    match(TOKEN_ASTERISK);
  }
}

static void rank_specifier() {
  token(TOKEN_OPENBRACKET, "in array rank specifier");
  while (match(TOKEN_COMMA)) {
    ;
  }
  token(TOKEN_CLOSEBRACKET, "in array rank specifier");
}

static void type() {
  non_array_type();

  // Handle all the stuff at the end....
  while (check(TOKEN_OPENBRACKET) || check(TOKEN_QUESTION) ||
         check(TOKEN_ASTERISK)) {

    // Array ranks.
    if (check(TOKEN_OPENBRACKET)) {
      rank_specifier();
    }

    // Nullable.
    match(TOKEN_QUESTION);

    // Pointer.
    match(TOKEN_ASTERISK);
  }
}

// ============================================================================
// Expressions
// ============================================================================
static void expression(const char *where);

static const struct ParseRule *get_rule(enum TokenType type);

enum Precedence {
  PREC_NONE,

  // NOTE: These are from lowest precedence to highest precedence.
  // https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#operator-precedence-and-associativity
  PREC_ASSIGNMENT,
  PREC_CONDITIONAL,
  PREC_NULL_COALESCING,
  PREC_CONDITIONAL_OR,
  PREC_CONDITIONAL_AND,
  PREC_LOGICAL_OR,
  PREC_LOGICAL_XOR,
  PREC_LOGICAL_AND,
  PREC_EQUALITY,
  PREC_RELATIONAL,
  PREC_GREATERTHAN,
  PREC_SHIFT,
  PREC_ADDITIVE,
  PREC_MULTIPLICATIVE,
  PREC_UNARY,
  PREC_PRIMARY,
};

typedef void (*ParseFn)();

struct ParseRule {
  ParseFn prefix;
  ParseFn infix;
  enum Precedence precedence;
};

static const struct ParseRule *get_rule(enum TokenType type);

static void parse_precedence(enum Precedence precedence, const char *where) {
  group();
  ParseFn prefix_rule = get_rule(parser.current.type)->prefix;
  if (prefix_rule == NULL) {
    error("Expect expression %s.", where);
    return;
  }

  prefix_rule();

  while (precedence <= get_rule(parser.current.type)->precedence) {
    ParseFn infix_rule = get_rule(parser.current.type)->infix;
    infix_rule();
  }
  end();
}

static void block(const char *where);

static void implicitly_typed_lambda() {
  group();
  identifier("in implicitly typed lambda");
  space();
  token(TOKEN_EQUALS_GREATERTHAN, "in implicitly typed lambda");
  if (check(TOKEN_OPENBRACE)) {
    space();
    block("at the beginning of the body of a lambda");
  } else {
    line_indent();
    group();
    expression("in the body of a lambda");
    end();
    dedent();
  }
  end();
}

static void primary() {
  if (check_identifier() && check_next(TOKEN_EQUALS_GREATERTHAN)) {
    implicitly_typed_lambda();
  } else {
    single_token();
  }
}

// This comes up all the time, both here in expression land and down in
// statement land.
static void parenthesized_expression() {
  group();
  token(TOKEN_OPENPAREN, "in parenthesized expression");
  {
    softline_indent();
    expression("between parentheses in a parenthesized expression");
    dedent();
  }
  softline();
  token(TOKEN_CLOSEPAREN, "in parenthesized expression");
  end();
}

static bool check_parenthesized_implicitly_typed_lambda() {
  // Case 1: ( x ,
  // Case 2: ( x ) =>
  // Case 3: ( ) =>
  if (parser.current.type != TOKEN_OPENPAREN) {
    return false;
  }

  int index = parser.index;
  struct Token token = next_significant_token(&index);
  if (is_identifier_token(token.type)) {
    token = next_significant_token(&index);
    if (token.type == TOKEN_COMMA) {
      return true; // 1
    } else if (token.type == TOKEN_CLOSEPAREN) {
      token = next_significant_token(&index);
      if (token.type == TOKEN_EQUALS_GREATERTHAN) {
        return true; // 2
      }
    }
  } else if (token.type == TOKEN_CLOSEPAREN) {
    token = next_significant_token(&index);
    if (token.type == TOKEN_EQUALS_GREATERTHAN) {
      return true; // 3
    }
  }

  return false;
}

static void parenthesized_implicitly_typed_lambda() {
  group();
  {
    group();
    token(TOKEN_OPENPAREN, "in parenthesized lambda");
    if (check_identifier()) {
      softline_indent();
      bool first = true;
      while (first || check(TOKEN_COMMA)) {
        if (!first) {
          token(TOKEN_COMMA, "in parenthesized lambda");
          line();
        }
        first = false;
        identifier("in parenthesized lambda");
      }
      dedent();
      softline();
    }
    token(TOKEN_CLOSEPAREN, "in parenthesized lambda");
    end();
  }
  space();
  token(TOKEN_EQUALS_GREATERTHAN, "in parenthesized lambda");
  if (check(TOKEN_OPENBRACE)) {
    space();
    block("at the beginning of the body of a lambda");
  } else {
    line_indent();
    group();
    expression("in the body of a lambda");
    end();
    dedent();
  }
  end();
}

static bool check_parenthesized_explicitly_typed_lambda() {
  // do we have the following:
  //   case 1: ( T x , ... ) =>
  //   case 2: ( T x ) =>
  //   case 3: ( out T x,
  //   case 4: ( ref T x,
  //   case 5: ( out T x ) =>
  //   case 6: ( ref T x ) =>
  //   case 7: ( in T x ) =>
  //
  // if so then parse it as a lambda

  // Note: in the first two cases, we cannot distinguish a lambda from a tuple
  // expression containing declaration expressions, so we scan forwards to the
  // `=>` so we know for sure.
  return false;
}

static void parenthesized_explicitly_typed_lambda() {
  notimplemented("Not Implemented: Parenthesized explicitly typed lambda");
}

const static enum TokenType cannot_follow_cast_tokens[] = {
    TOKEN_KW_AS,
    TOKEN_KW_IS,
    TOKEN_SEMICOLON,
    TOKEN_CLOSEPAREN,
    TOKEN_CLOSEBRACKET,
    TOKEN_OPENBRACE,
    TOKEN_CLOSEBRACE,
    TOKEN_COMMA,
    TOKEN_EQUALS,
    TOKEN_PLUS_EQUALS,
    TOKEN_MINUS_EQUALS,
    TOKEN_ASTERISK_EQUALS,
    TOKEN_SLASH_EQUALS,
    TOKEN_PERCENT_EQUALS,
    TOKEN_AMPERSAND_EQUALS,
    TOKEN_CARET_EQUALS,
    TOKEN_BAR_EQUALS,
    TOKEN_LESSTHAN_LESSTHAN_EQUALS,
    TOKEN_QUESTION,
    TOKEN_COLON,
    TOKEN_BAR_BAR,
    TOKEN_AMPERSAND_AMPERSAND,
    TOKEN_BAR,
    TOKEN_CARET,
    TOKEN_AMPERSAND,
    TOKEN_EQUALS_EQUALS,
    TOKEN_EXCLAMATION_EQUALS,
    TOKEN_LESSTHAN,
    TOKEN_LESSTHAN_EQUALS,
    TOKEN_GREATERTHAN,
    TOKEN_GREATERTHAN_EQUALS,
    TOKEN_QUESTION_QUESTION_EQUALS,
    TOKEN_LESSTHAN_LESSTHAN,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_ASTERISK,
    TOKEN_SLASH,
    TOKEN_PERCENT,
    TOKEN_PLUS_PLUS,
    TOKEN_MINUS_MINUS,
    TOKEN_OPENBRACKET,
    TOKEN_DOT,
    TOKEN_MINUS_GREATERTHAN,
    TOKEN_QUESTION_QUESTION,
    TOKEN_KW_SWITCH,
    TOKEN_EOF,
};

static bool check_cast() {
  // This allows a lot of nonsense but if it makes sense we should provide the
  // right answer.
  if (parser.current.type != TOKEN_OPENPAREN) {
    return false;
  }

  int balance = 0;
  int index = parser.index;
  struct Token token;
  for (token = next_significant_token(&index); token.type != TOKEN_EOF;
       token = next_significant_token(&index)) {

    if (check_is_any(token.type, builtin_type_tokens,
                     ARRAY_SIZE(builtin_type_tokens))) {
      continue;
    }

    if (is_identifier_token(token.type)) {
      continue;
    }

    if (token.type == TOKEN_DOT) {
      continue;
    }

    if (token.type == TOKEN_GREATERTHAN) {
      balance -= 1;
      if (balance < 0) {
        return false;
      }
      continue;
    }

    if (token.type == TOKEN_LESSTHAN) {
      balance += 1;
      continue;
    }

    if (token.type == TOKEN_COMMA) {
      if (balance == 0) {
        return false;
      }
      continue;
    }

    if (token.type == TOKEN_CLOSEPAREN) {
      break;
    }

    return false;
  }

  token = next_significant_token(&index);
  if (check_is_any(token.type, cannot_follow_cast_tokens,
                   ARRAY_SIZE(cannot_follow_cast_tokens))) {
    return false;
  }

  return true;
}

static void cast() { notimplemented("Not Implemented: cast"); }

static void grouping() {
  if (check_parenthesized_implicitly_typed_lambda()) {
    parenthesized_implicitly_typed_lambda();
  } else if (check_cast()) {
    cast();
  } else if (check_parenthesized_explicitly_typed_lambda()) {
    parenthesized_explicitly_typed_lambda();
  } else {
    parenthesized_expression();
  }
}

static void argument_list_inner(enum TokenType closing_type) {
  if (!check(closing_type)) {
    softline_indent();
    group();
    if (check(TOKEN_IDENTIFIER) && check_next(TOKEN_COLON)) {
      identifier("in the name of a named argument");
      token(TOKEN_COLON, "after the name of a named argument");
      space();
    }
    expression("as the value of an argument");

    while (match(TOKEN_COMMA)) {
      end();

      line();

      group();
      if (check(TOKEN_IDENTIFIER) && check_next(TOKEN_COLON)) {
        identifier("in the name of a named argument");
        token(TOKEN_COLON, "after the end of a named argument");
        space();
      }
      expression("as the value of an argument");
    }
    end();
    dedent();
    softline();
  }
}

static void argument_list() {
  group();
  token(TOKEN_OPENPAREN, "at the beginning of an argument list");
  argument_list_inner(TOKEN_CLOSEPAREN);
  token(TOKEN_CLOSEPAREN, "at the end of an argument list");
  end();
}

static void invocation() { argument_list(); }

static void unary_prefix() {
  single_token();
  expression("to the right of a unary operator");
}

// TODO: UNARY!
static void array_initializer_inner(const char *where) {
  group();
  token(TOKEN_OPENBRACE, where);
  if (check(TOKEN_CLOSEBRACE)) {
    space();
  } else {
    softline_indent();
    group();

    bool first = true;
    while (first || check(TOKEN_COMMA)) {
      if (!first) {
        token(TOKEN_COMMA, "in array initializer");
        end();

        line();

        group();
      }
      first = false;

      if (check(TOKEN_OPENBRACE)) {
        array_initializer_inner(where);
      } else if (!check(TOKEN_CLOSEBRACE)) {
        expression("in an element of an array or collection");
      }
    }

    end();
    dedent();
    softline();
  }
  token(TOKEN_CLOSEBRACE, where);
  end();
}

static void collection_initializer() {
  array_initializer_inner("in collection initializer");
}

static void array_initializer() {
  array_initializer_inner("in array initializer");
}

static void object_initializer() {
  group();
  token(TOKEN_OPENBRACE, "at the beginning of an object initializer");
  if (check(TOKEN_CLOSEBRACE)) {
    space();
  } else {
    softline_indent();
    group();
    bool first = true;
    while (first || check(TOKEN_COMMA)) {
      if (!first) {
        token(TOKEN_COMMA, "between members in an object initializer");
        if (check(TOKEN_CLOSEBRACE)) {
          break;
        }
        end();

        line();

        group();
      }
      first = false;

      if (check(TOKEN_OPENBRACE)) {
        // I'm in a collection initializer and this is an element; it's going to
        // be an expression list just like in an array initializer.
        collection_initializer();
      } else if (!check(TOKEN_CLOSEBRACE)) {
        if (match(TOKEN_OPENBRACKET)) {
          argument_list_inner(TOKEN_CLOSEBRACKET);
          token(TOKEN_CLOSEBRACKET, "at the end of an object initializer");
        } else {
          // In an object initializer this is technically only allowed to be an
          // identifier. But in a collection this can be anything. And in an
          // anonymous object expression this can be 'this.' or 'base.' or
          // several other interesting forms. And I don't feel like building
          // parsers for all those distinct forms right now, mainly because
          // they're very difficult to distinguish from each other, and I can't
          // be bothered. So. This allows more forms than it technically should,
          // but I'm not worried.
          expression("in the member name of an object initializer");
        }
        if (check(TOKEN_EQUALS)) {
          space();
          token(
              TOKEN_EQUALS,
              "between the member and the expression in an object initializer");
          line_indent();
          expression("in the member value of an object initializer");
          dedent();
        }
      }
    }
    end();
    dedent();
  }
  softline();
  token(TOKEN_CLOSEBRACE, "at the end of an object initializer");
  end();
}

static void array_sizes(const char *where) {
  token(TOKEN_OPENBRACKET, where);
  {
    softline_indent();
    group();
    expression("in the list of sizes of an array");
    while (match(TOKEN_COMMA)) {
      end();

      line();

      group();
      expression("in the list of sizes of an array");
    }
    end();
    dedent();
  }
  token(TOKEN_CLOSEBRACKET, where);
}

static void object_creation() {
  token(TOKEN_KW_NEW, "in object creation");

  bool had_type = false;
  if (check_type()) {
    space();
    non_array_type();
    had_type = true;

    if (check(TOKEN_OPENPAREN)) {
      // Object or delegate creation.
      argument_list();

      if (check(TOKEN_OPENBRACE)) {
        line();
        object_initializer();
      }
    } else if (check(TOKEN_OPENBRACE)) {
      line();
      object_initializer();
    } else if (check(TOKEN_OPENBRACKET)) {
      if (!check_next(TOKEN_COMMA)) {
        array_sizes("in object creation");
      }

      while (check(TOKEN_OPENBRACKET)) {
        rank_specifier();
      }

      if (check(TOKEN_OPENBRACE)) {
        line();
        array_initializer();
      }
    } else {
      error("Expected constructor arguments, array size, or object "
            "initialization");
    }
  } else if (check(TOKEN_OPENBRACE)) {
    object_initializer();
  } else if (check(TOKEN_OPENBRACKET)) {
    if (!check_next(TOKEN_COMMA)) {
      array_sizes("in object creation");
    }

    while (check(TOKEN_OPENBRACKET)) {
      rank_specifier();
    }

    if (check(TOKEN_OPENBRACE)) {
      line();
      array_initializer();
    }
  } else {
    error("Expected a type name, array size, or anonymous object "
          "initialization.");
  }
}

static void query_expression() {
  notimplemented("Not Implemented: query_expression ('from')");
  advance();
}

static void binary() {
  group();
  enum TokenType op = parser.current.type;

  space();
  token(op, "in binary expression");

  bool indented;
  switch (op) {
  case TOKEN_EQUALS:
  case TOKEN_ASTERISK_EQUALS:
  case TOKEN_PLUS_EQUALS:
  case TOKEN_BAR_EQUALS:
  case TOKEN_AMPERSAND_EQUALS:
  case TOKEN_EXCLAMATION_EQUALS:
  case TOKEN_MINUS_EQUALS:
  case TOKEN_PERCENT_EQUALS:
  case TOKEN_QUESTION_QUESTION_EQUALS:
  case TOKEN_SLASH_EQUALS:
    line_indent();
    indented = true;
    break;

  default:
    line();
    indented = false;
    break;
  }

  const struct ParseRule *rule = get_rule(op);
  parse_precedence((enum Precedence)(rule->precedence + 1),
                   "to the right of a binary expression");

  if (indented) {
    dedent();
  }
  end();
}

static void member_access() {
  token(TOKEN_DOT, "in member access expression");
  identifier("in member access expression");
  optional_type_argument_list();
}

static void greater_than() {
  // This one is weird because it *might* be a shift operator, but might also be
  // a less than operator. Good thing our lexer doesn't discard whitespace,
  // right?
  group();

  enum Precedence prec = PREC_GREATERTHAN;
  if (parser.index < parser.buffer.count) {
    if (parser.buffer.tokens[parser.index + 1].type == TOKEN_GREATERTHAN) {
      prec = PREC_SHIFT;
    }
  }

  space();
  token(TOKEN_GREATERTHAN, "in binary expression");
  if (prec == PREC_SHIFT) {
    token(TOKEN_GREATERTHAN, "in left shift expression");
  }
  line();

  parse_precedence(prec + 1, "to the right of a binary expression");
  end();
}

static void is_as() {
  notimplemented("Not Implemented: is or as");
  advance();
}

static void conditional() {
  if (check_next(TOKEN_DOT)) {
    token(TOKEN_QUESTION, "in conditional member access expression");
    token(TOKEN_DOT, "in conditional member access expression");
    identifier("in conditional member access expression");
    optional_type_argument_list();
  } else if (check_next(TOKEN_OPENBRACKET)) {
    token(TOKEN_QUESTION,
          "at the beginning of a conditional element access expression");
    token(TOKEN_OPENBRACKET,
          "at the beginning of a conditional element access expression");
    argument_list_inner(TOKEN_CLOSEBRACKET);
    token(TOKEN_CLOSEBRACKET,
          "at the end of a conditional element access expression");
  } else {
    const struct ParseRule *rule = get_rule(TOKEN_QUESTION);

    token(TOKEN_QUESTION, "at the beginning of a ternary expression");
    line_indent();
    parse_precedence((enum Precedence)(rule->precedence + 1),
                     "after the question mark in a ternary expression");
    line();
    token(TOKEN_COLON, "in ternary expression");
    space();
    parse_precedence((enum Precedence)(rule->precedence + 1),
                     "after the colon in a ternary expression");
    dedent();
  }
}

const static struct ParseRule rules[] = {
#define TKN(id, txt, is_id, prefix, infix, prec) {prefix, infix, prec},
#include "token.inc"
#undef TKN
};

static const struct ParseRule *get_rule(enum TokenType type) {
  return &rules[type];
}

static void expression(const char *where) {
  parse_precedence(PREC_ASSIGNMENT, where);
}

// ============================================================================
// Statements
// ============================================================================

static void statement();
static void embedded_statement(bool embedded);

static void block(const char *where) {
  token(TOKEN_OPENBRACE, where);
  if (check(TOKEN_CLOSEBRACE)) {
    space();
    token(TOKEN_CLOSEBRACE, "at the end of a block");
    return;
  }

  // There *is* a body of some kind.
  breakparent();
  {
    softline_indent();
    statement();
    while (!(check(TOKEN_CLOSEBRACE) || check(TOKEN_EOF))) {
      if (parser.previous.type != TOKEN_SEMICOLON) {
        line();
      }
      line();
      statement();
    }
    dedent();
  }
  softline();
  token(TOKEN_CLOSEBRACE, "at the end of a block");
}

#define DEBUG_TOKEN(x)                                                         \
  do {                                                                         \
    if (!check(x)) {                                                           \
      fprintf(stderr, "***** %d\n", __LINE__);                                 \
    }                                                                          \
  } while (0);                                                                 \
  token(x)

// I want the type and the first identifier to be grouped.
static void variable_declarators(const char *where) {
  line();
  identifier(where);
  if (check(TOKEN_EQUALS)) {
    end();   // This is the end of the group with my type.
    group(); // Re-group, but just the value.
    space(); // No line break for the first variable.
    token(TOKEN_EQUALS, where);
    {
      line_indent();
      // N.B.: This is technically a "fixed_poiner_initializer", not a real
      // initializer, but we fold it in because it's pretty harmless (this code
      // is way more lenient by design than the real C# parser) and the
      // indentation & formatting logic is complex and needs to be kept the
      // same.
      match(TOKEN_AMPERSAND);
      expression("in the initial value of a variable");
      dedent();
    }
  }

  while (match(TOKEN_COMMA)) {
    end();
    group();
    line();

    identifier(where);
    if (check(TOKEN_EQUALS)) {
      space();
      token(TOKEN_EQUALS, where);
      {
        line_indent();
        // ("fixed_pointer_initializer", See above.)
        match(TOKEN_AMPERSAND);
        expression("in the initial value of a variable");
        dedent();
      }
    }
  }
  token(TOKEN_SEMICOLON, where);
}

static bool check_is_local_variable_type(enum TokenType type) {
  return (type == TOKEN_KW_VAR) || check_is_type(type);
}

static bool check_local_variable_type() {
  return check_is_local_variable_type(parser.current.type);
}

static void local_variable_type() {
  if (!match(TOKEN_KW_VAR)) {
    type();
  }
}

static bool check_local_variable_declaration() {
  // The problem here is that types have all these trailing things, and we need
  // to account for them.
  int index = parser.index;
  struct Token token = parser.current;
  if (check_is_local_variable_type(token.type)) {
    bool parsing_type_junk = true;
    while (parsing_type_junk) {
      int depth = 0;
      token = next_significant_token(&index);
      switch (token.type) {
      case TOKEN_LESSTHAN:
        depth += 1;
        break;

      case TOKEN_GREATERTHAN:
        depth -= 1;
        if (depth < 0) {
          parsing_type_junk = false;
        }
        break;

      case TOKEN_COMMA:
        if (depth == 0) {
          parsing_type_junk = false;
        }
        break;

      case TOKEN_QUESTION:
      case TOKEN_ASTERISK:
        break;

      default:
        if (depth == 0) {
          parsing_type_junk = false;
        } else if (!is_identifier_token(token.type)) {
          parsing_type_junk = false;
        }
        break;
      }
    }

    if (is_identifier_token(token.type)) {
      DEBUG(("Check local variable declaration: true"));
      return true;
    }
  }

  DEBUG(
      ("Check local variable declaration: false (%s)", token_text(token.type)));
  return false;
}

static void local_variable_declaration() {
  group();
  local_variable_type();
  variable_declarators("in local variable declaration");
  end();
}

static void local_const_declaration() {
  token(TOKEN_KW_CONST, "in constant declaration");
  space();
  local_variable_declaration();
}

static void if_statement() {
  token(TOKEN_KW_IF, "at the beginning of an if statement");
  space();
  parenthesized_expression();
  embedded_statement(/*embedded*/ true);
  while (check(TOKEN_KW_ELSE)) {
    line();
    token(TOKEN_KW_ELSE, "in an if statement");
    if (check(TOKEN_KW_IF)) {
      space();
      token(TOKEN_KW_IF, "in an if statement");
      space();
      parenthesized_expression();
    }

    embedded_statement(/*embedded*/ true);
  }
}

const static enum TokenType switch_section_end_tokens[] = {
    TOKEN_KW_CASE,
    TOKEN_KW_DEFAULT,
    TOKEN_CLOSEBRACE,
    TOKEN_EOF,
};

static void case_statement() {
  token(TOKEN_KW_CASE, "at the beginning of a case statement");
  space();
  parenthesized_expression();
  line();
  token(TOKEN_OPENBRACE, "at the beginning of the case block");
  line();
  while (check(TOKEN_KW_CASE) || check(TOKEN_KW_DEFAULT)) {
    if (match(TOKEN_KW_CASE)) {
      space();
      {
        indent();
        expression("in the label of a switch case");
        dedent();
      }
    } else {
      token(TOKEN_KW_DEFAULT, "or label in the case block");
    }
    token(TOKEN_COLON,
          "between the label and statement list in the case block");
    {
      line_indent();
      bool first = true;
      while (!check_any(switch_section_end_tokens,
                        ARRAY_SIZE(switch_section_end_tokens))) {
        if (!first) {
          line();
        }
        first = false;
        statement();
      }
      dedent();
    }
    line();
  }
  token(TOKEN_CLOSEBRACE, "at the end of the case block");
}

static void while_statement() {
  token(TOKEN_KW_WHILE, "at the beginning of a while loop");
  space();
  parenthesized_expression();
  embedded_statement(/*embedded*/ true);
}

static void do_statement() {
  token(TOKEN_KW_DO, "at the beginning of a do loop");
  embedded_statement(/*embedded*/ true);
  line();
  token(TOKEN_KW_WHILE, "after the body of a do loop");
  space();
  parenthesized_expression();
  token(TOKEN_SEMICOLON, "at the end of a do loop");
}

static void statement_expression_list(const char *where) {
  expression(where);
  while (match(TOKEN_COMMA)) {
    line();
    expression(where);
  }
}

static void for_initializer() {
  if (check_local_variable_declaration()) {
    local_variable_declaration();
  } else {
    statement_expression_list("in the initializer of a for loop");
  }
}
static void for_condition() { expression("in the condition of a for loop"); }
static void for_iterator() {
  statement_expression_list("in the iterator of a for loop");
}

static void for_statement() {
  token(TOKEN_KW_FOR, "at the beginning of a for loop");
  space();
  {
    group();
    token(TOKEN_OPENPAREN, "at the beginning of a for loop");
    {
      softline_indent();
      for_initializer();

      token(TOKEN_SEMICOLON, "between sections of a for loop");
      line();

      for_condition();

      token(TOKEN_SEMICOLON, "between sections of a for loop");
      line();

      for_iterator();
      dedent();
    }
    softline();
    token(TOKEN_CLOSEPAREN, "at the end of the for loop condition");
    end();
  }
  embedded_statement(/*embedded*/ true);
}

static void foreach_statement() {
  token(TOKEN_KW_FOREACH, "at the beginning of a foreach loop");
  space();
  {
    group();
    token(TOKEN_OPENPAREN, "at the beginning of a foreach loop");
    {
      softline_indent();
      local_variable_type();
      space();
      identifier("as the variable in a foreach loop");
      space();
      token(TOKEN_KW_IN, "in a foreach loop");
      line();
      expression("in the collection part of a foreach loop");
      dedent();
    }
    softline();
    token(TOKEN_CLOSEPAREN, "at the end of the foreach loop");
    end();
  }
  embedded_statement(/*embedded*/ true);
}

static void goto_statement() {
  token(TOKEN_KW_GOTO, "at the beginning of a goto statement");
  space();
  if (match(TOKEN_KW_CASE)) {
    space();
    identifier("in the case label in a goto statement");
  } else if (!match(TOKEN_KW_DEFAULT)) {
    identifier("in the label in a goto statement");
  }
  token(TOKEN_SEMICOLON, "at the end of a goto statement");
}

static void return_statement() {
  token(TOKEN_KW_RETURN, "at the beginning of a return statement");
  if (!check(TOKEN_SEMICOLON)) {
    space();
    expression("in the value of a return statement");
  }
  token(TOKEN_SEMICOLON, "at the end of a return statement");
}

static void throw_statement() {
  token(TOKEN_KW_THROW, "at the beginning of a throw statement");
  if (!check(TOKEN_SEMICOLON)) {
    space();
    expression("in the value of a throw statement");
  }
  token(TOKEN_SEMICOLON, "at the end of a throw statement");
}

static void try_statement() {
  token(TOKEN_KW_TRY, "at the beginning of a try block");
  line();
  block("at the beginning of the body of a try block");
  while (check(TOKEN_KW_CATCH)) {
    line();
    {
      group();
      token(TOKEN_KW_CATCH, "at the beginning of a catch block");
      space();
      token(TOKEN_OPENPAREN, "at the beginning of a catch block");
      type();
      if (check_identifier()) {
        space();
        identifier("in the variable declaration in a catch block");
      }
      token(TOKEN_CLOSEPAREN, "at the end of the declaration in a catch block");

      if (check(TOKEN_KW_WHEN)) {
        line_indent();
        token(TOKEN_KW_WHEN, "in a catch block");
        space();
        parenthesized_expression();
        dedent();
      }
      end();
    }
    line();
    block("at the beginning of the body of a catch block");
  }
  if (match(TOKEN_KW_FINALLY)) {
    line();
    block("in the body of a finally block");
  }
}

static void checked_statement() {
  token(TOKEN_KW_CHECKED, "at the beginning of a checked statement");
  line();
  block("in the body of a checked statement");
}

static void unchecked_statement() {
  token(TOKEN_KW_UNCHECKED, "at the beginning of an unchecked statement");
  line();
  block("in the body of an unchecked statement");
}

static void lock_statement() {
  token(TOKEN_KW_LOCK, "at the beginning of a lock statement");
  space();
  parenthesized_expression();
  embedded_statement(/*embedded*/ true);
}

static void using_statement() {
  token(TOKEN_KW_USING, "at the beginning of a using statement");
  space();
  {
    group();
    token(TOKEN_OPENPAREN,
          "at the beginning of the using statement, expression");
    {
      softline_indent();
      if (check_local_variable_declaration()) {
        local_variable_declaration();
      } else {
        expression("in the value of the resource in a using statement");
      }
      dedent();
    }
    softline();
    token(TOKEN_CLOSEPAREN, "at the end of the using statement expression");
    end();
  }
  embedded_statement(/*embedded*/ true);
}

static void yield_statement() {
  group();
  token(TOKEN_KW_YIELD, "at the beginning of a yield statement");
  space();
  if (!match(TOKEN_KW_BREAK)) {
    token(TOKEN_KW_RETURN, "or break in a yield statement");
    {
      line_indent();
      expression("in the value of a yield return statement");
      dedent();
    }
  }
  token(TOKEN_SEMICOLON, "at the end of a yield statement");
  end();
}

static void unsafe_statement() {
  token(TOKEN_KW_UNSAFE, "at the beginning of an unsafe block");
  line();
  block("in the body of an unsafe block");
}

static void fixed_statement() {
  token(TOKEN_KW_FIXED, "at the beginning of a fixed block");
  space();
  {
    group();
    token(TOKEN_OPENPAREN, "at the beginning of a fixed expression");
    {
      softline_indent();
      // N.B.: According to the spec this must be a pointer type and cannot use
      // 'var', but meh.
      local_variable_declaration();
      dedent();
    }
    softline();
    token(TOKEN_CLOSEPAREN, "at the end of a fixed expression");
    end();
  }
  embedded_statement(/*embedded*/ true);
}

// Pass 'true' for embedded if this is a true embedded statement: it controls
// whitespace and indentation. In particular, blocks are indented differently
// than everything else. Returns 'true' if it matched a block, otherwise
// 'false', so that we can consume optional semicolons properly.
static void embedded_statement(bool embedded) {
  if (check(TOKEN_OPENBRACE)) {
    if (embedded) {
      line();
    }
    block("at the beginning of a block");
  } else {
    if (embedded) {
      line_indent();
    }
    if (!match(TOKEN_SEMICOLON)) {
      switch (parser.current.type) {
      case TOKEN_KW_IF:
        if_statement();
        break;
      case TOKEN_KW_CASE:
        case_statement();
        break;
      case TOKEN_KW_WHILE:
        while_statement();
        break;
      case TOKEN_KW_DO:
        do_statement();
        break;
      case TOKEN_KW_FOR:
        for_statement();
        break;
      case TOKEN_KW_FOREACH:
        foreach_statement();
        break;

      case TOKEN_KW_BREAK:
      case TOKEN_KW_CONTINUE:
        single_token();
        token(TOKEN_SEMICOLON, "at the end of a break or continue");
        break;

      case TOKEN_KW_GOTO:
        goto_statement();
        break;

      case TOKEN_KW_RETURN:
        return_statement();
        break;

      case TOKEN_KW_THROW:
        throw_statement();
        break;

      case TOKEN_KW_TRY:
        try_statement();
        break;

      case TOKEN_KW_CHECKED:
        checked_statement();
        break;

      case TOKEN_KW_UNCHECKED:
        unchecked_statement();
        break;

      case TOKEN_KW_LOCK:
        lock_statement();
        break;

      case TOKEN_KW_USING:
        using_statement();
        break;

      case TOKEN_KW_YIELD:
        yield_statement();
        break;

      case TOKEN_KW_UNSAFE:
        unsafe_statement();
        break;

      case TOKEN_KW_FIXED:
        fixed_statement();
        break;

      default:
        expression("or some other statement");
        token(TOKEN_SEMICOLON, "at the end of an expression statement");
        break;
      }
    }

    if (embedded) {
      dedent();
    }
  }
}

static void statement() {
  // Label.
  if (check_identifier() && check_next(TOKEN_COLON)) {
    dedent();
    identifier("in a label identifier");
    token(TOKEN_COLON, "in a label");
    line_indent();
  }

  // Declaration?
  if (check_local_variable_declaration()) {
    local_variable_declaration();
  } else if (check(TOKEN_KW_CONST)) {
    local_const_declaration();
  } else {
    embedded_statement(/*embedded*/ false);
  }
}

// ============================================================================
// Member Declarations
// ============================================================================

static void attribute_name() { type_name(); }

static void attribute_arguments() {
  token(TOKEN_OPENPAREN, "at the beginning of attribute arguments");
  {
    softline_indent();
    while (!check(TOKEN_CLOSEPAREN) && !check(TOKEN_EOF)) {
      group();
      bool indented = false;
      if (check_name_equals()) {
        name_equals("in attribute arguments");

        line_indent();
        indented = true;
      }

      expression("in the argument of an attribute initializer");

      if (indented) {
        dedent();
      }
      end();
    }
    dedent();
  }
  softline();
  token(TOKEN_CLOSEPAREN, "at the end of attribute arguments");
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
  token(TOKEN_OPENBRACKET, "at the beginning of an attribute section");
  {
    softline_indent();
    if (check(TOKEN_IDENTIFIER) && check_next(TOKEN_COLON)) {
      identifier("in an attribute target");
      token(TOKEN_COLON, "after an attribute target");
      line();
    }

    attribute_list();
    dedent();
  }
  softline();
  token(TOKEN_CLOSEBRACKET, "at the end of an attribute section");
  end();
}

static bool attributes() {
  if (check(TOKEN_OPENBRACKET)) {
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

// These are shared through lots of different declarations; we allow all of them
// everywhere, even though it doesn't make sense to have e.g. `async struct`.
const static enum TokenType modifier_tokens[] = {
    TOKEN_KW_NEW,     TOKEN_KW_PUBLIC,   TOKEN_KW_PROTECTED, TOKEN_KW_INTERNAL,
    TOKEN_KW_PRIVATE, TOKEN_KW_ABSTRACT, TOKEN_KW_SEALED,    TOKEN_KW_STATIC,
    TOKEN_KW_UNSAFE,  TOKEN_KW_VIRTUAL,  TOKEN_KW_OVERRIDE,  TOKEN_KW_EXTERN,
    TOKEN_KW_ASYNC,   TOKEN_KW_READONLY,
};

static bool check_modifier() {
  return check_any(modifier_tokens, ARRAY_SIZE(modifier_tokens));
}

static bool match_modifier() {
  return match_any(modifier_tokens, ARRAY_SIZE(modifier_tokens));
}

static bool is_type_keyword(enum TokenType token);
static void type_declaration();

static void declaration_modifiers() {
  group();
  bool first = true;
  while (check_modifier()) {
    if (!first) {
      line();
    }
    first = false;

    match_modifier();
  }
  end();

  if (!first) {
    line();
  }
}

static void const_declaration() {
  attributes();
  declaration_modifiers();

  {
    group();

    token(TOKEN_KW_CONST, "at the beginning of a const declaration");
    space();
    type();
    {
      line_indent();
      bool first = true;
      while (check_identifier() || check(TOKEN_COMMA)) {
        if (!first) {
          token(TOKEN_COMMA, "between const member declarations");
          line();
        }

        group();
        identifier("in a const member declaration");
        space();
        token(TOKEN_EQUALS,
              "between the name and value of a const member declaration");
        {
          line_indent();
          expression("in the value of a const member declaration");
          dedent();
        }
        end();
      }

      dedent();
    }
    end();
  }
}

static void field_declaration() {
  attributes();
  declaration_modifiers();

  {
    group();
    type();
    variable_declarators("in a field declaration");
    end();
  }
}

static void return_type() {
  if (!match(TOKEN_KW_VOID)) {
    type();
  }
}

const static enum TokenType parameter_modifier_tokens[] = {
    TOKEN_KW_IN, TOKEN_KW_OUT, TOKEN_KW_REF, TOKEN_KW_THIS, TOKEN_KW_PARAMS,
};

static bool check_formal_parameter() {
  return check(TOKEN_OPENBRACKET) ||
         check_any(parameter_modifier_tokens,
                   ARRAY_SIZE(parameter_modifier_tokens)) ||
         check_type() || check(TOKEN_COMMA);
}

static void formal_parameter_list(const char *where) {
  group();
  token(TOKEN_OPENPAREN, where);
  {
    softline_indent();
    bool first = true;
    while (check_formal_parameter()) {
      if (!first) {
        token(TOKEN_COMMA, "between parameters");
        line();
      }
      first = false;

      {
        group();
        attributes();
        if (match_any(parameter_modifier_tokens,
                      ARRAY_SIZE(parameter_modifier_tokens))) {
          space();
        }
        type();
        space();
        identifier("in a parameter name");
        if (check(TOKEN_EQUALS)) {
          space();
          token(TOKEN_EQUALS,
                "between the name and default value of a parameter");
          {
            line_indent();
            expression("in the default value of a parameter");
            dedent();
          }
        }
        end();
      }
    }

    dedent();
  }
  token(TOKEN_CLOSEPAREN, "at the end of a parameter list");
  end();
}

static void type_constraint() {
  if (match(TOKEN_KW_NEW)) {
    token(TOKEN_OPENPAREN, "in constructor type constraint");
    token(TOKEN_CLOSEPAREN, "in constructor type constraint");
  } else {
    identifier("in type constraint");
  }
}

static void optional_type_parameter_list() {
  group();
  if (match(TOKEN_LESSTHAN)) {
    {
      softline_indent();
      attributes();
      if (match(TOKEN_KW_IN) || match(TOKEN_KW_OUT)) {
        space();
      }
      identifier("in a type parameter list");
      while (match(TOKEN_COMMA)) {
        line();
        attributes();
        if (match(TOKEN_KW_IN) || match(TOKEN_KW_OUT)) {
          space();
        }
        identifier("in a type parameter list");
      }

      dedent();
    }
    softline();
    token(TOKEN_GREATERTHAN, "at the end of a type parameter list");
  }
  end();
}

static void type_parameter_constraint() {
  group();
  token(TOKEN_KW_WHERE, "at the beginning of a type parameter constraint");
  space();
  identifier("in the name of a type parameter in a constraint");
  token(TOKEN_COLON, "after the name of a type parameter in a constraint");
  {
    line_indent();
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

static void member_name() {
  // This might seem weird, but since members can be qualified by interface type
  // (e.g., a.b<t,y>.c.interface.foo) they're basically indistinguishable from a
  // type name without an analysis pass, or without deciding to split off the
  // last segment, and none of that matters for what we're doing.
  type_name();
}

static void method_declaration() {
  // method header
  attributes();

  {
    group();
    {
      group();
      declaration_modifiers();
      if (match(TOKEN_KW_PARTIAL)) {
        line();
      }

      {
        group();
        return_type();
        line();
        member_name();
        end();
      }

      optional_type_parameter_list();
      end();
    }

    formal_parameter_list("in a method declaration");

    if (check(TOKEN_KW_WHERE)) {
      line_indent();
      type_parameter_constraint_clauses();
      dedent();
    }
    end();
  }

  if (!match(TOKEN_SEMICOLON)) {
    if (check(TOKEN_EQUALS_GREATERTHAN)) {
      line_indent();
      token(TOKEN_EQUALS_GREATERTHAN, "in the expression body of a method");
      {
        line_indent();
        expression("in the expression body of a method");
        token(TOKEN_SEMICOLON, "at the end of the expression body of a method");
        dedent();
      }
      dedent();
    } else {
      line();
      block("at the beginning of the body of a method");
    }
  }
}

const static enum TokenType accessor_modifiers[] = {
    TOKEN_KW_PROTECTED,
    TOKEN_KW_INTERNAL,
    TOKEN_KW_PRIVATE,
};

static bool check_accessor() {
  return check(TOKEN_OPENBRACKET) ||
         check_any(accessor_modifiers, ARRAY_SIZE(accessor_modifiers)) ||
         check(TOKEN_KW_GET) || check(TOKEN_KW_SET);
}

static void property_declaration() {
  attributes();
  declaration_modifiers();

  group();

  type();
  space();
  member_name();

  // property_body
  if (check(TOKEN_EQUALS_GREATERTHAN)) {
    // Simple "getter" property.
    space();
    token(TOKEN_EQUALS_GREATERTHAN,
          "at the beginning of the expression body of a property");
    {
      line_indent();
      expression("in the expression body of a property");
      token(TOKEN_SEMICOLON, "at the end of the expression body of a property");
      dedent();
    }
  } else {
    line();
    token(TOKEN_OPENBRACE,
          "at the beginning of the accessor declarations of a property");
    {
      line_indent();

      // accessor_declarations
      while (check_accessor()) {
        attributes();

        group();
        while (match_any(accessor_modifiers, ARRAY_SIZE(accessor_modifiers))) {
          space();
        }

        if (!match(TOKEN_KW_GET)) {
          token(TOKEN_KW_SET, "or get at the beginning of a property accessor");
        }
        if (!match(TOKEN_SEMICOLON)) {
          block("or semicolon at the beginning of the body of a property "
                "accessor");
        }
        end();

        line();
      }
      dedent();
    }
    token(TOKEN_CLOSEBRACE,
          "at the end of the accessor declarations of a property");

    // property_initializer?
    if (check(TOKEN_EQUALS)) {
      group();

      space();
      token(TOKEN_EQUALS,
            "at the beginning of a property initializer expression");
      {
        line_indent();
        expression("in a property initializer");
        token(TOKEN_SEMICOLON,
              "at the end of a property initializer expression");
        dedent();
      }
      end();
    }
  }

  end();
}

static void event_declaration() {
  attributes();
  declaration_modifiers();

  {
    group();
    token(TOKEN_KW_EVENT, "at the beginning of an event declaration");
    space();
    type();

    if (check_next(TOKEN_OPENBRACE)) {
      space();
      identifier("in the name of an event declaration");

      line();
      token(TOKEN_OPENBRACE, "at the beginning of an event declaration");
      {
        line_indent();
        while (check(TOKEN_KW_ADD) || check(TOKEN_KW_REMOVE) ||
               check(TOKEN_OPENBRACKET)) {
          attributes();

          group();
          if (!match(TOKEN_KW_ADD)) {
            token(TOKEN_KW_REMOVE,
                  "or add before an event accessor declaration");
          }
          line();
          block("as the body of an event accessor");
          end();

          line();
        }
        // Stuff

        dedent();
      }
      token(TOKEN_CLOSEBRACE, "at the end of an event declaration");
    } else {
      variable_declarators("in an event declaration");
    }

    end();
  }
}

static void indexer_declaration() {
  notimplemented("Not Implemented: Indexer");
  advance();
}

static void operator_declaration() {
  notimplemented("Not Implemented: Operator");
  advance();
}

static void destructor_declaration() {
  notimplemented("Not Implemented: Destructor");
  advance();
}

enum MemberKind {
  MEMBERKIND_NONE,
  MEMBERKIND_CONST,
  MEMBERKIND_FIELD,
  MEMBERKIND_METHOD,
  MEMBERKIND_PROPERTY,
  MEMBERKIND_EVENT,
  MEMBERKIND_INDEXER,
  MEMBERKIND_OPERATOR,
  MEMBERKIND_DESTRUCTOR,
  MEMBERKIND_TYPE,
};

static enum MemberKind check_member() {
  // OK this one sucks because we need to scan forward through tokens to figure
  // out what we're actually looking at. If we don't appear to be looking at a
  // member, then we return MEMBERKIND_NONE.
  int index = parser.index - 1;
  while (index < parser.buffer.count) {
    enum TokenType token = parser.buffer.tokens[index].type;
    index += 1;

    // Maybe this token has the clue about what we are...
    if (token == TOKEN_KW_CONST) {
      return MEMBERKIND_CONST;
    }
    if (token == TOKEN_KW_THIS) {
      return MEMBERKIND_INDEXER;
    }
    if (token == TOKEN_KW_EVENT) {
      return MEMBERKIND_EVENT;
    }
    if (token == TOKEN_KW_OPERATOR) {
      return MEMBERKIND_OPERATOR;
    }
    if (token == TOKEN_TILDE) {
      return MEMBERKIND_DESTRUCTOR; // Destructors are like methods.
    }
    if (token == TOKEN_KW_ASYNC) {
      return MEMBERKIND_METHOD; // Only methods can be async.
    }
    if (token == TOKEN_KW_PARTIAL) {
      return MEMBERKIND_METHOD; // Only methods can be partial.
    }

    if (is_type_keyword(token)) {
      return MEMBERKIND_TYPE;
    }

    if (token == TOKEN_EQUALS) {
      // Well.... if I have got this far and I see an '=' it must be a field.
      // (Otherwise it would have been a const.)
      return MEMBERKIND_FIELD;
    }
    if (token == TOKEN_OPENPAREN) {
      // ...must be the arg list of a method.
      return MEMBERKIND_METHOD;
    }
    if (token == TOKEN_OPENBRACE) {
      // If we see an open brace before a paren then it's a property.
      return MEMBERKIND_PROPERTY;
    }
    if (token == TOKEN_EQUALS_GREATERTHAN) {
      // If we see one of those arrow thingies it's definitely a property.
      return MEMBERKIND_PROPERTY;
    }
    if (token == TOKEN_SEMICOLON) {
      // No brace, no arrow, no special keyword, must be a field.
      return MEMBERKIND_FIELD;
    }
    if (token == TOKEN_CLOSEBRACE) {
      // Welp, I don't know, we missed all the clues somehow!
      return MEMBERKIND_NONE;
    }
  }

  return MEMBERKIND_NONE;
}

static void member_declarations() {
  enum MemberKind last_member_kind = MEMBERKIND_NONE;
  for (;;) {
    // Before we parse *anything* scan ahead to figure out what we're looking
    // at. That way we can figure out how much whitespace to put in.
    enum MemberKind member_kind = check_member();
    if (member_kind == MEMBERKIND_NONE) {
      break;
    }

    if (last_member_kind != MEMBERKIND_NONE) {
      // I've been around before, need to terminate the previous member.
      line();
      if (last_member_kind != member_kind ||
          (member_kind != MEMBERKIND_FIELD &&
           member_kind != MEMBERKIND_CONST)) {
        // Everything except fields gets a blank line in between.
        line();
      }
    }

    switch (member_kind) {
    case MEMBERKIND_CONST:
      const_declaration();
      break;

    case MEMBERKIND_FIELD:
      field_declaration();
      break;

    case MEMBERKIND_METHOD:
      method_declaration();
      break;

    case MEMBERKIND_PROPERTY:
      property_declaration();
      break;

    case MEMBERKIND_EVENT:
      event_declaration();
      break;

    case MEMBERKIND_INDEXER:
      indexer_declaration();
      break;

    case MEMBERKIND_OPERATOR:
      operator_declaration();
      break;

    case MEMBERKIND_DESTRUCTOR:
      destructor_declaration();
      break;

    case MEMBERKIND_TYPE:
      type_declaration();
      break;

    case MEMBERKIND_NONE:
    default:
      assert(!"Never reached");
      break;
    }

    last_member_kind = member_kind;
  }
}

// ============================================================================
// Type Declarations
// ============================================================================
static void base_types() {
  group();
  token(TOKEN_COLON, "before the list of base types");
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

static void class_declaration() {
  token(TOKEN_KW_CLASS, "at the beginning of a class declaration");
  space();
  identifier("in the name a class declaration");
  optional_type_parameter_list();

  if (check(TOKEN_COLON)) {
    line_indent();
    base_types();
    dedent();
  }

  if (check(TOKEN_KW_WHERE)) {
    line_indent();
    type_parameter_constraint_clauses();
    dedent();
  }

  line();
  token(TOKEN_OPENBRACE, "at the beginning of a class declaration");
  {
    line_indent();
    member_declarations();
    dedent();
  }
  line();
  token(TOKEN_CLOSEBRACE, "at the end of a class declaration");
  match(TOKEN_SEMICOLON);
}

static void struct_declaration() {
  token(TOKEN_KW_STRUCT, "at the beginning of a struct declaration");
  space();
  identifier("in the name of a struct declaration");
  optional_type_parameter_list();

  if (check(TOKEN_COLON)) {
    line_indent();
    base_types();
    dedent();
  }

  if (check(TOKEN_KW_WHERE)) {
    line_indent();
    type_parameter_constraint_clauses();
    dedent();
  }

  line();
  token(TOKEN_OPENBRACE, "at the beginning of a struct declaration");
  {
    line_indent();
    member_declarations();
    dedent();
  }
  line();
  token(TOKEN_CLOSEBRACE, "at the end of a struct declaration");
  match(TOKEN_SEMICOLON);
}

static void interface_declaration() {
  token(TOKEN_KW_INTERFACE, "at the beginning of an interface declaration");
  space();
  identifier("in the name of an interface declaration");
  optional_type_parameter_list();

  if (check(TOKEN_COLON)) {
    line_indent();
    base_types();
    dedent();
  }

  if (check(TOKEN_KW_WHERE)) {
    line_indent();
    type_parameter_constraint_clauses();
    dedent();
  }

  line();
  token(TOKEN_OPENBRACE, "at the beginning of an interface declaration");
  {
    line_indent();
    member_declarations();
    dedent();
  }
  line();
  token(TOKEN_CLOSEBRACE, "at the end of an interface declaration");
  match(TOKEN_SEMICOLON);
}

static void enum_declaration() {
  token(TOKEN_KW_ENUM, "at the beginning of an enum declaration");
  space();
  identifier("in the name of an enum declaration");

  if (match(TOKEN_COLON)) {
    line_indent();
    type();
    dedent();
  }

  line();
  token(TOKEN_OPENBRACE, "at the beginning of an enum declaration");
  {
    line_indent();

    bool first = true;
    while (check(TOKEN_OPENBRACKET) || check_identifier() ||
           check(TOKEN_COMMA)) {
      if (!first) {
        token(TOKEN_COMMA, "between enum members");
        line();
      }
      first = false;

      // TODO: Attributes here??
      identifier("in the name of an enum member");
      if (check(TOKEN_EQUALS)) {
        space();
        token(TOKEN_EQUALS, "between the name and the value of an enum member");
        {
          line_indent();
          expression("in the value of an enum member");
          dedent();
        }
      }
    }

    dedent();
  }
  token(TOKEN_CLOSEBRACE, "at the end of an enum declaration");

  match(TOKEN_SEMICOLON);
}

static void delegate_declaration() {
  group();
  token(TOKEN_KW_DELEGATE, "at the beginning of a delegate declaration");
  space();
  return_type();
  line();
  identifier("in the name of a delegate declaration");
  optional_type_parameter_list();

  formal_parameter_list("in a delegate declaration");

  if (check(TOKEN_KW_WHERE)) {
    line_indent();
    type_parameter_constraint_clauses();
    dedent();
  }

  end();
}

const static enum TokenType type_keyword_tokens[] = {
    TOKEN_KW_CLASS, TOKEN_KW_STRUCT,   TOKEN_KW_INTERFACE,
    TOKEN_KW_ENUM,  TOKEN_KW_DELEGATE,
};

static bool check_type_keyword() {
  return check_any(type_keyword_tokens, ARRAY_SIZE(type_keyword_tokens));
}

static bool is_type_keyword(enum TokenType token) {
  for (size_t i = 0; i < ARRAY_SIZE(type_keyword_tokens); i++) {
    if (type_keyword_tokens[i] == token) {
      return true;
    }
  }
  return false;
}

static bool check_type_declaration() {
  return check(TOKEN_OPENBRACKET) || check_modifier() || check_type_keyword() ||
         check(TOKEN_KW_PARTIAL);
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
    error("Expected some kind of type keyword.");
  }
}

// ============================================================================
// Namespaces, Compilation Units
// ============================================================================

static void extern_alias() {
  token(TOKEN_KW_EXTERN, "at the beginning of an extern alias");
  space();
  token(TOKEN_KW_ALIAS, "at the beginning of an extern alias");
  space();
  identifier("in the name of an extern alias");
  token(TOKEN_SEMICOLON, "at the end of an extern alias");
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

static void using_directive() {
  group();
  token(TOKEN_KW_USING, "at the beginning of a using directive");
  space();

  if (match(TOKEN_KW_STATIC)) {
    space();
  }

  bool indented = false;
  if (check_name_equals()) {
    name_equals("in a using directive");

    indented = true;
    line_indent();
  }

  namespace_or_type_name("in the name of a using directive");
  token(TOKEN_SEMICOLON, "at the end of a using directive");

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

static void namespace_declaration();

static void namespace_members() {
  bool first = true;
  for (;;) {
    if (check(TOKEN_KW_NAMESPACE)) {
      if (!first) {
        // Blank lines between members.
        line();
        line();
      }

      namespace_declaration();
    } else if (check_type_declaration()) {
      if (!first) {
        // Blank lines between members.
        line();
        line();
      }

      type_declaration();
    } else {
      break;
    }
    first = false;
  }
}

static void namespace_body() {
  extern_alias_directives();
  using_directives();
  namespace_members();
}

static void qualified_identifier(const char *where) {
  identifier(where);
  while (match(TOKEN_DOT)) {
    identifier(where);
  }
}

static void namespace_declaration() {
  token(TOKEN_KW_NAMESPACE, "at the beginning of a namespace declaration");
  space();
  qualified_identifier("in the name of a namespace declaration");
  line();
  token(TOKEN_OPENBRACE, "at the beginning of a namespace declaration");

  {
    line_indent();
    namespace_body();
    dedent();
  }

  line();
  token(TOKEN_CLOSEBRACE, "at the end of a namespace declaration");
  match(TOKEN_SEMICOLON);
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

  parser.buffer = scan_tokens(source);
  parser.index = 0;
  parser.trivia_index = 0;
  parser.has_trivia = false;

  advance();

  compilation_unit();
  line();

  token(TOKEN_EOF, "at the end of the file");

  return !parser.had_error;
}
