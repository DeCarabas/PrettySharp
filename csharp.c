/*
prettysharp
Copyright (C) 2019 John Doty

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/
#include "csharp.h"
#include "lexer.h"
#include "token.h"

// #define BREAK_ON_STUCK

struct Parser {
  struct DocBuilder *builder;
  struct TokenBuffer buffer;
  int index;

#ifdef BREAK_ON_STUCK
  int loop_count;
#endif

  struct Token current;
  struct Token previous;

  int trivia_index;
  bool has_trivia;

  bool last_was_line;

  bool check_only;
  bool had_error;
};

struct Parser parser;

static void parser_init(struct DocBuilder *builder, struct TokenBuffer buffer) {
  parser.builder = builder;
  parser.buffer = buffer;
  parser.index = 0;

#ifdef BREAK_ON_STUCK
  parser.loop_count = 0;
#endif

  memset(&parser.current, 0, sizeof(struct Token));
  memset(&parser.previous, 0, sizeof(struct Token));

  parser.trivia_index = 0;
  parser.has_trivia = false;

  parser.last_was_line = true;
  parser.had_error = false;
  parser.check_only = false;
}

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
  parser.had_error = true;
  if (parser.check_only) {
#ifdef PRINT_DEBUG_ENABLED
    DEBUG_("vv SUPPRESSED ERROR vv");
    vDEBUG(format, args);
    DEBUG_("^^ SUPPRESSED ERROR ^^");
#endif
    return;
  }

  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(stderr, " at '%.*s'", (int)(token->length), token->start);
  }

  fprintf(stderr, ": ");
  vfprintf(stderr, format, args);
  fprintf(stderr, "\n");

  // N.B.: Resync is hard and we're not doing anything if we get a parse error
  // anyway; right now we'll just abort here.
  exit(ERR_PARSE_ERROR);
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
/* static void notimplemented(const char *format, ...) { */
/*   va_list args; */
/*   va_start(args, format); */
/*   verror(format, args); */
/*   va_end(args); */
/* } */

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
static void flush_trivia(bool next_is_line) {
  if (!parser.has_trivia) {
    return;
  }

  bool alone_on_line = false;
  bool last_was_line = parser.last_was_line;
  for (; parser.trivia_index < parser.index; parser.trivia_index++) {
    struct Token trivia = parser.buffer.tokens[parser.trivia_index];
    if (trivia.type == TOKEN_TRIVIA_BLOCK_COMMENT) {
      DEBUG(("Handling block comment"));
      if (!last_was_line) {
        if (alone_on_line) {
          doc_line(parser.builder);
        } else {
          doc_text(parser.builder, " ", 1);
        }
      }

      doc_text(parser.builder, trivia.start, trivia.length);
    } else if (trivia.type == TOKEN_TRIVIA_LINE_COMMENT) {
      DEBUG(("Handling line comment"));
      if (!last_was_line) {
        if (alone_on_line) {
          doc_line(parser.builder);
        } else {
          doc_text(parser.builder, " ", 1);
        }
      }

      doc_breakparent(parser.builder);
      doc_text(parser.builder, trivia.start, trivia.length);
    } else if (trivia.type == TOKEN_TRIVIA_DIRECTIVE) {
      DEBUG(("Handling directive"));

      if (!last_was_line) {
        doc_line(parser.builder); // TODO: Maintain indent?
      }

      doc_breakparent(parser.builder);
      doc_text(parser.builder, trivia.start, trivia.length);
    } else if (trivia.type == TOKEN_TRIVIA_EOL) {
      if (next_is_line) {
        // We're only flushing up to the end of the line; let everything else
        // take it's course naturally. (Leave has_trivia set to true.) This
        // causes trailing comments to stick to the token before them,
        // generally, but leaves any additional comments sticking to the
        // trailing tokens, where they should be.
        return;
      }
      alone_on_line = true;
      continue;
    } else {
      continue;
    }

    if (!next_is_line) {
      doc_line(parser.builder);
      last_was_line = true;
      alone_on_line = true;
    } else {
      last_was_line = false;
    }
  }

  parser.last_was_line = last_was_line;
  parser.has_trivia = false;
}

// ============================================================================
// Formatting
// ============================================================================

static void text(struct Token token) {
  flush_trivia(/*next_is_line*/ false);
  parser.last_was_line = false;
  doc_text(parser.builder, token.start, token.length);
}
static void group() {
  flush_trivia(/*next_is_line*/ false);
  doc_group(parser.builder);
}
static void end() { doc_end(parser.builder); }
static void indent() { doc_indent(parser.builder); }
static void dedent() { doc_dedent(parser.builder); }
static void line() {
  flush_trivia(/*next_is_line*/ true);
  doc_line(parser.builder);
  parser.last_was_line = true;
}
static void line_indent() {
  indent();
  line();
}
static void softline() {
  flush_trivia(/*next_is_line*/ true);
  doc_softline(parser.builder);
  parser.last_was_line = true;
}
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
#ifdef BREAK_ON_STUCK
  parser.loop_count = 0;
#endif

  parser.previous = parser.current;
  parser.trivia_index = parser.index;
  for (;;) {
    if (parser.index == parser.buffer.count) {
      break;
    }
    parser.current = parser.buffer.tokens[parser.index];
    parser.index += 1;

    if (parser.current.type == TOKEN_TRIVIA_BLOCK_COMMENT ||
        parser.current.type == TOKEN_TRIVIA_LINE_COMMENT ||
        parser.current.type == TOKEN_TRIVIA_DIRECTIVE) {
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
        token.type == TOKEN_TRIVIA_WHITESPACE ||
        token.type == TOKEN_TRIVIA_DIRECTIVE) {
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

/* static bool check_next_identifier() { */
/*   int i = parser.index; */
/*   struct Token token = next_significant_token(&i); */
/*   return is_identifier_token(token.type); */
/* } */

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
#ifdef BREAK_ON_STUCK
  parser.loop_count++;
  if (parser.loop_count == 100000) {
    fprintf(stderr,
            "%4d We're stuck!! Looking for '%s' at '%s' on source line %d\n",
            line, token_text(type), token_text(parser.current.type),
            parser.current.line);
    abort();
  }
#else
  UNUSED(line);
#endif

  bool result = parser.current.type == type;
  DEBUG(("%4d Check %s == %s -> %s", line, token_text(type),
         token_text(parser.current.type), result ? "true" : "false"));
  return result;
}

#define check(t) check_(t, __LINE__)

static bool check_is_any_(enum TokenType type, const enum TokenType *types,
                          int count, int line) {
#ifndef PRINT_DEBUG_ENABLED
  UNUSED(line);
#endif

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

static bool check_expensive(void (*parse_func)(), struct Token *token,
                            int *index) {
  struct Parser saved_parser = parser;
  parser.had_error = false;
  parser.check_only = true;
  parser.index = *index;
  parser.current = *token;

  // TODO: Probably core should give me a way to save and restore builders.
  struct DocBuilder saved_builder = *parser.builder;

  parse_func();

  bool success = !parser.had_error;
  *token = parser.current;
  *index = parser.index;

  parser = saved_parser;
  parser.builder->count = saved_builder.count;
  parser.builder->margin = saved_builder.margin;
  parser.builder->indent = saved_builder.indent;
  parser.builder->group_depth = saved_builder.group_depth;

  return success;
}

// ============================================================================
// Names
// ============================================================================

static bool check_identifier() {
  bool result = is_identifier_token(parser.current.type);
  DEBUG(("Check identifier %s == %s", token_text(parser.current.type),
         result ? "true" : "false"));
  return result;
}

static void identifier(const char *where) {
  if (check_identifier()) {
    DEBUG(("Identifier '%.*s' %s", parser.current.length, parser.current.start,
           where));
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

static bool check_is_type(int *index, struct Token *token, bool array);
static void type(const char *where);

static bool check_type_argument_list() {
  int index = parser.index;
  struct Token token = parser.current;
  if (token.type != TOKEN_LESSTHAN) {
    return false;
  }

  token = next_significant_token(&index);
  while (token.type != TOKEN_GREATERTHAN) {
    if (!check_is_type(&index, &token, /*array*/ true)) {
      return false;
    }

    while (token.type == TOKEN_COMMA) {
      token = next_significant_token(&index);
      if (!check_is_type(&index, &token, /*array*/ true)) {
        return false;
      }
    }
  }

  if (token.type != TOKEN_GREATERTHAN) {
    return false;
  }

  return true;
}

static void optional_type_argument_list() {
  if (check_type_argument_list()) {
    group();
    token(TOKEN_LESSTHAN, "at the beginnign of a type argument list");
    if (!check(TOKEN_GREATERTHAN)) {
      softline_indent();
      type("in an optional type argument list");
      while (match(TOKEN_COMMA)) {
        line();
        type("in an optional type argument list");
      }
      dedent();
    }
    softline();
    token(TOKEN_GREATERTHAN, "at the end of a type argument list");
    end();
  }
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

static void type_name(const char *where) { namespace_or_type_name(where); }

static bool check_name_equals() {
  return check(TOKEN_IDENTIFIER) && check_next(TOKEN_EQUALS);
}

const static enum TokenType builtin_type_tokens[] = {
    TOKEN_KW_SBYTE, TOKEN_KW_BYTE,   TOKEN_KW_SHORT,   TOKEN_KW_USHORT,
    TOKEN_KW_INT,   TOKEN_KW_UINT,   TOKEN_KW_LONG,    TOKEN_KW_ULONG,
    TOKEN_KW_CHAR,  TOKEN_KW_FLOAT,  TOKEN_KW_DOUBLE,  TOKEN_KW_DECIMAL,
    TOKEN_KW_BOOL,  TOKEN_KW_OBJECT, TOKEN_KW_DYNAMIC, TOKEN_KW_STRING,
};

static void tuple_element_type() {
  group();
  type("in the element type of a tuple element");
  if (check_identifier()) {
    line_indent();
    identifier("as the name in a tuple type");
    dedent();
  }
  end();
}

static void non_array_type(const char *where) {
  if (match(TOKEN_OPENPAREN)) {
    softline_indent();
    tuple_element_type();
    do {
      token(TOKEN_COMMA, "between elements of a tuple type");
      line();
      tuple_element_type();
    } while (check(TOKEN_COMMA));

    dedent();
    softline();
    token(TOKEN_CLOSEPAREN, "at the end of a tuple type");
  } else if (!match_any(builtin_type_tokens, ARRAY_SIZE(builtin_type_tokens))) {
    type_name(where);
  }

  while (check(TOKEN_QUESTION) || check(TOKEN_ASTERISK)) {
    // Nullable.
    match(TOKEN_QUESTION);

    // Pointer.
    match(TOKEN_ASTERISK);
  }
}

static void rank_specifier() {
  if (!match(TOKEN_QUESTION_OPENBRACKET)) {
    token(TOKEN_OPENBRACKET, "in array rank specifier");
  }
  while (match(TOKEN_COMMA)) {
    ;
  }
  token(TOKEN_CLOSEBRACKET, "in array rank specifier");
}

static void type(const char *where) {
  non_array_type(where);

  // Handle all the stuff at the end.
  // N.B.: TOKEN_QUESTION_OPENBRACKET is the result of a design decision that
  // makes the pratt expression parser work better, but makes this part a
  // little....
  //
  //   •_•)
  //   ( •_•)>⌐■-■
  //   (⌐■_■)
  //
  //       *questionable*.
  while (check(TOKEN_OPENBRACKET) || check(TOKEN_QUESTION) ||
         check(TOKEN_QUESTION_OPENBRACKET) || check(TOKEN_ASTERISK)) {

    // Array ranks.
    if (check(TOKEN_OPENBRACKET) || check(TOKEN_QUESTION_OPENBRACKET)) {
      rank_specifier();
    }

    // Nullable.
    match(TOKEN_QUESTION);

    // Pointer.
    match(TOKEN_ASTERISK);
  }
}

static bool check_is_type(int *index, struct Token *token, bool array) {
  DEBUG(("Checking For Type...."));
  bool result = check_expensive(array ? type : non_array_type, token, index);
  DEBUG(("      ... check for type: %s", result ? "true" : "false"));
  return result;
}

static bool check_type() {
  int index = parser.index;
  struct Token token = parser.current;
  return check_is_type(&index, &token, /*array*/ true);
}

static bool check_non_array_type() {
  int index = parser.index;
  struct Token token = parser.current;
  return check_is_type(&index, &token, /*array*/ false);
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
    error("Expect expression %s", where);
    return;
  }

  prefix_rule();

  while (precedence <= get_rule(parser.current.type)->precedence) {
    ParseFn infix_rule = get_rule(parser.current.type)->infix;
    infix_rule();
  }
  end();
}

static void inline_block(const char *where);

static bool check_implicitly_typed_lambda() {
  int index = parser.index;
  struct Token token = parser.current;
  if (token.type == TOKEN_KW_ASYNC) {
    token = next_significant_token(&index);
  }

  if (!is_identifier_token(token.type)) {
    return false;
  }

  token = next_significant_token(&index);
  if (token.type != TOKEN_EQUALS_GREATERTHAN) {
    return false;
  }
  return true;
}

static void implicitly_typed_lambda() {
  group();
  {
    group();
    if (match(TOKEN_KW_ASYNC)) {
      line();
    }
    identifier("in implicitly typed lambda");
    space();
    token(TOKEN_EQUALS_GREATERTHAN, "in implicitly typed lambda");
    end();
  }
  if (check(TOKEN_OPENBRACE)) {
    space();
    inline_block("at the beginning of the body of a lambda");
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
  if (check_implicitly_typed_lambda()) {
    implicitly_typed_lambda();
  } else if (check_identifier()) {
    simple_name("in a primary expression");
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

    group();
    bool indented = false;
    if (check_identifier() && check_next(TOKEN_COLON)) {
      identifier("in the name of a named tuple element");
      token(TOKEN_COLON,
            "between the name and the expression in a named tuple element");
      line_indent();
      indented = true;
    }

    expression("between parentheses in a tuple or parenthesized expression");
    if (indented) {
      dedent();
      indented = false;
    }

    while (match(TOKEN_COMMA)) {
      end();
      line();
      group();

      if (check_identifier() && check_next(TOKEN_COLON)) {
        identifier("in the name of a named tuple element");
        token(TOKEN_COLON,
              "between the name and the expression in a named tuple element");
        line_indent();
        indented = true;
      }

      expression("in a tuple element");
      if (indented) {
        dedent();
      }
    }

    end();
    dedent();
  }
  softline();
  token(TOKEN_CLOSEPAREN, "in parenthesized expression");
  end();
}

static void checked_expression() {
  group();
  token(TOKEN_KW_CHECKED, "at the beginning of a checked expression");
  token(TOKEN_OPENPAREN, "after 'checked' in a checked expression");
  {
    softline_indent();
    expression("between the parentheses of a checked expression");
    dedent();
  }
  softline();
  token(TOKEN_CLOSEPAREN, "at the end of a checked expression");
  end();
}

static void unchecked_expression() {
  group();
  token(TOKEN_KW_UNCHECKED, "at the beginning of a checked expression");
  token(TOKEN_OPENPAREN, "after 'checked' in a checked expression");
  {
    softline_indent();
    expression("between the parentheses of a checked expression");
    dedent();
  }
  softline();
  token(TOKEN_CLOSEPAREN, "at the end of a checked expression");
  end();
}

static void null_member_access() {
  token(TOKEN_QUESTION_DOT, "in null conditional member access expression");
  identifier("in null conditional member access expression");
  optional_type_argument_list();
}

static bool check_local_variable_declaration();
static void local_variable_type();

static void argument_list_inner(enum TokenType closing_type) {
  if (!check(closing_type)) {
    softline_indent();
    group();
    if (check(TOKEN_IDENTIFIER) && check_next(TOKEN_COLON)) {
      identifier("in the name of a named argument");
      token(TOKEN_COLON, "after the name of a named argument");
      space();
    }

    if (match(TOKEN_KW_OUT)) {
      space();
      if (check_local_variable_declaration()) {
        local_variable_type();
        space();
        identifier("in an inline out parameter declaration");
      } else {
        expression("as the value of an out argument");
      }
    } else {
      if (match(TOKEN_KW_IN) || match(TOKEN_KW_REF)) {
        space();
      }
      expression("as the value of an argument");
    }

    while (match(TOKEN_COMMA)) {
      end();

      line();

      group();
      if (check(TOKEN_IDENTIFIER) && check_next(TOKEN_COLON)) {
        identifier("in the name of a named argument");
        token(TOKEN_COLON, "after the end of a named argument");
        space();
      }

      if (match(TOKEN_KW_OUT)) {
        space();
        if (check_local_variable_declaration()) {
          local_variable_type();
          space();
          identifier("in an inline out parameter declaration");
        } else {
          expression("as the value of an out argument");
        }
      } else {
        if (match(TOKEN_KW_IN) || match(TOKEN_KW_REF)) {
          space();
        }
        expression("as the value of an argument");
      }
    }
    end();
    dedent();
    softline();
  }
}

static void null_element_access() {
  token(TOKEN_QUESTION_OPENBRACKET,
        "in null conditional element access expression");
  argument_list_inner(TOKEN_CLOSEBRACKET);
  token(TOKEN_CLOSEBRACKET,
        "at the end of a null conditional element access expression");
}

static void element_access() {
  token(TOKEN_OPENBRACKET, "in an element access expression");
  argument_list_inner(TOKEN_CLOSEBRACKET);
  token(TOKEN_CLOSEBRACKET, "at the end of an element access expression");
}

static bool check_parenthesized_implicitly_typed_lambda() {
  // Case 1: ( x ,
  // Case 2: ( x ) =>
  // Case 3: ( ) =>
  int index = parser.index;
  struct Token token = parser.current;
  if (token.type == TOKEN_KW_ASYNC) {
    token = next_significant_token(&index);
  }

  if (token.type != TOKEN_OPENPAREN) {
    return false;
  }

  token = next_significant_token(&index);
  if (is_identifier_token(token.type)) {
    token = next_significant_token(&index);
    if (token.type == TOKEN_COMMA) {
      // This scan is to disambiguate between a lambda and a tuple.
      token = next_significant_token(&index);
      while (token.type == TOKEN_COMMA || is_identifier_token(token.type) ||
             check_is_any(token.type, builtin_type_tokens,
                          ARRAY_SIZE(builtin_type_tokens))) {
        token = next_significant_token(&index);
      }

      // ) =>
      if (token.type != TOKEN_CLOSEPAREN) {
        return false;
      }
      token = next_significant_token(&index);
      return token.type == TOKEN_EQUALS_GREATERTHAN; // 1
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
    if (match(TOKEN_KW_ASYNC)) {
      line();
    }
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
    inline_block("at the beginning of the body of a lambda");
  } else {
    line_indent();
    group();
    expression("in the body of a lambda");
    end();
    dedent();
  }
  end();
}

static enum TokenType anon_func_param_modifier[] = {
    TOKEN_KW_REF,
    TOKEN_KW_OUT,
    TOKEN_KW_IN,
};

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
  bool requires_scan = true;
  int index = parser.index;
  struct Token token = parser.current;
  if (token.type == TOKEN_KW_ASYNC) {
    token = next_significant_token(&index);
  }

  if (token.type != TOKEN_OPENPAREN) {
    DEBUG(("explicit paren lambda: no openparen -> false"));
    return false;
  }

  token = next_significant_token(&index);
  if (check_is_any(token.type, anon_func_param_modifier,
                   ARRAY_SIZE(anon_func_param_modifier))) {
    token = next_significant_token(&index);

    // TODO: Why do we even continue here? The official C# parser does, but
    // could this really be anything else?
    requires_scan = false;
  }

  if (!check_is_type(&index, &token, /*array*/ true)) {
    DEBUG(("explicit paren lambda: no type -> false"));
    return false;
  }

  if (!is_identifier_token(token.type)) {
    DEBUG(("explicit paren lambda: no variable identifier -> false"));
    return false;
  }

  token = next_significant_token(&index);
  if (token.type != TOKEN_CLOSEPAREN && token.type != TOKEN_COMMA) {
    DEBUG(("explicit paren lambda: no ) or , -> false"));
    return false;
  }

  if (requires_scan) {
    while (token.type != TOKEN_CLOSEPAREN) {
      token = next_significant_token(&index);
      if (token.type == TOKEN_EOF || token.type == TOKEN_SEMICOLON) {
        DEBUG(("explicit paren lambda: didn't find ) -> false"));
        return false;
      }
    }

    token = next_significant_token(&index);
    if (token.type != TOKEN_EQUALS_GREATERTHAN) {
      DEBUG(("explicit paren lambda: no '=>' -> false"));
      return false;
    }
  }

  return true;
}

static void formal_parameter_list(const char *where);

static void parenthesized_explicitly_typed_lambda() {
  group();
  {
    group();
    if (match(TOKEN_KW_ASYNC)) {
      line();
    }
    formal_parameter_list(
        "at the beginning of a parenthesized, explicitly typed lambda");
    space();
    token(TOKEN_EQUALS_GREATERTHAN, "in parenthesized lambda");
    end();
  }
  if (check(TOKEN_OPENBRACE)) {
    space();
    inline_block("at the beginning of the body of an explicitly typed lambda");
  } else {
    line_indent();
    group();
    expression("in the expression body of an explicitly typed lambda");
    end();
    dedent();
  }
  end();
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
    DEBUG(("Check cast: false (not open paren)"));
    return false;
  }

  int index = parser.index;
  struct Token token = next_significant_token(&index);
  if (!check_is_type(&index, &token, /*array*/ true)) {
    DEBUG(("Check cast: false (not is type)"));
    return false;
  }

  if (token.type != TOKEN_CLOSEPAREN) {
    DEBUG(("Check cast: false (not close paren)"));
    return false;
  }

  token = next_significant_token(&index);
  if (check_is_any(token.type, cannot_follow_cast_tokens,
                   ARRAY_SIZE(cannot_follow_cast_tokens))) {
    DEBUG(("Check cast: false (token cannot follow)"));
    return false;
  }

  DEBUG(("Check cast: true"));
  return true;
}

static void cast() {
  token(TOKEN_OPENPAREN, "at the beginning of a type cast");
  type("in a type cast expression");
  token(TOKEN_CLOSEPAREN, "after the type in a type cast");
  parse_precedence(PREC_UNARY, "after a type cast");
}

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

static void argument_list() {
  group();
  token(TOKEN_OPENPAREN, "at the beginning of an argument list");
  argument_list_inner(TOKEN_CLOSEPAREN);
  token(TOKEN_CLOSEPAREN, "at the end of an argument list");
  end();
}

static void invocation() { argument_list(); }

static void pointer_indirection() {
  token(TOKEN_ASTERISK, "to dereference a pointer");
  parse_precedence(PREC_UNARY, "to the right of pointer indirection");
}

static void unary_prefix() {
  single_token();
  expression("to the right of a unary operator");
}

static void unary_postfix() { single_token(); }

static void anonymous_method_expression() {
  token(TOKEN_KW_DELEGATE, "at the beginning of an anonymous method");
  if (check(TOKEN_OPENPAREN)) {
    space();
    formal_parameter_list("at the beginning of an anonymous method");
  }
  line();
  inline_block("at the beginning of the body of an anonymous method");
}

static void async_lambda_or_delegate() {
  if (check_next(TOKEN_KW_DELEGATE)) {
    token(TOKEN_KW_ASYNC, "in an async expression?");
    space();
    anonymous_method_expression();
  } else if (check_implicitly_typed_lambda()) {
    implicitly_typed_lambda();
  } else if (check_parenthesized_implicitly_typed_lambda()) {
    parenthesized_implicitly_typed_lambda();
  } else if (check_parenthesized_explicitly_typed_lambda()) {
    parenthesized_explicitly_typed_lambda();
  } else {
    error("Expected some kind of lambda or delegate");
  }
}

static void await_expression() {
  group();
  token(TOKEN_KW_AWAIT, "in await expression");
  line_indent();
  parse_precedence(PREC_UNARY, "to the right of await");
  dedent();
  end();
}

static void typeof_expression() {
  group();
  token(TOKEN_KW_TYPEOF, "at the beginning of a typeof expression");
  token(TOKEN_OPENPAREN,
        "at the beginning of the argument to a typeof expression");
  {
    softline_indent();
    type("in the type of a typeof expression");
    dedent();
  }
  softline();
  token(TOKEN_CLOSEPAREN, "at the end of the argument to a typeof expression");

  end();
}

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
        // I'm in a collection initializer and this is an element; it's going
        // to be an expression list just like in an array initializer.
        collection_initializer();
      } else if (!check(TOKEN_CLOSEBRACE)) {
        if (match(TOKEN_OPENBRACKET)) {
          argument_list_inner(TOKEN_CLOSEBRACKET);
          token(TOKEN_CLOSEBRACKET, "at the end of an object initializer");
        } else {
          // In an object initializer this is technically only allowed to be
          // an identifier. But in a collection this can be anything. And in
          // an anonymous object expression this can be 'this.' or 'base.' or
          // several other interesting forms. And I don't feel like building
          // parsers for all those distinct forms right now, mainly because
          // they're very difficult to distinguish from each other, and I
          // can't be bothered. So. This allows more forms than it technically
          // should, but I'm not worried.
          expression("in the member name of an object initializer");
        }
        if (check(TOKEN_EQUALS)) {
          space();
          token(TOKEN_EQUALS, "between the member and the expression in an "
                              "object initializer");
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
  if (!match(TOKEN_QUESTION_OPENBRACKET)) {
    token(TOKEN_OPENBRACKET, where);
  }
  if (!check(TOKEN_CLOSEBRACKET)) {
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
  if (check_non_array_type()) {
    space();
    non_array_type("in an object creation expression");
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
    } else if (check(TOKEN_OPENBRACKET) || check(TOKEN_QUESTION_OPENBRACKET)) {
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
          "initialization");
  }
}

static void from_clause() {
  group();
  token(TOKEN_KW_FROM, "at the beginning of a from clause");
  {
    line_indent();
    {
      group();
      if (!(check_identifier() && check_next(TOKEN_KW_IN))) {
        type("in the type of the variable in a from clause");
        line();
      }
      identifier("in the variable of a from clause");
      end();
    }
    line();
    {
      group();
      token(TOKEN_KW_IN,
            "between the variable and the expression in a from clause");
      line();
      expression("at the end of a from clause");
      end();
    }
    dedent();
  }
  end();
}

static void let_clause() {
  group();
  {
    group();
    token(TOKEN_KW_LET, "at the beginning of a let clause");
    line();
    {
      group();
      identifier("in the variable of a let clause");
      line();
      token(TOKEN_EQUALS,
            "between the variable and the expression of a let clause");
      end();
    }
    end();
  }
  {
    line_indent();
    {
      group();
      expression("as the value of a let clause");
      end();
    }
    dedent();
  }
  end();
}

static void where_clause() {
  group();
  token(TOKEN_KW_WHERE, "at the beginning of a where clause");
  {
    line_indent();
    {
      group();
      expression("in the predicate of a where clause");
      end();
    }
    dedent();
  }
  end();
}

static void join_clause() {
  group();
  token(TOKEN_KW_JOIN, "at the beginning of a join clause");
  line_indent();
  {
    group();
    {
      group();
      if (!(check_identifier() && check_next(TOKEN_KW_IN))) {
        type("in the type of the variable in a join clause");
        line();
      }
      identifier("in the variable of a join clause");
      end();
    }
    line();
    token(TOKEN_KW_IN, "between the variable and collection in a join clause");
    line();
    expression("in the collection of a join clause");
    end();
  }

  line();

  {
    group();
    token(TOKEN_KW_ON,
          "between the collection and the condition in a join clause");
    line();
    {
      group();
      expression("on the left of the equality in a join clause");
      line();
      token(TOKEN_KW_EQUALS, "between the two sides of a join condition");
      line();
      expression("on the right of the equality in a join clause");
      end();
    }
    end();
  }

  if (check(TOKEN_KW_INTO)) {
    line();

    group();
    token(TOKEN_KW_INTO, "after the condition in a join into clause");
    space();
    identifier("in the target variable of a join into clause");
    end();
  }
  dedent();
  end();
}

static void orderby_clause() {
  group();
  token(TOKEN_KW_ORDERBY, "at the beginning of an orderby clause");
  {
    line_indent();
    bool first = true;
    while (first || match(TOKEN_COMMA)) {
      if (!first) {
        end();
      }
      first = false;
      group();
      expression("in the value of an ordering");
      line();
      if (!match(TOKEN_KW_ASCENDING)) {
        match(TOKEN_KW_DESCENDING);
      }
    }
    end();
    dedent();
  }
  end();
}

static void select_clause() {
  group();
  token(TOKEN_KW_SELECT, "at the beginning of a select clause");
  {
    line_indent();
    expression("in the value of a select clause");
    dedent();
  }
  end();
}

static void group_clause() {
  group();
  token(TOKEN_KW_GROUP, "at the beginning of a group clause");
  {
    line_indent();
    {
      group();
      expression("in the value to group in a group clause");
      line();
      token(TOKEN_KW_BY,
            "between the value to group and the value to group by");
      line();
      expression("in the value to group by in a group clause");
      end();
    }
    dedent();
  }
  end();
}

static void select_or_group_clause() {
  if (check(TOKEN_KW_SELECT)) {
    select_clause();
  } else if (check(TOKEN_KW_GROUP)) {
    group_clause();
  } else {
    error("Expected select or group clause");
  }
}

static void query_body() {
  bool more_query;
  do {
    more_query = false;
    for (;;) {
      if (check(TOKEN_KW_FROM)) {
        from_clause();
        line();
      } else if (check(TOKEN_KW_LET)) {
        let_clause();
        line();
      } else if (check(TOKEN_KW_WHERE)) {
        where_clause();
        line();
      } else if (check(TOKEN_KW_JOIN)) {
        join_clause();
        line();
      } else if (check(TOKEN_KW_ORDERBY)) {
        orderby_clause();
        line();
      } else {
        break;
      }
    }
    group();
    select_or_group_clause();
    if (check(TOKEN_KW_INTO)) {
      line();
      token(TOKEN_KW_INTO, "in a query continuation");
      line();
      identifier("in the variable after 'into'");
      more_query = true;
    }
    end();
    if (more_query) {
      line();
    }
  } while (more_query);
}

static void query_expression() {
  breakparent();
  // TODO: Set the indent right here, somehow!
  from_clause();
  line();
  query_body();
  // TODO: Reset the indent right here!
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
  if (!match(TOKEN_DOT)) {
    token(TOKEN_MINUS_GREATERTHAN, "or '.' in member access expression");
  }
  identifier("in member access expression");
  optional_type_argument_list();
}

static void greater_than() {
  // This one is weird because it *might* be a shift operator, but might also
  // be a less than operator. Good thing our lexer doesn't discard whitespace,
  // right?
  group();

  enum Precedence prec = PREC_GREATERTHAN;
  if (parser.index < parser.buffer.count) {
    const enum TokenType next = parser.buffer.tokens[parser.index].type;
    if (next == TOKEN_GREATERTHAN) {
      prec = PREC_SHIFT;
    } else if (next == TOKEN_GREATERTHAN_EQUALS) {
      prec = PREC_ASSIGNMENT;
    }
  }

  space();
  token(TOKEN_GREATERTHAN, "in greater than expression");
  if (PREC_SHIFT == prec) {
    token(TOKEN_GREATERTHAN, "in left shift expression");
  } else if (PREC_ASSIGNMENT == prec) {
    token(TOKEN_GREATERTHAN_EQUALS, "in left shift assignment expression");
  }
  line();

  parse_precedence(prec + 1,
                   "to the right of a greater than or left shift expression");
  end();
}

static void is() {
  group();
  space();
  token(TOKEN_KW_IS, "in relational expression (is)");
  line();
  type("in the type in an 'is' expression");
  if (check_identifier()) {
    space();
    identifier("in the pattern matching part of an 'is' expression");
  }
  end();
}

static void as() {
  group();
  space();
  token(TOKEN_KW_AS, "in relational expression (as)");
  line();
  type("in the type in an 'as' expression");
  end();
}

static void conditional() {
  const struct ParseRule *rule = get_rule(TOKEN_QUESTION);

  space();
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

static void inter_statement_space() {
  const enum TokenType prev = parser.previous.type;
  if (prev != TOKEN_SEMICOLON && prev != TOKEN_COLON) {
    line();
  } else {
    // If there's a blank line between statements on purpose then maintain it.
    // We should maybe maintain this when scanning forward and not scan
    // backwards?
    int eol_count = 0;
    for (int i = parser.index - 2; i >= 0; i--) {
      enum TokenType typ = parser.buffer.tokens[i].type;
      if (typ == TOKEN_TRIVIA_WHITESPACE) {
        continue;
      } else if (typ == TOKEN_TRIVIA_EOL) {
        eol_count += 1;
        if (eol_count > 1) {
          break;
        }
      } else if (typ == TOKEN_TRIVIA_BLOCK_COMMENT ||
                 typ == TOKEN_TRIVIA_LINE_COMMENT ||
                 typ == TOKEN_TRIVIA_DIRECTIVE) {
        eol_count = 0;
      } else {
        break;
      }
    }
    if (eol_count > 1) {
      line();
    }
  }

  line();
}

static void block_impl(bool force_break, const char *where) {
  token(TOKEN_OPENBRACE, where);
  if (check(TOKEN_CLOSEBRACE)) {
    space();
    token(TOKEN_CLOSEBRACE, "at the end of a block");
    return;
  }

  // There *is* a body of some kind.
  bool broken = false;
  if (force_break) {
    breakparent();
    broken = true;
  }
  {
    line_indent();
    statement();
    while (!(check(TOKEN_CLOSEBRACE) || check(TOKEN_EOF))) {
      if (!broken) {
        breakparent();
        broken = true;
      }
      inter_statement_space();
      statement();
    }
    dedent();
  }
  line();
  token(TOKEN_CLOSEBRACE, "at the end of a block");
}

static void inline_block(const char *where) {
  block_impl(/*break*/ false, where);
}

static void block(const char *where) { block_impl(/*break*/ true, where); }

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
      if (check(TOKEN_OPENBRACE)) {
        array_initializer();
      } else if (match(TOKEN_KW_STACKALLOC)) {
        // Technically we can only do this in variable initializers, but...?
        line_indent();
        non_array_type("in the type of a stackalloc array initializer");
        if (match(TOKEN_OPENBRACKET)) {
          expression("between the brackets in a stackalloc array initializer");
          token(TOKEN_CLOSEBRACKET,
                "at the end of a stackalloc array initializer");
        }
        dedent();
      } else {
        // N.B.: This is technically a "fixed_poiner_initializer", not a real
        // initializer, but we fold it in because it's pretty harmless (this
        // code is way more lenient by design than the real C# parser) and the
        // indentation & formatting logic is complex and needs to be kept the
        // same.
        match(TOKEN_AMPERSAND);
        expression(" or array initializer in the initial value of a variable");
      }
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
        if (check(TOKEN_OPENBRACE)) {
          array_initializer();
        } else if (match(TOKEN_KW_STACKALLOC)) {
          // Technically we can only do this in variable initializers, but...?
          line_indent();
          non_array_type("in the type of a stackalloc array initializer");
          if (match(TOKEN_OPENBRACKET)) {
            expression(
                "between the brackets in a stackalloc array initializer");
            token(TOKEN_CLOSEBRACKET,
                  "at the end of a stackalloc array initializer");
          }
          dedent();
        } else {
          // ("fixed_pointer_initializer", See above.)
          match(TOKEN_AMPERSAND);
          expression("in the initial value of a variable");
          dedent();
        }
      }
    }
  }
}

static void local_variable_type() {
  if (!match(TOKEN_KW_VAR)) {
    type("in the type of a local variable");
  }
}

static bool check_local_deconstruction_declaration() {
  int index = parser.index;
  struct Token token = parser.current;

  int elementCount = 0;
  if (token.type == TOKEN_KW_VAR) {
    token = next_significant_token(&index);
    if (token.type != TOKEN_OPENPAREN) {
      DEBUG(("Check local decon: false (no open after var)"));
      return false;
    }

    token = next_significant_token(&index);
    for (;;) {
      if (!is_identifier_token(token.type)) {
        DEBUG(("Check local decon: false (tuple, not ident)"));
        return false;
      }

      elementCount += 1;
      token = next_significant_token(&index);
      if (token.type != TOKEN_COMMA) {
        break;
      }
      token = next_significant_token(&index);
    }
  } else {
    if (token.type != TOKEN_OPENPAREN) {
      DEBUG(("Check local decon: false (no open no var)"));
      return false;
    }

    token = next_significant_token(&index);
    for (;;) {
      // N.B.: Much of this complexity is to keep from confusing a
      // deconstruction with a cast or a function call or something. We need to
      // be very careful in what we allow here.
      if (token.type == TOKEN_KW_VAR) {
        token = next_significant_token(&index);
        if (!is_identifier_token(token.type)) {
          DEBUG(("Check local decon: false (var not followed by ident)"));
          return false;
        }
        token = next_significant_token(&index);
      } else {
        if (is_identifier_token(token.type)) {
          // This could either be the start of a type or just a standalone
          // identifier. There's currently no way of knowing! So let's just
          // try...
          int i2 = index;
          struct Token t2 = next_significant_token(&i2);
          if (t2.type == TOKEN_COMMA || t2.type == TOKEN_CLOSEPAREN) {
            // Yeah, it is, we can move on.
            index = i2;
            token = t2;
          } else {
            // It was an identifier but must be the start of a type...
            if (!check_is_type(&index, &token, /*array*/ true)) {
              DEBUG(("Check local decon: false (id not type)"));
              return false;
            }

            if (!is_identifier_token(token.type)) {
              DEBUG(("Check local decon: false (no id after type)"));
              return false;
            }
            token = next_significant_token(&index);
          }
        } else {
          // It was not an identifier so must be the start of a type...
          if (!check_is_type(&index, &token, /*array*/ true)) {
            DEBUG(("Check local decon: false (not type)"));
            return false;
          }

          if (!is_identifier_token(token.type)) {
            DEBUG(("Check local decon: false (no id after type')"));
            return false;
          }
          token = next_significant_token(&index);
        }
      }

      elementCount += 1;
      if (token.type != TOKEN_COMMA) {
        break;
      }
      token = next_significant_token(&index);
    }
  }

  if (elementCount < 2) {
    DEBUG(("Check local decon: false (0 or 1 element)"));
    return false;
  }

  if (token.type != TOKEN_CLOSEPAREN) {
    DEBUG(("Check local decon: false (tuple, no close paren)"));
    return false;
  }

  token = next_significant_token(&index);
  if (token.type != TOKEN_EQUALS) {
    DEBUG(("Check local decon: false (tuple, no =)"));
    return false;
  }

  DEBUG(("Check local decon: true"));
  return true;
}

static void local_deconstruction_declaration() {
  group();
  if (match(TOKEN_KW_VAR)) {
    line();
    {
      group();
      token(TOKEN_OPENPAREN, "after var in a tuple deconstruction");
      softline_indent();
      {
        group();
        for (;;) {
          identifier("in the variable part of a tuple deconstruction");
          if (!match(TOKEN_COMMA)) {
            break;
          }
          line();
        }
        end();
      }
      dedent();
      softline();
      token(TOKEN_CLOSEPAREN, "at the end of a tuple deconstruction");
      end();
    }
  } else {
    group();
    token(TOKEN_OPENPAREN, "at the start of a tuple deconstruction");
    softline_indent();
    {
      group();
      for (;;) {
        if (check_identifier() &&
            (check_next(TOKEN_COMMA) || check_next(TOKEN_CLOSEPAREN))) {
          identifier(
              "in the existing identifier slot of a tuple deconstruction");
        } else {
          group();
          local_variable_type();
          line();
          identifier("in the varible slot of a tuple deconstruction");
          end();
        }
        if (!match(TOKEN_COMMA)) {
          break;
        }
        line();
      }
      end();
    }
    dedent();
    softline();
    token(TOKEN_CLOSEPAREN, "at the end of a tuple deconstruction");
    end();
  }

  line();
  token(TOKEN_EQUALS, "after ) in a tuple deconstruction");
  line();
  expression("to the right of = in a tuple deconstruction");

  end();
}

static bool check_local_variable_declaration() {
  int index = parser.index;
  struct Token token = parser.current;
  if (token.type == TOKEN_KW_VAR) {
    token = next_significant_token(&index);
  } else if (token.type == TOKEN_KW_AWAIT) {
    // So technically this is wrong: in a non-async method you can have a local
    // variable declaration where the type is named "await" and that's OK.
    // Fixing this properly means that we need to actually attempt to parse a
    // non-declaration statement before we parse a declaration statement, and
    // that's slow and expensive and I don't want to do it right now. So uh:
    // don't name types "await".
    DEBUG(("Check local variable declaration: await"));
    return false;
  } else if (!check_is_type(&index, &token, /*array*/ true)) {
    // This leaves index and token on the next token after the end of the
    // type.
    DEBUG(("Check local variable declaration: not a type"));
    return false;
  }

  if (is_identifier_token(token.type)) {
    DEBUG(("Check local variable declaration: true (type and identifier)"));
    return true;
  } else {
    DEBUG(("Check local variable declaration: %s not identifier",
           token_text(token.type)));
    return false;
  }
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
  token(TOKEN_SEMICOLON, "at the end of a local const declaration");
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

static void switch_statement() {
  token(TOKEN_KW_SWITCH, "at the beginning of a switch statement");
  space();
  parenthesized_expression();
  line();
  token(TOKEN_OPENBRACE, "at the beginning of the case block");
  line();
  if (match(TOKEN_CLOSEBRACE)) {
    return;
  }

  bool first_case = true;
  while (check(TOKEN_KW_CASE) || check(TOKEN_KW_DEFAULT)) {
    if (!first_case) {
      if (parser.previous.type != TOKEN_COLON) {
        line();
      }
      line();
    }
    first_case = false;

    if (match(TOKEN_KW_CASE)) {
      space();
      {
        indent();
        if (check_type()) {
          group();
          type("in the type in a case pattern");
          if (check_identifier()) {
            line();
            identifier("after the type in a case pattern");
            if (check(TOKEN_KW_WHEN)) {
              line_indent();
              group();
              token(TOKEN_KW_WHEN, "after the identifier in a case pattern");
              space();
              expression("after 'when' in a pattern case pattern");
              end();
              dedent();
            }
          }
          end();
        } else {
          expression("in the label of a switch case");
        }
        dedent();
      }
    } else {
      token(TOKEN_KW_DEFAULT, "or label in the case block");
    }
    token(TOKEN_COLON,
          "between the label and statement list in the case block");
    if (!check_any(switch_section_end_tokens,
                   ARRAY_SIZE(switch_section_end_tokens))) {
      line_indent();
      bool first = true;
      while (!check_any(switch_section_end_tokens,
                        ARRAY_SIZE(switch_section_end_tokens))) {
        if (!first) {
          inter_statement_space();
        }
        first = false;
        statement();
      }
      dedent();
    }
  }
  line();
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
  token(TOKEN_SEMICOLON, "at the end of the initializer of a for loop");
}
static void for_condition() {
  expression("in the condition of a for loop");
  token(TOKEN_SEMICOLON, "between sections of a for loop");
}
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
      if (!match(TOKEN_SEMICOLON)) {
        for_initializer();
      }

      if (!match(TOKEN_SEMICOLON)) {
        line();
        for_condition();
      }

      if (!check(TOKEN_CLOSEPAREN)) {
        line();
        for_iterator();
      }
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
    expression("in the case label in a goto statement");
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
      if (check(TOKEN_OPENPAREN)) {
        space();
        token(TOKEN_OPENPAREN, "at the beginning of a catch block");
        type("in the type of the exception in a catch block");
        if (check_identifier()) {
          space();
          identifier("in the variable declaration in a catch block");
        }
        token(TOKEN_CLOSEPAREN,
              "at the end of the declaration in a catch block");
      }
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
  if (check(TOKEN_KW_FINALLY)) {
    line();
    token(TOKEN_KW_FINALLY, "at the beginning of a finally block");
    line();
    block("in the body of a finally block");
  }
}

static void checked_statement() {
  if (check_next(TOKEN_OPENPAREN)) {
    checked_expression();
    token(TOKEN_SEMICOLON,
          "at the end of a checked expression posing as a statement");
  } else {
    token(TOKEN_KW_CHECKED, "at the beginning of a checked statement");
    line();
    block("in the body of a checked statement");
  }
}

static void unchecked_statement() {
  if (check_next(TOKEN_OPENPAREN)) {
    unchecked_expression();
    token(TOKEN_SEMICOLON,
          "at the end of an unchecked expression posing as a statement");
  } else {
    token(TOKEN_KW_UNCHECKED, "at the beginning of an unchecked statement");
    line();
    block("in the body of an unchecked statement");
  }
}

static void lock_statement() {
  token(TOKEN_KW_LOCK, "at the beginning of a lock statement");
  space();
  parenthesized_expression();
  embedded_statement(/*embedded*/ true);
}

static void using_statement() {
  // Here we intentionally stack nested using statements at the same level of
  // indent because that's how lots of people like to use them.
  bool first = true;
  while (check(TOKEN_KW_USING)) {
    if (!first) {
      line();
    }
    first = false;
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
      // N.B.: According to the spec this must be a pointer type and cannot
      // use 'var', but meh.
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
      case TOKEN_KW_SWITCH:
        switch_statement();
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
    token(TOKEN_SEMICOLON, "at the end of a local variable declaration");
  } else if (check_local_deconstruction_declaration()) {
    local_deconstruction_declaration();
    token(TOKEN_SEMICOLON, "at the end of a local deconstruction declaration");
  } else if (check(TOKEN_KW_CONST)) {
    local_const_declaration();
  } else {
    embedded_statement(/*embedded*/ false);
  }
}

// ============================================================================
// Member Declarations
// ============================================================================

static void attribute_name() { type_name("in an attribute name"); }

static void attribute_argument() {
  group();
  bool indented = false;
  if (check_name_equals()) {
    name_equals("in attribute arguments");

    line_indent();
    indented = true;
  } else if (check_identifier() && check_next(TOKEN_COLON)) {
    identifier("in the name of a named attribute argument");
    token(TOKEN_COLON,
          "between the name and the value of a named attribute argument");

    line_indent();
    indented = true;
  }

  expression("in the argument of an attribute initializer");

  if (indented) {
    dedent();
  }
  end();
}

static void opt_attribute_arguments() {
  if (match(TOKEN_OPENPAREN)) {
    if (!check(TOKEN_CLOSEPAREN)) {
      softline_indent();
      attribute_argument();
      while (match(TOKEN_COMMA)) {
        line();
        attribute_argument();
      }
      dedent();
    }
    softline();
    token(TOKEN_CLOSEPAREN, "at the end of attribute arguments");
  }
}

static void attribute() {
  group();
  attribute_name();
  opt_attribute_arguments();
  end();
}

static void attribute_list() {
  attribute();
  while (match(TOKEN_COMMA) && !check(TOKEN_CLOSEBRACKET)) {
    line();
    attribute();
  }
}

static enum TokenType attribute_target_tokens[] = {
    TOKEN_KW_RETURN,
    TOKEN_KW_GLOBAL,
    TOKEN_KW_EVENT,
};

static bool check_attribute_target() {
  return check_identifier() || check_any(attribute_target_tokens,
                                         ARRAY_SIZE(attribute_target_tokens));
}

static void attribute_target() {
  if (!match_any(attribute_target_tokens,
                 ARRAY_SIZE(attribute_target_tokens))) {
    identifier("in an attribute target");
  }
}

static void attribute_section() {
  group();
  token(TOKEN_OPENBRACKET, "at the beginning of an attribute section");
  {
    softline_indent();
    if (check_attribute_target() && check_next(TOKEN_COLON)) {
      attribute_target();
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

// These are shared through lots of different declarations; we allow all of
// them everywhere, even though it doesn't make sense to have e.g. `async
// struct`.
const static enum TokenType modifier_tokens[] = {
    TOKEN_KW_ABSTRACT, TOKEN_KW_ASYNC,    TOKEN_KW_EXTERN,   TOKEN_KW_INTERNAL,
    TOKEN_KW_NEW,      TOKEN_KW_OVERRIDE, TOKEN_KW_PRIVATE,  TOKEN_KW_PROTECTED,
    TOKEN_KW_PUBLIC,   TOKEN_KW_READONLY, TOKEN_KW_SEALED,   TOKEN_KW_STATIC,
    TOKEN_KW_UNSAFE,   TOKEN_KW_VIRTUAL,  TOKEN_KW_VOLATILE,
};

static bool check_is_modifier(enum TokenType token) {
  return check_is_any(token, modifier_tokens, ARRAY_SIZE(modifier_tokens));
}

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

  {
    group();
    declaration_modifiers();
    token(TOKEN_KW_CONST, "at the beginning of a const declaration");
    space();
    type("in the type of a const");
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
  token(TOKEN_SEMICOLON, "at the end of a const member declaration");
}

static void field_declaration() {
  attributes();

  {
    group();
    declaration_modifiers();
    {
      group();
      type("in the type of a field");
      variable_declarators("in a field declaration");
      token(TOKEN_SEMICOLON, "at the end of a field declaration");
      end();
    }
    end();
  }
}

static void return_type() {
  if (!match(TOKEN_KW_VOID)) {
    type("in the return type of a method");
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

static void formal_parameter() {
  group();
  attributes();
  if (match_any(parameter_modifier_tokens,
                ARRAY_SIZE(parameter_modifier_tokens))) {
    space();
  }
  type("in the type of a formal parameter");
  space();
  identifier("in a parameter name");
  if (check(TOKEN_EQUALS)) {
    space();
    token(TOKEN_EQUALS, "between the name and default value of a parameter");
    {
      line_indent();
      expression("in the default value of a parameter");
      dedent();
    }
  }
  end();
}

static void formal_parameter_list_inner() {
  // N.B. __arglist is massively undocumented so shrug.
  if (!match(TOKEN_KW_ARGLIST)) {
    bool first = true;
    while (check_formal_parameter()) {
      if (!first) {
        token(TOKEN_COMMA, "between parameters");
        line();
      }
      first = false;

      formal_parameter();
    }
  }
}

static void formal_parameter_list(const char *where) {
  group();
  token(TOKEN_OPENPAREN, where);
  {
    softline_indent();
    formal_parameter_list_inner();
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
    if (!match(TOKEN_KW_CLASS)) {
      if (!match(TOKEN_KW_STRUCT)) {
        type("in the type of a type constraint");
      }
    }
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

static void member_name(const char *where) {
  // This might seem weird, but since members can be qualified by interface
  // type (e.g., a.b<t,y>.c.interface.foo) they're basically indistinguishable
  // from a type name without an analysis pass, or without deciding to split
  // off the last segment, and none of that matters for what we're doing.
  namespace_or_type_name(where);
}

static void method_declaration() {
  // method header
  attributes();
  group();
  {
    bool is_constructor = true;
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
        // Note: this can be a constructor, in which case the return_type
        // *was* the method name. If we were being serious here we would
        // double check the type was the same as the enclosing type, but nah.
        if (check_identifier()) {
          is_constructor = false;
          line();
          member_name("in the name of a method");
        }
        end();
      }

      if (!is_constructor) {
        optional_type_parameter_list();
      }
      end();
    }

    formal_parameter_list("in a method declaration");

    if (is_constructor) {
      if (check(TOKEN_COLON)) {
        line_indent();
        token(TOKEN_COLON, "in a constructor initializer");
        space();
        if (!match(TOKEN_KW_THIS)) {
          token(TOKEN_KW_BASE, "in a constructor initializer");
        }
        argument_list();
        dedent();
      }
    } else {
      if (check(TOKEN_KW_WHERE)) {
        line_indent();
        type_parameter_constraint_clauses();
        dedent();
      }
    }
    end();
  }

  if (!match(TOKEN_SEMICOLON)) {
    if (check(TOKEN_EQUALS_GREATERTHAN)) {
      space();
      token(TOKEN_EQUALS_GREATERTHAN, "in the expression body of a method");
      {
        line_indent();
        expression("in the expression body of a method");
        token(TOKEN_SEMICOLON, "at the end of the expression body of a method");
        dedent();
      }
    } else {
      line();
      block("at the beginning of the body of a method");
    }
  }
  end();
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

static void accessor_declarations() {
  // accessor_declarations
  bool first = true;
  while (check_accessor()) {
    if (!first) {
      line();
    }
    first = false;
    attributes();

    group();
    while (match_any(accessor_modifiers, ARRAY_SIZE(accessor_modifiers))) {
      space();
    }

    if (!match(TOKEN_KW_GET)) {
      token(TOKEN_KW_SET, "or get at the beginning of a property accessor");
    }
    if (!match(TOKEN_SEMICOLON)) {
      line();
      inline_block("or semicolon at the beginning of the body of a property "
                   "accessor");
    }
    end();
  }
}

static void property_declaration() {
  attributes();

  group();
  {
    group();
    declaration_modifiers();
    type("in the type of a property");
    space();
    member_name("in the name of a property");
    end();
  }
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

      accessor_declarations();

      dedent();
    }
    line();
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

  {
    group();
    declaration_modifiers();
    token(TOKEN_KW_EVENT, "at the beginning of an event declaration");
    space();
    type("in the type of an event");

    if (check_next(TOKEN_OPENBRACE)) {
      space();
      identifier("in the name of an event declaration");
      end();
      group();

      line();
      token(TOKEN_OPENBRACE, "at the beginning of an event declaration");
      {
        line_indent();
        bool first = true;
        while (check(TOKEN_KW_ADD) || check(TOKEN_KW_REMOVE) ||
               check(TOKEN_OPENBRACKET)) {
          if (!first) {
            line();
          }
          first = false;

          attributes();

          group();
          if (!match(TOKEN_KW_ADD)) {
            token(TOKEN_KW_REMOVE,
                  "or add before an event accessor declaration");
          }
          line();
          block("as the body of an event accessor");
          end();
        }
        dedent();
      }
      line();
      token(TOKEN_CLOSEBRACE, "at the end of an event declaration");
    } else {
      variable_declarators("in an event declaration");
      token(TOKEN_SEMICOLON, "at the end of an event declaration");
    }

    end();
  }
}

static void indexer_declaration() {
  attributes();

  group();
  {
    group();
    declaration_modifiers();
    type("in the type of an indexer");
    space();
    if (check_identifier()) {
      member_name("in the interface name of an indexer");
      token(TOKEN_DOT, "between the interface name of the indexer and 'this'");
    }
    token(TOKEN_KW_THIS, "in an indexer");
    end();
  }
  {
    group();
    token(TOKEN_OPENBRACKET, "after 'this' in an indexer");
    softline_indent();
    formal_parameter_list_inner();
    dedent();
    softline();
    token(TOKEN_CLOSEBRACKET, "after the parameter list of an indexer");
    end();
  }

  // property_body
  if (check(TOKEN_EQUALS_GREATERTHAN)) {
    // Simple "getter" property.
    space();
    token(TOKEN_EQUALS_GREATERTHAN,
          "at the beginning of the expression body of an indexer");
    {
      line_indent();
      expression("in the expression body of a property");
      token(TOKEN_SEMICOLON, "at the end of the expression body of an indexer");
      dedent();
    }
  } else {
    line();
    token(TOKEN_OPENBRACE,
          "at the beginning of the accessor declarations of an indexer");
    {
      line_indent();

      accessor_declarations();

      dedent();
    }
    line();
    token(TOKEN_CLOSEBRACE,
          "at the end of the accessor declarations of an indexer");
  }

  end();
}

static enum TokenType overloadable_operator_tokens[] = {
    TOKEN_AMPERSAND,
    TOKEN_ASTERISK,
    TOKEN_BAR,
    TOKEN_CARET,
    TOKEN_EQUALS_EQUALS,
    TOKEN_EXCLAMATION,
    TOKEN_EXCLAMATION_EQUALS,
    TOKEN_GREATERTHAN,
    TOKEN_GREATERTHAN_EQUALS,
    TOKEN_KW_FALSE,
    TOKEN_KW_TRUE,
    TOKEN_LESSTHAN,
    TOKEN_LESSTHAN_EQUALS,
    TOKEN_LESSTHAN_LESSTHAN,
    TOKEN_MINUS,
    TOKEN_MINUS_MINUS,
    TOKEN_PERCENT,
    TOKEN_PLUS,
    TOKEN_PLUS_PLUS,
    TOKEN_SLASH,
    TOKEN_TILDE,
};

static void operator_declaration() {
  attributes();

  group();
  {
    group();
    declaration_modifiers();
    if (match(TOKEN_KW_EXPLICIT) || match(TOKEN_KW_IMPLICIT)) {
      token(TOKEN_KW_OPERATOR, "in conversion operator");
      line();
      type("in the type of a conversion operator");
      token(TOKEN_OPENPAREN, "before the argument in a conversion operator");
      {
        softline_indent();
        formal_parameter();
        dedent();
      }
      softline();
      token(TOKEN_CLOSEPAREN, "after the argument in a conversion operator");
    } else {
      {
        group();
        type("in the return type of an operator");
        line();
        token(TOKEN_KW_OPERATOR, "after the type in an operator declaration");
        line();
        if ((parser.index < parser.buffer.count - 1) &&
            (parser.buffer.tokens[parser.index].type == TOKEN_GREATERTHAN) &&
            (parser.buffer.tokens[parser.index + 1].type ==
             TOKEN_GREATERTHAN)) {
          // NOTE: This is a bit weird here but we don't tokenize >> as a right
          // shift because it would make parsing generic types difficult. We
          // rely on the fact that we can see trivia in our parser to
          // disambiguate this.
          token(TOKEN_GREATERTHAN,
                "in an overloaded right shift declaration (1)");
          token(TOKEN_GREATERTHAN,
                "in an overloaded right shift declaration (2)");
        } else if (!match_any(overloadable_operator_tokens,
                              ARRAY_SIZE(overloadable_operator_tokens))) {
          error("Expected an overloadable operator in an operator declaration");
        }
        end();
      }
      formal_parameter_list("in an operator declaration");
    }
    end();
  }

  if (!match(TOKEN_SEMICOLON)) {
    if (check(TOKEN_EQUALS_GREATERTHAN)) {
      space();
      token(TOKEN_EQUALS_GREATERTHAN,
            "in the expression body of an operator declaration");
      {
        line_indent();
        expression("in the expression body of a method");
        token(TOKEN_SEMICOLON,
              "at the end of the expression body of an operator declaration");
        dedent();
      }
    } else {
      line();
      block("at the beginning of the body of an operator declaration");
    }
  }

  end();
}

static enum TokenType destructor_mod_tokens[] = {
    TOKEN_KW_EXTERN,
    TOKEN_KW_UNSAFE,
};

static void destructor_declaration() {
  // method header
  attributes();
  group();
  while (match_any(destructor_mod_tokens, ARRAY_SIZE(destructor_mod_tokens))) {
    line();
  }
  token(TOKEN_TILDE, "at the beginning of a destructor declaration");
  identifier("in the name of a destructor");
  token(TOKEN_OPENPAREN, "after the name of a destructor");
  token(TOKEN_CLOSEPAREN, "in the empty argument list of a destructor");
  if (!match(TOKEN_SEMICOLON)) {
    line();
    block("in the body of a destructor");
  }
  end();
}

static enum TokenType fixed_size_buffer_modifier_tokens[] = {
    TOKEN_KW_NEW,      TOKEN_KW_PUBLIC,  TOKEN_KW_PROTECTED,
    TOKEN_KW_INTERNAL, TOKEN_KW_PRIVATE, TOKEN_KW_UNSAFE,
};

static void fixed_size_buffer_declaration() {
  attributes();
  group();
  while (match_any(fixed_size_buffer_modifier_tokens,
                   ARRAY_SIZE(fixed_size_buffer_modifier_tokens))) {
    line();
  }
  {
    group();
    token(TOKEN_KW_FIXED, "at the beginning of a fixed buffer declaration");
    line();
    type("in the type of a fixed size buffer");

    {
      line_indent();
      group();
      identifier("in the field name of a fixed size buffer declaration");
      token(TOKEN_OPENBRACKET,
            "after the field name in a fixed size buffer declaration");
      {
        softline_indent();
        expression("in the size of a fixed size buffer declaration");
        dedent();
      }
      softline();
      token(TOKEN_CLOSEBRACKET,
            "after the buffer size in a fixed size buffer declaration");

      while (match(TOKEN_COMMA)) {
        end();
        line();
        group();

        identifier("in the field name of a fixed size buffer declaration");
        token(TOKEN_OPENBRACKET,
              "after the field name in a fixed size buffer declaration");
        {
          softline_indent();
          expression("in the size of a fixed size buffer declaration");
          dedent();
        }
        softline();
        token(TOKEN_CLOSEBRACKET,
              "after the buffer size in a fixed size buffer declaration");
      }
      end();
      dedent();
    }
    end();
  }
  token(TOKEN_SEMICOLON, "at the end of a fixed-size buffer declaration");
  end();
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
  MEMBERKIND_FIXED_SIZE_BUFFER,
};

static enum MemberKind check_member() {
  // OK this one sucks because we need to scan forward through tokens to
  // figure out what we're actually looking at. If we don't appear to be
  // looking at a member, then we return MEMBERKIND_NONE.
  DEBUG(("Check member..."));
  int index = parser.index;
  struct Token token = parser.current;

  // Attributes.
  while (token.type == TOKEN_OPENBRACKET) {
    DEBUG(("Scanning attributes"));
    int depth = 1;
    bool scanning_attributes = true;
    while (scanning_attributes) {
      token = next_significant_token(&index);
      switch (token.type) {
      case TOKEN_OPENBRACKET:
        depth++;
        break;

      case TOKEN_CLOSEBRACKET:
        depth--;
        if (depth == 0) {
          scanning_attributes = false;
        }
        break;

      case TOKEN_EOF:
        DEBUG(("EOF while scanning attributes"));
        return MEMBERKIND_NONE;

      default:
        break;
      }
    }
    token = next_significant_token(&index);
  }

  while (check_is_modifier(token.type)) {
    token = next_significant_token(&index);
  }

  if (token.type == TOKEN_KW_FIXED) {
    // Blah blah, yeah, technically this is special and has all kinds of
    // restrictions but have you seen the rest of this parser? :D
    DEBUG(("fixed -> MEMBERKIND_FIXED_SIZE_BUFFER"));
    return MEMBERKIND_FIXED_SIZE_BUFFER;
  }
  if (token.type == TOKEN_TILDE) {
    DEBUG(("~ -> MEMBERKIND_DESTRUCTOR"));
    return MEMBERKIND_DESTRUCTOR;
  }
  if (token.type == TOKEN_KW_EVENT) {
    DEBUG(("event -> MEMBERKIND_EVENT"));
    return MEMBERKIND_EVENT;
  }
  if (token.type == TOKEN_KW_CONST) {
    DEBUG(("const -> MEMBERKIND_CONST"));
    return MEMBERKIND_CONST;
  }
  if (is_type_keyword(token.type)) {
    DEBUG(("type keyword -> MEMBERKIND_TYPE"));
    return MEMBERKIND_TYPE;
  }
  if (token.type == TOKEN_KW_IMPLICIT || token.type == TOKEN_KW_EXPLICIT) {
    DEBUG(("implicit or explicit -> MEMBERKIND_OPERATOR"));
    return MEMBERKIND_OPERATOR;
  }

  if (!check_is_type(&index, &token, /*array*/ true)) {
    if (token.type == TOKEN_KW_VOID) {
      token = next_significant_token(&index);
    } else {
      DEBUG(("No type while determining member"));
      return MEMBERKIND_NONE;
    }
  }

  // From here on out it's easy; just scan.
  for (;;) {
    switch (token.type) {
    case TOKEN_EOF:
      DEBUG(("EOF before member type determination"));
      return MEMBERKIND_NONE;
    case TOKEN_KW_OPERATOR:
      DEBUG(("operator -> MEMBERKIND_OPERATOR"));
      return MEMBERKIND_OPERATOR;
    case TOKEN_OPENBRACKET:
      DEBUG(("[ -> MEMBERKIND_INDEXER"));
      return MEMBERKIND_INDEXER;
    case TOKEN_OPENPAREN:
      DEBUG(("( -> MEMBERKIND_METHOD"));
      return MEMBERKIND_METHOD;
    case TOKEN_EQUALS:
      // N.B.: Explicitly look for this because it helps us find a field with
      // an initial value of a lambda, which would otherwise look like a
      // property with an expression body.
      DEBUG(("= -> MEMBERKIND_FIELD"));
      return MEMBERKIND_FIELD;
    case TOKEN_SEMICOLON:
      DEBUG(("; -> MEMBERKIND_FIELD"));
      return MEMBERKIND_FIELD;
    case TOKEN_EQUALS_GREATERTHAN:
      DEBUG(("=> -> MEMBERKIND_PROPERTY"));
      return MEMBERKIND_PROPERTY;
    case TOKEN_OPENBRACE:
      DEBUG(("{ -> MEMBERKIND_PROPERTY"));
      return MEMBERKIND_PROPERTY;
    default:
      break;
    }
    token = next_significant_token(&index);
  }
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
      } else if (check(TOKEN_OPENBRACKET)) {
        // If you start with attributes you need a blank line.
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

    case MEMBERKIND_FIXED_SIZE_BUFFER:
      fixed_size_buffer_declaration();
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
  type_name("in a list of base types");
  {
    indent();
    while (match(TOKEN_COMMA)) {
      line();
      type_name("in a list of base types");
    }
    dedent();
  }
  end();
}

static void class_declaration() {
  {
    group();
    {
      group();
      token(TOKEN_KW_CLASS, "at the beginning of a class declaration");
      space();
      identifier("in the name a class declaration");
      optional_type_parameter_list();

      if (check(TOKEN_COLON)) {
        line_indent();
        base_types();
        dedent();
      }
      end();
    }

    if (check(TOKEN_KW_WHERE)) {
      line_indent();
      type_parameter_constraint_clauses();
      dedent();
    }
    end();
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
  {
    group();
    {
      group();
      token(TOKEN_KW_STRUCT, "at the beginning of a struct declaration");
      space();
      identifier("in the name of a struct declaration");
      optional_type_parameter_list();

      if (check(TOKEN_COLON)) {
        line_indent();
        base_types();
        dedent();
      }
      end();
    }

    if (check(TOKEN_KW_WHERE)) {
      line_indent();
      type_parameter_constraint_clauses();
      dedent();
    }
    end();
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
  group();
  {
    group();
    {
      group();
      token(TOKEN_KW_INTERFACE, "at the beginning of an interface declaration");
      space();
      identifier("in the name of an interface declaration");
      optional_type_parameter_list();
      end();
    }

    if (check(TOKEN_COLON)) {
      line_indent();
      base_types();
      dedent();
    }
    end();
  }

  if (check(TOKEN_KW_WHERE)) {
    line_indent();
    type_parameter_constraint_clauses();
    dedent();
  }
  end();

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
    type("in the base type of an enum");
    dedent();
  }

  line();
  token(TOKEN_OPENBRACE, "at the beginning of an enum declaration");
  {
    line_indent();

    group();
    bool first = true;
    while (check(TOKEN_OPENBRACKET) || check_identifier() ||
           check(TOKEN_COMMA)) {
      if (!first) {
        token(TOKEN_COMMA, "between enum members");
        if (check(TOKEN_CLOSEBRACE)) {
          // Handle lone trailing comma
          break;
        }

        end();
        line();
        group();
      }
      first = false;

      attributes();
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
    end();

    dedent();
  }
  line();
  token(TOKEN_CLOSEBRACE, "at the end of an enum declaration");
  match(TOKEN_SEMICOLON);
}

static void delegate_declaration() {
  group();
  {
    group();
    token(TOKEN_KW_DELEGATE, "at the beginning of a delegate declaration");
    space();
    return_type();
    line();
    identifier("in the name of a delegate declaration");
    optional_type_parameter_list();

    formal_parameter_list("in a delegate declaration");
    end();
  }

  if (check(TOKEN_KW_WHERE)) {
    line_indent();
    type_parameter_constraint_clauses();
    dedent();
  }
  token(TOKEN_SEMICOLON, "at the end of a delegate declaration");
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
    error("Expected some kind of type keyword");
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
  parser_init(builder, scan_tokens(source));

  advance();

  compilation_unit();
  line();

  token(TOKEN_EOF, "at the end of the file");

  return !parser.had_error;
}
