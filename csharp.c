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
      doc_text(parser.builder, trivia.start, trivia.length);
      doc_line(parser.builder);
    } else if (trivia.type == TOKEN_TRIVIA_LINE_COMMENT) {
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

static struct Token next_significant_token() {
  struct Token token = parser.current;
  int index = parser.index;
  for (;;) {
    if (index == parser.buffer.count) {
      break;
    }
    token = parser.buffer.tokens[index];
    index += 1;

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
  return token;
}

static bool check_next(enum TokenType type) {
  struct Token token = next_significant_token();
  return token.type == type;
}

static bool check_next_identifier() {
  struct Token token = next_significant_token();
  return is_identifier_token(token.type);
}

static void single_token() {
  text(parser.current);
  advance();
}

static void token(enum TokenType type) {
  if (parser.current.type == type) {
    single_token();
  } else {
    error("Expected '%s'", token_text(type));
  }
}

static bool check(enum TokenType type) {
  bool result = parser.current.type == type;
  debug("check: %d (%s) vs %d (%s) => %s", parser.current.type,
        token_text(parser.current.type), type, token_text(type),
        result ? "true" : "false");
  return result;
}

static bool check_any(const enum TokenType *types, int count) {
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

static void identifier() {
  if (check_identifier()) {
    single_token();
  } else {
    error("Expected an identifier");
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

const static enum TokenType builtin_type_tokens[] = {
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

// ============================================================================
// Expressions
// ============================================================================
static void expression();

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

// These things all look at "previous".
static void parse_precedence(enum Precedence precedence) {
  group();
  ParseFn prefix_rule = get_rule(parser.current.type)->prefix;
  if (prefix_rule == NULL) {
    error("Expect expression.");
    return;
  }

  prefix_rule();

  while (precedence <= get_rule(parser.current.type)->precedence) {
    ParseFn infix_rule = get_rule(parser.current.type)->infix;
    infix_rule();
  }
  end();
}

static void primary() { single_token(); }

// This comes up all the time, both here in expression land and down in
// statement land.
static void parenthesized_expression() {
  group();
  token(TOKEN_OPENPAREN);
  {
    softline_indent();
    expression();
    dedent();
  }
  softline();
  token(TOKEN_CLOSEPAREN);
  end();
}

static void grouping() {
  // TODO: CASTING??? What if.... what..... hm.
  parenthesized_expression();
}

static void unary_prefix() {
  single_token();
  expression();
}

// TODO: UNARY!

static void query_expression() {
  notimplemented("Not Implemented: query_expression ('from')");
  advance();
}

static void binary() {
  enum TokenType op = parser.current.type;

  space();
  token(op);
  line();

  const struct ParseRule *rule = get_rule(op);
  parse_precedence((enum Precedence)(rule->precedence + 1));
}

static void greater_than() {
  // This one is weird because it *might* be a shift operator, but might also be
  // a less than operator. Good thing our lexer doesn't discard whitespace,
  // right?
  notimplemented("Not Implemented: greater than");
  advance();
}

static void is_as() {
  notimplemented("Not Implemented: is or as");
  advance();
}

static void conditional() {
  notimplemented("Not Implemented: conditional");
  advance();
}

const static struct ParseRule rules[] = {
#define TKN(id, txt, is_id, prefix, infix, prec) {prefix, infix, prec},
#include "token.inc"
#undef TKN
};

static const struct ParseRule *get_rule(enum TokenType type) {
  return &rules[type];
}

static void expression() { parse_precedence(PREC_ASSIGNMENT); }

// ============================================================================
// Statements
// ============================================================================

static void statement();
static void embedded_statement(bool embedded);

static void block() {
  token(TOKEN_OPENBRACE);
  {
    softline_indent();
    bool first = true;
    while (!check(TOKEN_CLOSEBRACE) && !check(TOKEN_EOF)) {
      if (first) {
        breakparent();
      } else {
        line();
      }
      first = false;

      statement();
    }
    dedent();
  }
  softline();
  token(TOKEN_CLOSEBRACE);
}

static void variable_declarators() {
  group();
  line_indent();
  identifier();
  if (check(TOKEN_EQUALS)) {
    space();
    token(TOKEN_EQUALS);
    {
      line_indent();
      // N.B.: This is technically a "fixed_poiner_initializer", not a real
      // initializer, but we fold it in because it's pretty harmless (this code
      // is way more lenient by design than the real C# parser) and the
      // indentation & formatting logic is complex and needs to be kept the
      // same.
      match(TOKEN_AMPERSAND);
      expression();
      dedent();
    }
  }

  while (check(TOKEN_COMMA)) {
    token(TOKEN_COMMA);
    end();
    group();
    line();

    identifier();
    if (check(TOKEN_EQUALS)) {
      space();
      token(TOKEN_EQUALS);
      {
        line_indent();
        // ("fixed_pointer_initializer", See above.)
        match(TOKEN_AMPERSAND);
        expression();
        dedent();
      }
    }
  }
  token(TOKEN_SEMICOLON);
  end();
  dedent();
}

static bool check_local_variable_type() {
  return check_type() || check(TOKEN_KW_VAR);
}

static void local_variable_type() {
  if (!match(TOKEN_KW_VAR)) {
    type();
  }
}

static bool check_local_variable_declaration() {
  return check_local_variable_type() && check_next_identifier();
}

static void local_variable_declaration() {
  group();
  local_variable_type();
  variable_declarators();
  end();
}

static void local_const_declaration() {
  token(TOKEN_KW_CONST);
  space();
  local_variable_declaration();
}

static void if_statement() {
  token(TOKEN_KW_IF);
  space();
  parenthesized_expression();
  embedded_statement(/*embedded*/ true);
}

const static enum TokenType switch_section_end_tokens[] = {
    TOKEN_KW_CASE,
    TOKEN_KW_DEFAULT,
    TOKEN_CLOSEBRACE,
};

static void case_statement() {
  token(TOKEN_KW_CASE);
  space();
  parenthesized_expression();
  line();
  token(TOKEN_OPENBRACE);
  line();
  while (check(TOKEN_KW_CASE) || check(TOKEN_KW_DEFAULT)) {
    if (match(TOKEN_KW_CASE)) {
      space();
      {
        indent();
        expression();
        dedent();
      }
    } else {
      token(TOKEN_KW_DEFAULT);
    }
    token(TOKEN_COLON);
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
  token(TOKEN_CLOSEBRACE);
}

static void while_statement() {
  token(TOKEN_KW_WHILE);
  space();
  parenthesized_expression();
  embedded_statement(/*embedded*/ true);
}

static void do_statement() {
  token(TOKEN_KW_DO);
  embedded_statement(/*embedded*/ true);
  line();
  token(TOKEN_KW_WHILE);
  space();
  parenthesized_expression();
  token(TOKEN_SEMICOLON);
}

static void statement_expression_list() {
  expression();
  while (match(TOKEN_COMMA)) {
    line();
    expression();
  }
}

static void for_initializer() {
  if (check_local_variable_declaration()) {
    local_variable_declaration();
  } else {
    statement_expression_list();
  }
}
static void for_condition() { expression(); }
static void for_iterator() { statement_expression_list(); }

static void for_statement() {
  token(TOKEN_KW_FOR);
  space();
  {
    group();
    token(TOKEN_OPENPAREN);
    {
      softline_indent();
      for_initializer();

      token(TOKEN_SEMICOLON);
      line();

      for_condition();

      token(TOKEN_SEMICOLON);
      line();

      for_iterator();
      dedent();
    }
    softline();
    token(TOKEN_CLOSEPAREN);
    end();
  }
  embedded_statement(/*embedded*/ true);
}

static void foreach_statement() {
  token(TOKEN_KW_FOREACH);
  space();
  {
    group();
    token(TOKEN_OPENPAREN);
    {
      softline_indent();
      local_variable_type();
      space();
      identifier();
      space();
      token(TOKEN_KW_IN);
      line();
      expression();
      dedent();
    }
    softline();
    token(TOKEN_CLOSEPAREN);
    end();
  }
  embedded_statement(/*embedded*/ true);
}

static void goto_statement() {
  token(TOKEN_KW_GOTO);
  space();
  if (match(TOKEN_KW_CASE)) {
    space();
    identifier();
  } else if (!match(TOKEN_KW_DEFAULT)) {
    identifier();
  }
  token(TOKEN_SEMICOLON);
}

static void return_statement() {
  token(TOKEN_KW_RETURN);
  if (!check(TOKEN_SEMICOLON)) {
    space();
    expression();
  }
  token(TOKEN_SEMICOLON);
}

static void throw_statement() {
  token(TOKEN_KW_THROW);
  if (!check(TOKEN_SEMICOLON)) {
    space();
    expression();
  }
  token(TOKEN_SEMICOLON);
}

static void try_statement() {
  token(TOKEN_KW_TRY);
  line();
  block();
  while (check(TOKEN_KW_CATCH)) {
    line();
    {
      group();
      token(TOKEN_KW_CATCH);
      space();
      token(TOKEN_OPENPAREN);
      type();
      if (check_identifier()) {
        space();
        identifier();
      }
      token(TOKEN_CLOSEPAREN);

      if (check(TOKEN_KW_WHEN)) {
        line_indent();
        token(TOKEN_KW_WHEN);
        space();
        parenthesized_expression();
        dedent();
      }
      end();
    }
    line();
    block();
  }
  if (check(TOKEN_KW_FINALLY)) {
    token(TOKEN_KW_FINALLY);
    line();
    block();
  }
}

static void checked_statement() {
  token(TOKEN_KW_CHECKED);
  line();
  block();
}

static void unchecked_statement() {
  token(TOKEN_KW_UNCHECKED);
  line();
  block();
}

static void lock_statement() {
  token(TOKEN_KW_LOCK);
  space();
  parenthesized_expression();
  embedded_statement(/*embedded*/ true);
}

static void using_statement() {
  token(TOKEN_KW_USING);
  space();
  {
    group();
    token(TOKEN_OPENPAREN);
    {
      softline_indent();
      if (check_local_variable_declaration()) {
        local_variable_declaration();
      } else {
        expression();
      }
      dedent();
    }
    softline();
    token(TOKEN_CLOSEPAREN);
    end();
  }
  embedded_statement(/*embedded*/ true);
}

static void yield_statement() {
  group();
  token(TOKEN_KW_YIELD);
  space();
  if (!match(TOKEN_KW_BREAK)) {
    token(TOKEN_KW_RETURN);
    {
      line_indent();
      expression();
      dedent();
    }
  }
  token(TOKEN_SEMICOLON);
  end();
}

static void unsafe_statement() {
  token(TOKEN_KW_UNSAFE);
  line();
  block();
}

static void fixed_statement() {
  token(TOKEN_KW_FIXED);
  space();
  {
    group();
    token(TOKEN_OPENPAREN);
    {
      softline_indent();
      // N.B.: According to the spec this must be a pointer type and cannot use
      // 'var', but meh.
      local_variable_declaration();
      dedent();
    }
    softline();
    token(TOKEN_CLOSEPAREN);
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
    block();
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
        token(TOKEN_SEMICOLON);
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
        expression();
        token(TOKEN_SEMICOLON);
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
    identifier();
    token(TOKEN_COLON);
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
  token(TOKEN_OPENPAREN);
  {
    softline_indent();
    while (!check(TOKEN_CLOSEPAREN) && !check(TOKEN_EOF)) {
      group();
      bool indented = false;
      if (check_name_equals()) {
        name_equals();

        line_indent();
        indented = true;
      }

      expression();

      if (indented) {
        dedent();
      }
      end();
    }
    dedent();
  }
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
  {
    softline_indent();
    if (check(TOKEN_IDENTIFIER) && check_next(TOKEN_COLON)) {
      identifier();
      token(TOKEN_COLON);
      line();
    }

    attribute_list();
    dedent();
  }
  softline();
  token(TOKEN_CLOSEBRACKET);
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
    TOKEN_KW_ASYNC,
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

    token(TOKEN_KW_CONST);
    space();
    type();
    {
      line_indent();
      bool first = true;
      while (check_identifier() || check(TOKEN_COMMA)) {
        if (!first) {
          token(TOKEN_COMMA);
          line();
        }

        group();
        identifier();
        space();
        token(TOKEN_EQUALS);
        {
          line_indent();
          expression();
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
    variable_declarators();
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

static void formal_parameter_list() {
  group();
  token(TOKEN_OPENPAREN);
  {
    softline_indent();
    bool first = true;
    while (check_formal_parameter()) {
      if (!first) {
        token(TOKEN_COMMA);
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
        identifier();
        if (check(TOKEN_EQUALS)) {
          space();
          token(TOKEN_EQUALS);
          {
            indent();
            line();
            expression();
            dedent();
          }
        }
        end();
      }
    }

    dedent();
  }
  token(TOKEN_CLOSEPAREN);
  end();
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
    {
      softline_indent();
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
    }
    softline();
    token(TOKEN_GREATERTHAN);
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
  declaration_modifiers();

  group();

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
  formal_parameter_list();

  if (check(TOKEN_KW_WHERE)) {
    line_indent();
    type_parameter_constraint_clauses();
    dedent();
  }

  if (!match(TOKEN_SEMICOLON)) {
    if (check(TOKEN_EQUALS_GREATERTHAN)) {
      line_indent();
      token(TOKEN_EQUALS_GREATERTHAN);
      {
        line_indent();
        expression();
        token(TOKEN_SEMICOLON);
        dedent();
      }
      dedent();
    } else {
      line();
      block();
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
    token(TOKEN_EQUALS_GREATERTHAN);
    {
      line_indent();
      expression();
      token(TOKEN_SEMICOLON);
      dedent();
    }
  } else {
    line();
    token(TOKEN_OPENBRACE);
    {
      line_indent();

      // accessor_declarations
      while (check_accessor()) {
        attributes();

        group();
        while (match_any(accessor_modifiers, ARRAY_SIZE(accessor_modifiers))) {
          space();
        }

        match(TOKEN_KW_GET);
        match(TOKEN_KW_SET);
        if (!match(TOKEN_SEMICOLON)) {
          block();
        }
        end();

        line();
      }
      dedent();
    }
    token(TOKEN_CLOSEBRACE);

    // property_initializer?
    if (check(TOKEN_EQUALS)) {
      group();

      space();
      token(TOKEN_EQUALS);
      {
        line_indent();
        expression();
        token(TOKEN_SEMICOLON);
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
    token(TOKEN_KW_EVENT);
    space();
    type();

    if (check_next(TOKEN_OPENBRACE)) {
      space();
      identifier();

      line();
      token(TOKEN_OPENBRACE);
      {
        line_indent();
        while (check(TOKEN_KW_ADD) || check(TOKEN_KW_REMOVE) ||
               check(TOKEN_OPENBRACKET)) {
          attributes();

          group();
          match(TOKEN_KW_ADD);
          match(TOKEN_KW_REMOVE);
          line();
          block();
          end();

          line();
        }
        // Stuff

        dedent();
      }
      token(TOKEN_CLOSEBRACE);
    } else {
      variable_declarators();
      token(TOKEN_SEMICOLON);
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

static void class_declaration() {
  token(TOKEN_KW_CLASS);
  space();
  identifier();
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
  token(TOKEN_OPENBRACE);
  {
    line_indent();
    member_declarations();
    dedent();
  }
  line();
  token(TOKEN_CLOSEBRACE);
  match(TOKEN_SEMICOLON);
}

static void struct_declaration() {
  token(TOKEN_KW_STRUCT);
  space();
  identifier();
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
  token(TOKEN_OPENBRACE);
  {
    line_indent();
    member_declarations();
    dedent();
  }
  line();
  token(TOKEN_CLOSEBRACE);
  match(TOKEN_SEMICOLON);
}

static void interface_declaration() {
  token(TOKEN_KW_INTERFACE);
  space();
  identifier();
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
  token(TOKEN_OPENBRACE);
  {
    line_indent();
    member_declarations();
    dedent();
  }
  line();
  token(TOKEN_CLOSEBRACE);
  match(TOKEN_SEMICOLON);
}

static void enum_declaration() {
  token(TOKEN_KW_ENUM);
  space();
  identifier();

  if (check(TOKEN_COLON)) {
    line_indent();
    type();
    dedent();
  }

  line();
  token(TOKEN_OPENBRACE);
  {
    line_indent();

    bool first = true;
    while (check(TOKEN_OPENBRACKET) || check_identifier() ||
           check(TOKEN_COMMA)) {
      if (!first) {
        token(TOKEN_COMMA);
        line();
      }
      first = false;

      identifier();
      if (check(TOKEN_EQUALS)) {
        space();
        token(TOKEN_EQUALS);
        {
          line_indent();
          expression();
          dedent();
        }
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
    line_indent();
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
  line();
  token(TOKEN_OPENBRACE);

  {
    line_indent();
    namespace_body();
    dedent();
  }

  line();
  token(TOKEN_CLOSEBRACE);
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

  token(TOKEN_EOF);

  return !parser.had_error;
}
