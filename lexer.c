#include "lexer.h"
#include "common.h"
#include "unicode.h"

struct LexerInterpolationState {
  bool enabled;
  int depth;
};

#define MAX_PREPROCESSOR_SYMBOLS (1000)

struct PPSymbolTable {
  struct Token symbols[MAX_PREPROCESSOR_SYMBOLS];
};

static void pp_init_table(struct PPSymbolTable *table) {
  for (int i = 0; i < MAX_PREPROCESSOR_SYMBOLS; i++) {
    table->symbols[i].type = TOKEN_NONE;
    table->symbols[i].length = 0;
  }
}

struct Lexer {
  const char *start;
  const char *current;
  int line;
  struct LexerInterpolationState interpolation;
  struct PPSymbolTable symbol_table;
};

struct Lexer lexer;

static void lexer_init(const char *source) {
  if (source[0] == -17 && source[1] == -69 && source[2] == -65) {
    // UTF-8 BOM.
    source += 3;
  } else if (source[0] == -1 || source[0] == -2) {
    // Some kind of UTF-16
    fprintf(stderr, "Unicode source files not supported");
    exit(100);
  }

  lexer.start = source;
  lexer.current = source;
  lexer.line = 1;
  lexer.interpolation.enabled = false;
  lexer.interpolation.depth = 0;

  pp_init_table(&lexer.symbol_table);
}

// #define LEX_PRINT_DEBUG_ENABLED

#ifdef LEX_PRINT_DEBUG_ENABLED

int last_debug_line = -1;
static void vLEX_DEBUG(const char *format, va_list args) {
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

static void LEX_DEBUG_(const char *format, ...) {
  va_list args;
  va_start(args, format);
  vLEX_DEBUG(format, args);
  va_end(args);
}

#define LEX_DEBUG(x) (DEBUG_ x)

#else

#define LEX_DEBUG(x) ((void)0)

#endif

static struct Token scan_token();

char advance() {
  lexer.current++;
  return lexer.current[-1];
}

bool is_at_end() { return *lexer.current == '\0'; }

char peek() { return *lexer.current; }

char peek_next() {
  if (is_at_end()) {
    return 0;
  }
  return lexer.current[1];
}

bool match(char c) {
  if (is_at_end()) {
    return false;
  }
  if (*lexer.current != c) {
    return false;
  }

  lexer.current++;
  return true;
}

struct Token make_token(enum TokenType type) {
  struct Token token;
  token.type = type;
  token.start = lexer.start;
  token.length = (int)(lexer.current - lexer.start);
  token.line = lexer.line;
  return token;
}

struct Token error_token(const char *message) {
  struct Token token;
  token.type = TOKEN_ERROR;
  token.start = message;
  token.length = strlen(message);
  token.line = lexer.line;
  return token;
}

static void eat_whitespace() {
  while (match(' ') || match('\t') || match('\r')) {
  }
}

struct Token scan_whitespace() {
  eat_whitespace();
  return make_token(TOKEN_TRIVIA_WHITESPACE);
}

struct Token scan_end_of_line() {
  struct Token token = make_token(TOKEN_TRIVIA_EOL);
  lexer.line++;
  return token;
}

struct Token scan_string_literal(char open) {
  for (;;) {
    if (match(open)) {
      break;
    } else if (match('\\')) {
      // N.B.: If I were actually interpreting string literals I would work
      // harder, translate characters, report errors, &c. But I'm not. Most
      // important is to skip over the potential closing character.
      match(open);
    } else if (is_at_end() || match('\n') || match('\r')) {
      // TODO: NEWLINES: C# technically has 5 newline characters.
      return error_token("Unterminated string constant.");
    } else {
      advance();
    }
  }

  // N.B.: If I were actually interpreting these things I would make sure that a
  // character literal is only one character long. But I'm not.
  return make_token(open == '"' ? TOKEN_STRING_LITERAL
                                : TOKEN_CHARACTER_LITERAL);
}

bool is_digit(char c) { return (c >= '0' && c <= '9'); }
bool is_binary_digit(char c) { return c == '1' || c == '0'; }
bool is_hex_digit(char c) {
  return is_digit(c) || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
}

bool scan_numeric_literal(char first) {
  const char *saved_current = lexer.current;

  bool decimal = false;
  bool hex = false;
  bool binary = false;
  if (first == '0') {
    if (peek() == 'x' || peek() == 'X') {
      advance();
      hex = true;
    } else if (peek() == 'b' || peek() == 'B') {
      advance();
      binary = true;
    } else {
      decimal = true;
    }
  } else if (first == '.') {
    if (is_digit(peek())) {
      decimal = true;
    }
  } else if (is_digit(first)) {
    decimal = true;
  }

  if (hex || binary) {
    if (hex) {
      while (is_hex_digit(peek()) || peek() == '_') {
        advance();
      }
    } else {
      while (is_binary_digit(peek()) || peek() == '_') {
        advance();
      }
    }

    if (match('l') || match('L')) {
      if (peek() == 'u' || peek() == 'U') {
        advance();
      }
    } else if (match('u') || match('U')) {
      if (peek() == 'l' || peek() == 'L') {
        advance();
      }
    }

    return true;
  } else if (decimal) {
    while (is_digit(peek()) || peek() == '_') {
      advance();
    }

    // If first is '.' then we just read the fraction, above.
    if (first != '.' && peek() == '.') {
      advance();
      while (is_digit(peek()) || peek() == '_') {
        advance();
      }
    }

    if (peek() == 'E' || peek() == 'e') {
      advance();
      if (peek() == '-' || peek() == '+') {
        advance();
      }
      while (is_digit(peek()) || peek() == '_') {
        advance();
      }
    }

    if (peek() == 'f' || peek() == 'F') {
      advance();
    } else if (peek() == 'd' || peek() == 'D') {
      advance();
    } else if (peek() == 'm' || peek() == 'M') {
      advance();
    } else if (peek() == 'l' || peek() == 'L') {
      advance();
      if (peek() == 'u' || peek() == 'U') {
        advance();
      }
    } else if (peek() == 'u' || peek() == 'U') {
      advance();
      if (peek() == 'l' || peek() == 'L') {
        advance();
      }
    }

    return true;
  }

  lexer.current = saved_current;
  return false;
}

static bool is_id_char(char c) {
  return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || is_digit(c) ||
         c == '_';
}

struct Token scan_identifier_or_keyword() {
  for (;;) {
    if (is_at_end()) {
      break;
    }

    if (peek() == '\\') {
      return error_token("NOTIMPL: Unicode escapes");
    }

    if (!is_id_char(peek())) {
      // Try decoding unicode. (Ugh.)
      uint32_t rune;
      const char *tmp = lexer.current;
      if (decode_utf8(&tmp, &rune) && is_identifier_part_rune(rune)) {
        lexer.current = tmp;
        continue;
      } else {
        break;
      }
    }

    advance();
  }

  return make_token(keyword_type(lexer.start, lexer.current - lexer.start));
}

struct Token scan_verbatim_string_literal() {
  for (;;) {
    if (match('"')) {
      if (!match('"')) {
        break;
      }
      // "" is allowed, and doesn't break the string.
    } else if (is_at_end()) {
      return error_token("Unterminated verbatim string constant.");
    } else if (match('\n')) {
      lexer.line++;
    } else {
      advance();
    }
  }

  return make_token(TOKEN_STRING_LITERAL);
}

struct Token scan_interpolated_string_literal(bool verbatim) {
  // OK we're doing what Roslyn does here, which is treat the entire
  // interpolated string as a single token, rather than tracking the state
  // through multiple calls to the lexer. This has advantages and
  // disadvantages, but an advantage is that I don't have to keep a stack
  // inside the lexer, so it's ever so slightly simpler.
  const char *old_start = lexer.start;
  struct LexerInterpolationState old_interpolation = lexer.interpolation;
  lexer.interpolation.enabled = true;
  lexer.interpolation.depth = 0;

  for (;;) {
    if (is_at_end()) {
      return error_token("Unterminated interpolated string literal");
    }

    if (match('"')) {
      if (verbatim && match('"')) {
        // This is how you quote a string in a verbatim string literal.
      } else {
        break;
      }
    } else if (!verbatim && match('\\')) {
      if (!is_at_end()) {
        advance();
      }
    } else if (match('{')) {
      if (!match('{')) {
        // This is the interpolation hole, spin it around...
        for (;;) {
          lexer.interpolation.depth = 1;
          struct Token nested = scan_token();
          if (nested.type == TOKEN_ERROR) {
            return nested;
          }
          if (nested.type == TOKEN_EOF ||
              nested.type == TOKEN_INTERPOLATION_END) {
            break;
          }
        }
      }
    } else {
      advance();
    }
  }

  lexer.start = old_start;
  lexer.interpolation = old_interpolation;
  return make_token(TOKEN_INTERPOLATED_STRING);
}

static void eat_to_end_of_line() {
  for (;;) {
    if (is_at_end()) {
      return;
    }
    switch (peek()) {
    case '\r':
    case '\n':
      return;
    default:
      advance();
    }
  }
}

struct Token scan_line_comment() {
  eat_to_end_of_line();
  return make_token(TOKEN_TRIVIA_LINE_COMMENT);
}

#define CHECK_STR(str) (memcmp(lexer.current, str, ARRAY_SIZE(str) - 1) == 0)

const char *pp_expression_error = NULL;

static bool define_symbol(const char *id_start, const char *id_end) {
  LEX_DEBUG(("DEFINE '%.*s'", (int)(id_end - id_start), id_start));
  int len = id_end - id_start;
  struct Token *target = NULL;
  for (int i = 0; i < MAX_PREPROCESSOR_SYMBOLS; i++) {
    struct Token *tok = &lexer.symbol_table.symbols[i];
    if (tok->type == TOKEN_IDENTIFIER && tok->length == len &&
        memcmp(tok->start, id_start, len) == 0) {
      return true;
    }

    if (target == NULL && tok->type == TOKEN_NONE) {
      target = tok;
    }
  }

  if (target == NULL) {
    return false;
  }

  target->type = TOKEN_IDENTIFIER;
  target->start = id_start;
  target->length = len;
  target->line = lexer.line;
  return true;
}

static void undefine_symbol(const char *id_start, const char *id_end) {
  LEX_DEBUG(("UNDEF '%.*s'", (int)(id_end - id_start), id_start));
  int len = id_end - id_start;
  for (int i = 0; i < MAX_PREPROCESSOR_SYMBOLS; i++) {
    struct Token *tok = &lexer.symbol_table.symbols[i];
    if (tok->type == TOKEN_IDENTIFIER && tok->length == len &&
        memcmp(tok->start, id_start, len) == 0) {
      tok->type = TOKEN_NONE;
      tok->length = 0;
      break;
    }
  }
}

static bool is_symbol_defined(const char *id_start, const char *id_end) {
  int len = id_end - id_start;
  for (int i = 0; i < MAX_PREPROCESSOR_SYMBOLS; i++) {
    struct Token *tok = &lexer.symbol_table.symbols[i];
    if (tok->type == TOKEN_IDENTIFIER && tok->length == len &&
        memcmp(tok->start, id_start, len) == 0) {
      LEX_DEBUG(("LOOKUP '%.*s' -> TRUE", (int)(id_end - id_start), id_start));
      return true;
    }
  }
  LEX_DEBUG(("LOOKUP '%.*s' -> FALSE", (int)(id_end - id_start), id_start));
  return false;
}

static bool scan_and_eval_pp_expression();

static bool scan_and_eval_pp_primary_expression() {
  if (match('(')) {
    eat_whitespace();
    bool result = scan_and_eval_pp_expression();
    eat_whitespace();
    if (!match(')')) {
      pp_expression_error =
          "Expected a ) after a parenthesized preprocessor expression";
    }
    return result;
  }

  const char *id_start = lexer.current;
  struct Token identifier = scan_identifier_or_keyword();
  if (identifier.type == TOKEN_ERROR) {
    pp_expression_error = identifier.start;
    return false;
  } else if (identifier.type == TOKEN_KW_TRUE) {
    return true;
  } else if (identifier.type == TOKEN_KW_FALSE) {
    return false;
  } else {
    const char *id_end = lexer.current;
    return is_symbol_defined(id_start, id_end);
  }
}

static bool scan_and_eval_pp_unary_expression() {
  bool negate = false;
  while (match('!')) {
    eat_whitespace();
    negate = !negate;
  }

  bool result = scan_and_eval_pp_primary_expression();
  return negate ? !result : result;
}

static bool scan_and_eval_pp_equality_expression() {
  bool result = scan_and_eval_pp_unary_expression();
  eat_whitespace();
  for (;;) {
    if (CHECK_STR("==")) {
      lexer.current += 2;
      eat_whitespace();
      bool sub_result = scan_and_eval_pp_unary_expression();
      result = (sub_result == result);
      eat_whitespace();
    } else if (CHECK_STR("!=")) {
      lexer.current += 2;
      eat_whitespace();
      bool sub_result = scan_and_eval_pp_unary_expression();
      result = (sub_result != result);
      eat_whitespace();
    } else {
      break;
    }
  }
  return result;
}

static bool scan_and_eval_pp_and_expression() {
  bool result = scan_and_eval_pp_equality_expression();
  eat_whitespace();
  while (CHECK_STR("||")) {
    lexer.current += 2;
    eat_whitespace();
    bool sub_result = scan_and_eval_pp_equality_expression();
    result = result && sub_result;
    eat_whitespace();
  }
  return result;
}

static bool scan_and_eval_pp_or_expression() {
  bool result = scan_and_eval_pp_and_expression();
  eat_whitespace();
  while (CHECK_STR("||")) {
    lexer.current += 2;
    eat_whitespace();
    bool sub_result = scan_and_eval_pp_and_expression();
    result = result || sub_result;
    eat_whitespace();
  }
  return result;
}

static bool scan_and_eval_pp_expression() {
  eat_whitespace();
  bool result = scan_and_eval_pp_or_expression();
  eat_whitespace();
  return result;
}

struct Token scan_disabled_text() {
  // We're in a disabled part of the text here because we failed a preprocessor
  // conditional. We need to scan and deal with nesting and stuff until we find
  // the next #elif or #else or #something. We'll count everything here as
  // one big TRIVIA_DIRECTIVE and it will be ignored by our consumer.
  int nest = 0;
  for (;;) {
    if (is_at_end()) {
      break;
    }
    eat_to_end_of_line();
    advance();
    lexer.line += 1;

    eat_whitespace();
    if (CHECK_STR("#if")) {
      nest++;
    } else if (CHECK_STR("#elif") || CHECK_STR("#else")) {
      if (nest == 0) {
        break;
      }
    } else if (CHECK_STR("#endif")) {
      if (nest == 0) {
        break;
      }
      nest -= 1;
    }
  }

  return make_token(TOKEN_TRIVIA_DIRECTIVE);
}

struct Token scan_directive() {
  // This is its own dorky little language with its own state table.
  // We actually want to handle it here because skipped source text is allowed
  // to be *lexically incorrect*. (http://dotyl.ink/l/n5u7nwelyi) So we have no
  // choice but to try to understand these things.
  eat_whitespace();
  if (CHECK_STR("define")) {
    lexer.current += (ARRAY_SIZE("define") - 1);
    eat_whitespace();

    const char *id_start = lexer.current;
    struct Token identifier = scan_identifier_or_keyword();
    if (identifier.type == TOKEN_ERROR) {
      return identifier;
    } else if (identifier.type == TOKEN_KW_TRUE) {
      return error_token("Literal 'true' not allowed in #define");
    } else if (identifier.type == TOKEN_KW_FALSE) {
      return error_token("Literal 'false' not allowed in #define");
    }
    const char *id_end = lexer.current;

    if (!define_symbol(id_start, id_end)) {
      return error_token("Too many preprocessor symbols defined");
    }
  } else if (CHECK_STR("undef")) {
    lexer.current += (ARRAY_SIZE("undef") - 1);
    eat_whitespace();

    const char *id_start = lexer.current;
    struct Token identifier = scan_identifier_or_keyword();
    if (identifier.type == TOKEN_ERROR) {
      return identifier;
    } else if (identifier.type == TOKEN_KW_TRUE) {
      return error_token("Literal 'true' not allowed in #define");
    } else if (identifier.type == TOKEN_KW_FALSE) {
      return error_token("Literal 'false' not allowed in #define");
    }
    const char *id_end = lexer.current;

    undefine_symbol(id_start, id_end);
  } else if (CHECK_STR("if")) {
    lexer.current += (ARRAY_SIZE("if") - 1);
    eat_whitespace();

    pp_expression_error = NULL;
    bool eval_result = scan_and_eval_pp_expression();
    if (pp_expression_error) {
      return error_token(pp_expression_error);
    } else if (!eval_result) {
      LEX_DEBUG(("IF -> FALSE"));
      return scan_disabled_text();
    } else {
      LEX_DEBUG(("***** IF -> TRUE"));
    }
  } else if (CHECK_STR("elif")) {
    lexer.current += (ARRAY_SIZE("elif") - 1);
    eat_whitespace();

    pp_expression_error = NULL;
    bool eval_result = scan_and_eval_pp_expression();
    if (pp_expression_error) {
      return error_token(pp_expression_error);
    } else if (!eval_result) {
      LEX_DEBUG(("ELIF -> FALSE"));
      return scan_disabled_text();
    } else {
      LEX_DEBUG(("ELIF -> TRUE"));
    }
  } else if (CHECK_STR("else")) {
    LEX_DEBUG(("ELSE"));
  } else if (CHECK_STR("endif")) {
    LEX_DEBUG(("ENDIF"));
  }

  for (;;) {
    switch (peek()) {
    case '\r':
    case '\n':
      return make_token(TOKEN_TRIVIA_DIRECTIVE);
    default:
      advance();
    }
  }
}

#undef CHECK_STR

struct Token scan_block_comment() {
  int end_line = lexer.line;
  for (;;) {
    if (is_at_end()) {
      return error_token("Unterminated block comment");
    }
    switch (peek()) {
    case '\r':
      advance();
      match('\n');
      end_line++;
      continue;

    case '\n':
      advance();
      end_line++;
      continue;

    case '*':
      advance();
      if (match('/')) {
        struct Token result = make_token(TOKEN_TRIVIA_BLOCK_COMMENT);
        lexer.line = end_line;
        return result;
      }

    default:
      advance();
    }
  }
}

static struct Token scan_token() {
  lexer.start = lexer.current;
  if (is_at_end()) {
    return make_token(TOKEN_EOF);
  }

  // TODO: Directives

  char c = advance();
  switch (c) {
  case '\t':
  case ' ':
    return scan_whitespace();

  case '\r':
    // TODO: NEWLINES: C# technically has 5 newline characters.
    // \r on its own is a new line, but \r\n is a newline together (not
    // two new lines!)
    match('\n');
    return scan_end_of_line();

  case '\n':
    return scan_end_of_line();

  case '"':
  case '\'':
    return scan_string_literal(c);

  case '/':
    if (match('/')) {
      return scan_line_comment();
    } else if (match('*')) {
      return scan_block_comment();
    }
    return make_token(match('=') ? TOKEN_SLASH_EQUALS : TOKEN_SLASH);

  case '#':
    return scan_directive();

  case '.':
    if (scan_numeric_literal(c)) {
      return make_token(TOKEN_NUMERIC_LITERAL);
    } else if (match('.')) {
      if (match('.')) {
        return error_token("Triple dot not allowed.");
      } else {
        return make_token(TOKEN_DOT_DOT);
      }
    } else {
      return make_token(TOKEN_DOT);
    }

  case ',':
    return make_token(TOKEN_COMMA);

  case ':':
    return make_token(match(':') ? TOKEN_COLON_COLON : TOKEN_COLON);

  case ';':
    return make_token(TOKEN_SEMICOLON);

  case '~':
    return make_token(TOKEN_TILDE);

  case '!':
    return make_token(match('=') ? TOKEN_EXCLAMATION_EQUALS
                                 : TOKEN_EXCLAMATION);

  case '=':
    if (match('=')) {
      return make_token(TOKEN_EQUALS_EQUALS);
    } else if (match('>')) {
      return make_token(TOKEN_EQUALS_GREATERTHAN);
    } else {
      return make_token(TOKEN_EQUALS);
    }

  case '*':
    return make_token(match('=') ? TOKEN_ASTERISK_EQUALS : TOKEN_ASTERISK);

  case '(':
    return make_token(TOKEN_OPENPAREN);

  case ')':
    return make_token(TOKEN_CLOSEPAREN);

  case '{':
    if (lexer.interpolation.enabled) {
      lexer.interpolation.depth++;
    }
    return make_token(TOKEN_OPENBRACE);

  case '}':
    if (lexer.interpolation.enabled) {
      lexer.interpolation.depth--;
      if (lexer.interpolation.depth == 0) {
        return make_token(TOKEN_INTERPOLATION_END);
      }
    }
    return make_token(TOKEN_CLOSEBRACE);

  case '[':
    return make_token(TOKEN_OPENBRACKET);

  case ']':
    return make_token(TOKEN_CLOSEBRACKET);

  case '?':
    if (match('.')) {
      return make_token(TOKEN_QUESTION_DOT);
    } else if (match('[')) {
      return make_token(TOKEN_QUESTION_OPENBRACKET);
    } else if (match('?')) {
      return make_token(match('=') ? TOKEN_QUESTION_QUESTION_EQUALS
                                   : TOKEN_QUESTION_QUESTION);
    } else {
      return make_token(TOKEN_QUESTION);
    }

  case '+':
    if (match('=')) {
      return make_token(TOKEN_PLUS_EQUALS);
    } else if (match('+')) {
      return make_token(TOKEN_PLUS_PLUS);
    } else {
      return make_token(TOKEN_PLUS);
    }

  case '-':
    if (match('=')) {
      return make_token(TOKEN_MINUS_EQUALS);
    } else if (match('-')) {
      return make_token(TOKEN_MINUS_MINUS);
    } else if (match('>')) {
      return make_token(TOKEN_MINUS_GREATERTHAN);
    } else {
      return make_token(TOKEN_MINUS);
    }

  case '%':
    return make_token(match('=') ? TOKEN_PERCENT_EQUALS : TOKEN_PERCENT);

  case '&':
    if (match('=')) {
      return make_token(TOKEN_AMPERSAND_EQUALS);
    } else if (match('&')) {
      return make_token(TOKEN_AMPERSAND_AMPERSAND);
    } else {
      return make_token(TOKEN_AMPERSAND);
    }

  case '^':
    if (match('=')) {
      return make_token(TOKEN_CARET_EQUALS);
    } else {
      return make_token(TOKEN_CARET);
    }

  case '|':
    if (match('=')) {
      return make_token(TOKEN_BAR_EQUALS);
    } else if (match('|')) {
      return make_token(TOKEN_BAR_BAR);
    } else {
      return make_token(TOKEN_BAR);
    }

  case '<':
    if (match('=')) {
      return make_token(TOKEN_LESSTHAN_EQUALS);
    } else if (match('<')) {
      if (match('=')) {
        return make_token(TOKEN_LESSTHAN_LESSTHAN_EQUALS);
      } else {
        return make_token(TOKEN_LESSTHAN_LESSTHAN);
      }
    } else {
      return make_token(TOKEN_LESSTHAN);
    }

  case '>':
    if (match('=')) {
      return make_token(TOKEN_GREATERTHAN_EQUALS);
    } else {
      return make_token(TOKEN_GREATERTHAN);
    }

  case '@':
    if (match('"')) {
      return scan_verbatim_string_literal();
    } else if (match('$')) {
      if (match('"')) {
        return scan_interpolated_string_literal(/*verbatim*/ true);
      } else {
        break; // Error case.
      }
    } else {
      return scan_identifier_or_keyword();
    }

  case '$':
    if (match('"')) {
      return scan_interpolated_string_literal(/*verbatim*/ false);
    } else if (match('@')) {
      if (match('"')) {
        return scan_interpolated_string_literal(/*verbatim*/ true);
      } else {
        break; // Error case.
      }
    } else {
      break; // Error case.
    }

  case '\\': // Oh stupid unicode escape.
    return scan_identifier_or_keyword();

  case '_':
    return scan_identifier_or_keyword();

  default:
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
      return scan_identifier_or_keyword();
    }

    if (is_digit(c)) {
      if (scan_numeric_literal(c)) {
        return make_token(TOKEN_NUMERIC_LITERAL);
      }
    }

    uint32_t rune;
    // We've already consumed the first byte, so we need to back up one.
    const char *tmp = lexer.current - 1;
    if (decode_utf8(&tmp, &rune) && is_identifier_start_rune(rune)) {
      lexer.current = tmp;
      return scan_identifier_or_keyword();
    }
  }

  return error_token("Unexpected character");
}

struct TokenBuffer scan_tokens(const char *source) {
  int capacity = 5 * 1024;

  struct TokenBuffer buffer;
  buffer.count = 0;
  buffer.tokens = malloc(capacity * sizeof(struct Token));

  lexer_init(source);
  for (;;) {
    if (buffer.count == capacity) {
      capacity *= 2;
      buffer.tokens = realloc(buffer.tokens, sizeof(struct Token) * capacity);
    }
    struct Token token = scan_token();
    buffer.tokens[buffer.count] = token;
    buffer.count++;

    if (token.type == TOKEN_EOF) {
      break;
    }
  }

  return buffer;
}

void free_token_buffer(struct TokenBuffer *buffer) {
  free(buffer->tokens);
  buffer->tokens = NULL;
}

void dump_lex(const char *source) {
  struct TokenBuffer buffer = scan_tokens(source);

  int line = -1;
  for (int i = 0; i < buffer.count; i++) {
    struct Token token = buffer.tokens[i];
    if (token.line != line) {
      printf("%4d ", token.line);
      line = token.line;
    } else {
      printf("   | ");
    }
    printf("%3d %-25s '%.*s'\n", token.type, dbg_token_type(token.type),
           token.length, token.start);

    if (token.type == TOKEN_EOF || token.type == TOKEN_ERROR)
      break;
  }
}
