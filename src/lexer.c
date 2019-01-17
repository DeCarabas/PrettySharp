#include "lexer.h"
#include "common.h"

struct LexerInterpolationState {
  bool enabled;
  int depth;
};

struct Lexer {
  const char *start;
  const char *current;
  int line;
  struct LexerInterpolationState interpolation;
};

struct Lexer lexer;

void lexer_init(const char *source) {
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
}

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

struct Token scan_whitespace() {
  while (match(' ') || match('\t') || match('\r')) {
  }
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

static enum TokenType check_keyword(int start, int length, const char *rest,
                                    enum TokenType type) {
  if (lexer.current - lexer.start == start + length &&
      memcmp(lexer.start + start, rest, length) == 0) {
    return type;
  }
  return TOKEN_IDENTIFIER;
}

static enum TokenType keyword_type() {
  size_t len = lexer.current - lexer.start;
  switch (lexer.start[0]) {
  case 'a':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'b':
        return check_keyword(2, 6, "stract", TOKEN_KW_ABSTRACT);

      case 'd':
        return check_keyword(2, 1, "d", TOKEN_KW_ADD);

      case 'l':
        return check_keyword(2, 3, "ias", TOKEN_KW_ALIAS);

      case 's':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'c':
            return check_keyword(3, 6, "ending", TOKEN_KW_ASCENDING);
          case 'y':
            return check_keyword(3, 2, "nc", TOKEN_KW_ASYNC);
          }
        } else {
          return TOKEN_KW_AS;
        }
        break;

      case 'w':
        return check_keyword(2, 3, "ait", TOKEN_KW_AWAIT);
      }
    }
    break;

  case 'b':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'a':
        return check_keyword(2, 2, "se", TOKEN_KW_BASE);
      case 'o':
        return check_keyword(2, 2, "ol", TOKEN_KW_BOOL);
      case 'r':
        return check_keyword(2, 3, "eak", TOKEN_KW_BREAK);
      case 'y':
        if (len > 2) {
          return check_keyword(2, 2, "te", TOKEN_KW_BYTE);
        } else {
          return TOKEN_KW_BY;
        }
        break;
      }
    }
    break;

  case 'c':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'a':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 's':
            return check_keyword(3, 1, "e", TOKEN_KW_CASE);
          case 't':
            return check_keyword(3, 2, "ch", TOKEN_KW_CATCH);
          }
        }
        break;

      case 'h':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'a':
            return check_keyword(3, 1, "r", TOKEN_KW_CHAR);
          case 'e':
            return check_keyword(3, 4, "cked", TOKEN_KW_CHECKED);
          }
        }
        break;

      case 'l':
        return check_keyword(2, 3, "ass", TOKEN_KW_CLASS);

      case 'o':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'n':
            if (len > 3) {
              switch (lexer.start[3]) {
              case 's':
                return check_keyword(4, 1, "t", TOKEN_KW_CONST);
              case 't':
                return check_keyword(4, 4, "inue", TOKEN_KW_CONTINUE);
              }
            }
            break;
          }
        }
        break;
      }
    }
    break;

  case 'd':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'e':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'c':
            return check_keyword(3, 4, "imal", TOKEN_KW_DECIMAL);
          case 'f':
            return check_keyword(3, 4, "ault", TOKEN_KW_DEFAULT);
          case 'l':
            return check_keyword(3, 5, "egate", TOKEN_KW_DELEGATE);
          case 's':
            return check_keyword(3, 7, "cending", TOKEN_KW_DESCENDING);
          }
        }
      case 'o':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'u':
            return check_keyword(3, 4, "ble", TOKEN_KW_DOUBLE);
          }
        } else {
          return TOKEN_KW_DO;
        }
      case 'y':
        return check_keyword(2, 5, "namic", TOKEN_KW_DYNAMIC);
      }
    }
    break;

  case 'e':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'l':
        return check_keyword(2, 2, "se", TOKEN_KW_ELSE);
      case 'n':
        return check_keyword(2, 2, "um", TOKEN_KW_ENUM);
      case 'q':
        return check_keyword(2, 4, "uals", TOKEN_KW_EQUALS);
      case 'v':
        return check_keyword(2, 3, "ent", TOKEN_KW_EVENT);
      case 'x':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'p':
            return check_keyword(3, 5, "licit", TOKEN_KW_EXPLICIT);
          case 't':
            return check_keyword(3, 4, "ern", TOKEN_KW_EXTERN);
          }
        }
      }
    }
    break;

  case 'f':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'a':
        return check_keyword(2, 3, "lse", TOKEN_KW_FALSE);
      case 'i':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'n':
            return check_keyword(3, 4, "ally", TOKEN_KW_FINALLY);
          case 'x':
            return check_keyword(3, 2, "ed", TOKEN_KW_FIXED);
          }
        }
        break;
      case 'l':
        return check_keyword(2, 4, "oat", TOKEN_KW_FLOAT);
      case 'o':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'r':
            if (len > 3) {
              return check_keyword(3, 4, "each", TOKEN_KW_FOREACH);
            } else {
              return TOKEN_KW_FOR;
            }
            break;
          }
        }
        break;
      case 'r':
        return check_keyword(2, 2, "om", TOKEN_KW_FROM);
      }
    }
    break;

  case 'g':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'e':
        return check_keyword(2, 1, "t", TOKEN_KW_GET);
      case 'l':
        return check_keyword(2, 4, "obal", TOKEN_KW_GLOBAL);
      case 'o':
        return check_keyword(2, 2, "to", TOKEN_KW_GOTO);
      case 'r':
        return check_keyword(2, 3, "oup", TOKEN_KW_GROUP);
      }
    }
    break;

  case 'i':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'f':
        return TOKEN_KW_IF;
      case 'm':
        return check_keyword(2, 6, "plicit", TOKEN_KW_IMPLICIT);
      case 'n':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 't':
            if (len > 3) {
              switch (lexer.start[3]) {
              case 'e':
                if (len > 4) {
                  switch (lexer.start[4]) {
                  case 'r':
                    if (len > 5) {
                      switch (lexer.start[5]) {
                      case 'f':
                        return check_keyword(6, 3, "ace", TOKEN_KW_INTERFACE);
                      case 'n':
                        return check_keyword(6, 2, "al", TOKEN_KW_INTERNAL);
                      }
                    }
                    break;
                  }
                }
                break;
              case 'o':
                return TOKEN_KW_INTO;
              }
            } else {
              return TOKEN_KW_INT;
            }
            break;
          }
        } else {
          return TOKEN_KW_IN;
        }
      case 's':
        return TOKEN_KW_IS;
      }
    }
    break;

  case 'j':
    return check_keyword(1, 3, "oin", TOKEN_KW_JOIN);

  case 'l':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'e':
        return check_keyword(2, 1, "t", TOKEN_KW_LET);
      case 'o':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'c':
            return check_keyword(3, 1, "k", TOKEN_KW_LOCK);
          case 'n':
            return check_keyword(3, 1, "g", TOKEN_KW_LONG);
          }
        }
        break;
      }
    }
    break;

  case 'n':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'a':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'm':
            if (len > 3) {
              switch (lexer.start[3]) {
              case 'e':
                if (len > 4) {
                  switch (lexer.start[4]) {
                  case 'o':
                    return check_keyword(5, 1, "f", TOKEN_KW_NAMEOF);
                  case 's':
                    return check_keyword(5, 4, "pace", TOKEN_KW_NAMESPACE);
                  }
                }
                break;
              }
            }
            break;
          }
        }
        break;
      case 'e':
        return check_keyword(2, 1, "w", TOKEN_KW_NEW);
      case 'u':
        return check_keyword(2, 2, "ll", TOKEN_KW_NULL);
      }
    }
    break;

  case 'o':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'b':
        return check_keyword(2, 4, "ject", TOKEN_KW_OBJECT);
      case 'n':
        return TOKEN_KW_ON;
      case 'p':
        return check_keyword(2, 6, "erator", TOKEN_KW_OPERATOR);
      case 'r':
        return check_keyword(2, 5, "derby", TOKEN_KW_ORDERBY);
      case 'u':
        return check_keyword(2, 1, "t", TOKEN_KW_OUT);
      case 'v':
        return check_keyword(2, 6, "erride", TOKEN_KW_OVERRIDE);
      }
    }
    break;

  case 'p':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'a':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'r':
            if (len > 3) {
              switch (lexer.start[3]) {
              case 'a':
                return check_keyword(4, 2, "ms", TOKEN_KW_PARAMS);
              case 't':
                return check_keyword(4, 3, "ial", TOKEN_KW_PARTIAL);
              }
            }
            break;
          }
        }
        break;
      case 'r':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'i':
            return check_keyword(3, 4, "vate", TOKEN_KW_PRIVATE);
          case 'o':
            return check_keyword(3, 6, "tected", TOKEN_KW_PROTECTED);
          }
        }
        break;
      case 'u':
        return check_keyword(2, 4, "blic", TOKEN_KW_PUBLIC);
      }
    }
    break;

  case 'r':
    if (len > 1) {
      if (lexer.start[1] == 'e') {
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'a':
            return check_keyword(3, 5, "donly", TOKEN_KW_READONLY);
          case 'f':
            return TOKEN_KW_REF;
          case 'm':
            return check_keyword(3, 3, "ove", TOKEN_KW_REMOVE);
          case 't':
            return check_keyword(3, 3, "urn", TOKEN_KW_RETURN);
          }
        }
      }
    }
    break;

  case 's':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'b':
        return check_keyword(2, 3, "yte", TOKEN_KW_SBYTE);
      case 'e':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'a':
            return check_keyword(3, 3, "led", TOKEN_KW_SEALED);
          case 'l':
            return check_keyword(3, 3, "ect", TOKEN_KW_SELECT);
          case 't':
            return TOKEN_KW_SET;
          }
        }
        break;
      case 'h':
        return check_keyword(2, 3, "ort", TOKEN_KW_SHORT);
      case 'i':
        return check_keyword(2, 4, "zeof", TOKEN_KW_SIZEOF);
      case 't':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'a':
            if (len > 3) {
              switch (lexer.start[3]) {
              case 'c':
                return check_keyword(4, 6, "kalloc", TOKEN_KW_STACKALLOC);
              case 't':
                return check_keyword(4, 2, "ic", TOKEN_KW_STATIC);
              }
            }
            break;
          case 'r':
            if (len > 3) {
              switch (lexer.start[3]) {
              case 'i':
                return check_keyword(4, 2, "ng", TOKEN_KW_STRING);
              case 'u':
                return check_keyword(4, 2, "ct", TOKEN_KW_STRUCT);
              }
            }
            break;
          }
        }
        break;
      case 'w':
        return check_keyword(2, 4, "itch", TOKEN_KW_SWITCH);
      }
    }
    break;

  case 't':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'h':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'i':
            return check_keyword(3, 1, "s", TOKEN_KW_THIS);
          case 'r':
            return check_keyword(3, 2, "ow", TOKEN_KW_THROW);
          }
        }
        break;
      case 'r':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'u':
            return check_keyword(3, 1, "e", TOKEN_KW_TRUE);
          case 'y':
            return TOKEN_KW_TRY;
          }
        }
        break;
      case 'y':
        return check_keyword(2, 4, "peof", TOKEN_KW_TYPEOF);
      }
    }
    break;

  case 'u':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'i':
        return check_keyword(2, 2, "nt", TOKEN_KW_UINT);
      case 'l':
        return check_keyword(2, 3, "ong", TOKEN_KW_ULONG);
      case 'n':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'c':
            return check_keyword(3, 6, "hecked", TOKEN_KW_UNCHECKED);
          case 's':
            return check_keyword(3, 3, "afe", TOKEN_KW_UNSAFE);
          }
        }
        break;
      case 's':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'h':
            return check_keyword(3, 3, "ort", TOKEN_KW_USHORT);
          case 'i':
            return check_keyword(3, 2, "ng", TOKEN_KW_USING);
          }
        }
        break;
      }
    }
    break;

  case 'v':
    if (len > 1) {
      switch (lexer.start[1]) {
      case 'a':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'l':
            return check_keyword(3, 2, "ue", TOKEN_KW_VALUE);
          case 'r':
            return TOKEN_KW_VAR;
          }
        }
        break;
      case 'i':
        return check_keyword(2, 5, "rtual", TOKEN_KW_VIRTUAL);
      case 'o':
        if (len > 2) {
          switch (lexer.start[2]) {
          case 'i':
            return check_keyword(3, 1, "d", TOKEN_KW_VOID);
          case 'l':
            return check_keyword(3, 5, "atile", TOKEN_KW_VOLATILE);
          }
        }
        break;
      }
    }
    break;

  case 'w':
    if (len > 1 && lexer.start[1] == 'h') {
      if (len > 2 && lexer.start[2] == 'e') {
        if (len > 3) {
          switch (lexer.start[3]) {
          case 'n':
            return TOKEN_KW_WHEN;
          case 'r':
            return check_keyword(4, 1, "e", TOKEN_KW_WHERE);
          }
        }
      } else {
        return check_keyword(2, 3, "ile", TOKEN_KW_WHILE);
      }
    }
    break;

  case 'y':
    return check_keyword(1, 4, "ield", TOKEN_KW_YIELD);
  }
  return TOKEN_IDENTIFIER;
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
      break;
    }

    advance();
  }

  return make_token(keyword_type());
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
    }
    advance();
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

struct Token scan_line_comment() {
  for (;;) {
    switch (peek()) {
    case '\r':
    case '\n':
      return make_token(TOKEN_TRIVIA_LINE_COMMENT);
    default:
      advance();
    }
  }
}

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

struct Token scan_token() {
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
    return make_token(match('=') ? TOKEN_SLASH_EQUAL : TOKEN_SLASH);

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
    if (match('?')) {
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
    } else if (match('&')) {
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

  case '\\':
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
  }

  return error_token("Unexpected character");
}
