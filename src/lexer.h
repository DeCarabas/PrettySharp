#ifndef LEXER_H
#define LEXER_H

enum TokenType {
  TOKEN_AMPERSAND,
  TOKEN_AMPERSAND_AMPERSAND,
  TOKEN_AMPERSAND_EQUALS,
  TOKEN_ASTERISK,
  TOKEN_ASTERISK_EQUALS,
  TOKEN_BAR,
  TOKEN_BAR_BAR,
  TOKEN_BAR_EQUALS,
  TOKEN_CARET,
  TOKEN_CARET_EQUALS,
  TOKEN_CHARACTER_LITERAL,
  TOKEN_CLOSEBRACE,
  TOKEN_CLOSEBRACKET,
  TOKEN_CLOSEPAREN,
  TOKEN_COLON,
  TOKEN_COLON_COLON,
  TOKEN_COMMA,
  TOKEN_DOT,
  TOKEN_DOT_DOT,
  TOKEN_EQUALS,
  TOKEN_EQUALS_EQUALS,
  TOKEN_EQUALS_GREATERTHAN,
  TOKEN_EXCLAMATION,
  TOKEN_EXCLAMATION_EQUALS,
  TOKEN_GREATERTHAN,
  TOKEN_GREATERTHAN_EQUALS,
  TOKEN_INTERPOLATED_STRING,
  TOKEN_INTERPOLATION_END,
  TOKEN_LESSTHAN,
  TOKEN_LESSTHAN_EQUALS,
  TOKEN_LESSTHAN_LESSTHAN,
  TOKEN_LESSTHAN_LESSTHAN_EQUALS,
  TOKEN_MINUS,
  TOKEN_MINUS_EQUALS,
  TOKEN_MINUS_GREATERTHAN,
  TOKEN_MINUS_MINUS,
  TOKEN_NUMERIC_LITERAL,
  TOKEN_OPENBRACE,
  TOKEN_OPENBRACKET,
  TOKEN_OPENPAREN,
  TOKEN_PERCENT,
  TOKEN_PERCENT_EQUALS,
  TOKEN_PLUS,
  TOKEN_PLUS_EQUALS,
  TOKEN_PLUS_PLUS,
  TOKEN_QUESTION,
  TOKEN_QUESTION_QUESTION,
  TOKEN_QUESTION_QUESTION_EQUALS,
  TOKEN_SEMICOLON,
  TOKEN_SLASH,
  TOKEN_SLASH_EQUAL,
  TOKEN_STRING_LITERAL,
  TOKEN_TILDE,

  TOKEN_TRIVIA_BLOCK_COMMENT,
  TOKEN_TRIVIA_EOL,
  TOKEN_TRIVIA_LINE_COMMENT,
  TOKEN_TRIVIA_WHITESPACE,

  TOKEN_IDENTIFIER,

  TOKEN_ERROR,
  TOKEN_EOF,
};

struct Token {
  enum TokenType type;
  const char *start;
  int length;
  int line;
};

void lexer_init(const char *source);
struct Token scan_token();

#endif
