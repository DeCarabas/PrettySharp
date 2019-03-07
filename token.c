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
#include "token.h"
#include "common.h"

const char *token_text(enum TokenType type) {
  switch (type) {

#define TKN(id, txt, is_id, prefix, infix, prec)                               \
  case TOKEN_##id:                                                             \
    return txt;

#include "token.inc"

#undef TKN

  default:
    return "<unknown token type>";
  }
}

bool is_identifier_token(enum TokenType type, bool in_query) {
  switch (type) {

#define TKN(id, txt, is_id, prefix, infix, prec)                               \
  case TOKEN_##id:                                                             \
    return is_id;

#include "token.inc"

#undef TKN

  default:
    return false;
  }
}

const char *dbg_token_type(enum TokenType type) {
  switch (type) {

#define TKN(id, txt, is_id, prefix, infix, prec)                               \
  case TOKEN_##id:                                                             \
    return "TOKEN_" #id;

#include "token.inc"

#undef TKN

  default:
    return "<?>";
  }
}

static enum TokenType check_kw(const char *token_start, size_t token_length,
                               size_t start, size_t length, const char *rest,
                               enum TokenType type) {
  if (token_length == start + length &&
      memcmp(token_start + start, rest, length) == 0) {
    return type;
  }
  return TOKEN_IDENTIFIER;
}

// This function was generated based on token.inc by the 'maketrie.py' program.
enum TokenType keyword_type(const char *start, size_t len) {
  if (len > 0) {
    switch (start[0]) {
    case '_':
      return check_kw(start, len, 1, 8, "_arglist", TOKEN_KW_ARGLIST);
    case 'a':
      if (len > 1) {
        switch (start[1]) {
        case 'b':
          return check_kw(start, len, 2, 6, "stract", TOKEN_KW_ABSTRACT);
        case 'd':
          return check_kw(start, len, 2, 1, "d", TOKEN_KW_ADD);
        case 'l':
          return check_kw(start, len, 2, 3, "ias", TOKEN_KW_ALIAS);
        case 's':
          if (len > 2) {
            switch (start[2]) {
            case 'c':
              return check_kw(start, len, 3, 6, "ending", TOKEN_KW_ASCENDING);
            case 'y':
              return check_kw(start, len, 3, 2, "nc", TOKEN_KW_ASYNC);
            }
          } else {
            if (len == 2) {
              return TOKEN_KW_AS;
            }
          }
          break;
        case 'w':
          return check_kw(start, len, 2, 3, "ait", TOKEN_KW_AWAIT);
        }
      }
      break;
    case 'b':
      if (len > 1) {
        switch (start[1]) {
        case 'a':
          return check_kw(start, len, 2, 2, "se", TOKEN_KW_BASE);
        case 'o':
          return check_kw(start, len, 2, 2, "ol", TOKEN_KW_BOOL);
        case 'r':
          return check_kw(start, len, 2, 3, "eak", TOKEN_KW_BREAK);
        case 'y':
          if (len > 2) {
            switch (start[2]) {
            case 't':
              return check_kw(start, len, 3, 1, "e", TOKEN_KW_BYTE);
            }
          } else {
            if (len == 2) {
              return TOKEN_KW_BY;
            }
          }
          break;
        }
      }
      break;
    case 'c':
      if (len > 1) {
        switch (start[1]) {
        case 'a':
          if (len > 2) {
            switch (start[2]) {
            case 's':
              return check_kw(start, len, 3, 1, "e", TOKEN_KW_CASE);
            case 't':
              return check_kw(start, len, 3, 2, "ch", TOKEN_KW_CATCH);
            }
          }
          break;
        case 'h':
          if (len > 2) {
            switch (start[2]) {
            case 'a':
              return check_kw(start, len, 3, 1, "r", TOKEN_KW_CHAR);
            case 'e':
              return check_kw(start, len, 3, 4, "cked", TOKEN_KW_CHECKED);
            }
          }
          break;
        case 'l':
          return check_kw(start, len, 2, 3, "ass", TOKEN_KW_CLASS);
        case 'o':
          if (len > 2) {
            switch (start[2]) {
            case 'n':
              if (len > 3) {
                switch (start[3]) {
                case 's':
                  return check_kw(start, len, 4, 1, "t", TOKEN_KW_CONST);
                case 't':
                  return check_kw(start, len, 4, 4, "inue", TOKEN_KW_CONTINUE);
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
        switch (start[1]) {
        case 'e':
          if (len > 2) {
            switch (start[2]) {
            case 'c':
              return check_kw(start, len, 3, 4, "imal", TOKEN_KW_DECIMAL);
            case 'f':
              return check_kw(start, len, 3, 4, "ault", TOKEN_KW_DEFAULT);
            case 'l':
              return check_kw(start, len, 3, 5, "egate", TOKEN_KW_DELEGATE);
            case 's':
              return check_kw(start, len, 3, 7, "cending", TOKEN_KW_DESCENDING);
            }
          }
          break;
        case 'o':
          if (len > 2) {
            switch (start[2]) {
            case 'u':
              return check_kw(start, len, 3, 3, "ble", TOKEN_KW_DOUBLE);
            }
          } else {
            if (len == 2) {
              return TOKEN_KW_DO;
            }
          }
          break;
        case 'y':
          return check_kw(start, len, 2, 5, "namic", TOKEN_KW_DYNAMIC);
        }
      }
      break;
    case 'e':
      if (len > 1) {
        switch (start[1]) {
        case 'l':
          return check_kw(start, len, 2, 2, "se", TOKEN_KW_ELSE);
        case 'n':
          return check_kw(start, len, 2, 2, "um", TOKEN_KW_ENUM);
        case 'q':
          return check_kw(start, len, 2, 4, "uals", TOKEN_KW_EQUALS);
        case 'v':
          return check_kw(start, len, 2, 3, "ent", TOKEN_KW_EVENT);
        case 'x':
          if (len > 2) {
            switch (start[2]) {
            case 'p':
              return check_kw(start, len, 3, 5, "licit", TOKEN_KW_EXPLICIT);
            case 't':
              return check_kw(start, len, 3, 3, "ern", TOKEN_KW_EXTERN);
            }
          }
          break;
        }
      }
      break;
    case 'f':
      if (len > 1) {
        switch (start[1]) {
        case 'a':
          return check_kw(start, len, 2, 3, "lse", TOKEN_KW_FALSE);
        case 'i':
          if (len > 2) {
            switch (start[2]) {
            case 'n':
              return check_kw(start, len, 3, 4, "ally", TOKEN_KW_FINALLY);
            case 'x':
              return check_kw(start, len, 3, 2, "ed", TOKEN_KW_FIXED);
            }
          }
          break;
        case 'l':
          return check_kw(start, len, 2, 3, "oat", TOKEN_KW_FLOAT);
        case 'o':
          if (len > 2) {
            switch (start[2]) {
            case 'r':
              if (len > 3) {
                switch (start[3]) {
                case 'e':
                  return check_kw(start, len, 4, 3, "ach", TOKEN_KW_FOREACH);
                }
              } else {
                if (len == 3) {
                  return TOKEN_KW_FOR;
                }
              }
              break;
            }
          }
          break;
        case 'r':
          return check_kw(start, len, 2, 2, "om", TOKEN_KW_FROM);
        }
      }
      break;
    case 'g':
      if (len > 1) {
        switch (start[1]) {
        case 'e':
          return check_kw(start, len, 2, 1, "t", TOKEN_KW_GET);
        case 'l':
          return check_kw(start, len, 2, 4, "obal", TOKEN_KW_GLOBAL);
        case 'o':
          return check_kw(start, len, 2, 2, "to", TOKEN_KW_GOTO);
        case 'r':
          return check_kw(start, len, 2, 3, "oup", TOKEN_KW_GROUP);
        }
      }
      break;
    case 'i':
      if (len > 1) {
        switch (start[1]) {
        case 'f':
          if (len == 2) {
            return TOKEN_KW_IF;
          }
          break;
        case 'm':
          return check_kw(start, len, 2, 6, "plicit", TOKEN_KW_IMPLICIT);
        case 'n':
          if (len > 2) {
            switch (start[2]) {
            case 't':
              if (len > 3) {
                switch (start[3]) {
                case 'e':
                  if (len > 4) {
                    switch (start[4]) {
                    case 'r':
                      if (len > 5) {
                        switch (start[5]) {
                        case 'f':
                          return check_kw(start, len, 6, 3, "ace",
                                          TOKEN_KW_INTERFACE);
                        case 'n':
                          return check_kw(start, len, 6, 2, "al",
                                          TOKEN_KW_INTERNAL);
                        }
                      }
                      break;
                    }
                  }
                  break;
                case 'o':
                  if (len == 4) {
                    return TOKEN_KW_INTO;
                  }
                  break;
                }
              } else {
                if (len == 3) {
                  return TOKEN_KW_INT;
                }
              }
              break;
            }
          } else {
            if (len == 2) {
              return TOKEN_KW_IN;
            }
          }
          break;
        case 's':
          if (len == 2) {
            return TOKEN_KW_IS;
          }
          break;
        }
      }
      break;
    case 'j':
      return check_kw(start, len, 1, 3, "oin", TOKEN_KW_JOIN);
    case 'l':
      if (len > 1) {
        switch (start[1]) {
        case 'e':
          return check_kw(start, len, 2, 1, "t", TOKEN_KW_LET);
        case 'o':
          if (len > 2) {
            switch (start[2]) {
            case 'c':
              return check_kw(start, len, 3, 1, "k", TOKEN_KW_LOCK);
            case 'n':
              return check_kw(start, len, 3, 1, "g", TOKEN_KW_LONG);
            }
          }
          break;
        }
      }
      break;
    case 'n':
      if (len > 1) {
        switch (start[1]) {
        case 'a':
          if (len > 2) {
            switch (start[2]) {
            case 'm':
              if (len > 3) {
                switch (start[3]) {
                case 'e':
                  if (len > 4) {
                    switch (start[4]) {
                    case 'o':
                      return check_kw(start, len, 5, 1, "f", TOKEN_KW_NAMEOF);
                    case 's':
                      return check_kw(start, len, 5, 4, "pace",
                                      TOKEN_KW_NAMESPACE);
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
          return check_kw(start, len, 2, 1, "w", TOKEN_KW_NEW);
        case 'u':
          return check_kw(start, len, 2, 2, "ll", TOKEN_KW_NULL);
        }
      }
      break;
    case 'o':
      if (len > 1) {
        switch (start[1]) {
        case 'b':
          return check_kw(start, len, 2, 4, "ject", TOKEN_KW_OBJECT);
        case 'n':
          if (len == 2) {
            return TOKEN_KW_ON;
          }
          break;
        case 'p':
          return check_kw(start, len, 2, 6, "erator", TOKEN_KW_OPERATOR);
        case 'r':
          return check_kw(start, len, 2, 5, "derby", TOKEN_KW_ORDERBY);
        case 'u':
          return check_kw(start, len, 2, 1, "t", TOKEN_KW_OUT);
        case 'v':
          return check_kw(start, len, 2, 6, "erride", TOKEN_KW_OVERRIDE);
        }
      }
      break;
    case 'p':
      if (len > 1) {
        switch (start[1]) {
        case 'a':
          if (len > 2) {
            switch (start[2]) {
            case 'r':
              if (len > 3) {
                switch (start[3]) {
                case 'a':
                  return check_kw(start, len, 4, 2, "ms", TOKEN_KW_PARAMS);
                case 't':
                  return check_kw(start, len, 4, 3, "ial", TOKEN_KW_PARTIAL);
                }
              }
              break;
            }
          }
          break;
        case 'r':
          if (len > 2) {
            switch (start[2]) {
            case 'i':
              return check_kw(start, len, 3, 4, "vate", TOKEN_KW_PRIVATE);
            case 'o':
              return check_kw(start, len, 3, 6, "tected", TOKEN_KW_PROTECTED);
            }
          }
          break;
        case 'u':
          return check_kw(start, len, 2, 4, "blic", TOKEN_KW_PUBLIC);
        }
      }
      break;
    case 'r':
      if (len > 1) {
        switch (start[1]) {
        case 'e':
          if (len > 2) {
            switch (start[2]) {
            case 'a':
              return check_kw(start, len, 3, 5, "donly", TOKEN_KW_READONLY);
            case 'f':
              if (len == 3) {
                return TOKEN_KW_REF;
              }
              break;
            case 'm':
              return check_kw(start, len, 3, 3, "ove", TOKEN_KW_REMOVE);
            case 't':
              return check_kw(start, len, 3, 3, "urn", TOKEN_KW_RETURN);
            }
          }
          break;
        }
      }
      break;
    case 's':
      if (len > 1) {
        switch (start[1]) {
        case 'b':
          return check_kw(start, len, 2, 3, "yte", TOKEN_KW_SBYTE);
        case 'e':
          if (len > 2) {
            switch (start[2]) {
            case 'a':
              return check_kw(start, len, 3, 3, "led", TOKEN_KW_SEALED);
            case 'l':
              return check_kw(start, len, 3, 3, "ect", TOKEN_KW_SELECT);
            case 't':
              if (len == 3) {
                return TOKEN_KW_SET;
              }
              break;
            }
          }
          break;
        case 'h':
          return check_kw(start, len, 2, 3, "ort", TOKEN_KW_SHORT);
        case 'i':
          return check_kw(start, len, 2, 4, "zeof", TOKEN_KW_SIZEOF);
        case 't':
          if (len > 2) {
            switch (start[2]) {
            case 'a':
              if (len > 3) {
                switch (start[3]) {
                case 'c':
                  return check_kw(start, len, 4, 6, "kalloc",
                                  TOKEN_KW_STACKALLOC);
                case 't':
                  return check_kw(start, len, 4, 2, "ic", TOKEN_KW_STATIC);
                }
              }
              break;
            case 'r':
              if (len > 3) {
                switch (start[3]) {
                case 'i':
                  return check_kw(start, len, 4, 2, "ng", TOKEN_KW_STRING);
                case 'u':
                  return check_kw(start, len, 4, 2, "ct", TOKEN_KW_STRUCT);
                }
              }
              break;
            }
          }
          break;
        case 'w':
          return check_kw(start, len, 2, 4, "itch", TOKEN_KW_SWITCH);
        }
      }
      break;
    case 't':
      if (len > 1) {
        switch (start[1]) {
        case 'h':
          if (len > 2) {
            switch (start[2]) {
            case 'i':
              return check_kw(start, len, 3, 1, "s", TOKEN_KW_THIS);
            case 'r':
              return check_kw(start, len, 3, 2, "ow", TOKEN_KW_THROW);
            }
          }
          break;
        case 'r':
          if (len > 2) {
            switch (start[2]) {
            case 'u':
              return check_kw(start, len, 3, 1, "e", TOKEN_KW_TRUE);
            case 'y':
              if (len == 3) {
                return TOKEN_KW_TRY;
              }
              break;
            }
          }
          break;
        case 'y':
          return check_kw(start, len, 2, 4, "peof", TOKEN_KW_TYPEOF);
        }
      }
      break;
    case 'u':
      if (len > 1) {
        switch (start[1]) {
        case 'i':
          return check_kw(start, len, 2, 2, "nt", TOKEN_KW_UINT);
        case 'l':
          return check_kw(start, len, 2, 3, "ong", TOKEN_KW_ULONG);
        case 'n':
          if (len > 2) {
            switch (start[2]) {
            case 'c':
              return check_kw(start, len, 3, 6, "hecked", TOKEN_KW_UNCHECKED);
            case 's':
              return check_kw(start, len, 3, 3, "afe", TOKEN_KW_UNSAFE);
            }
          }
          break;
        case 's':
          if (len > 2) {
            switch (start[2]) {
            case 'h':
              return check_kw(start, len, 3, 3, "ort", TOKEN_KW_USHORT);
            case 'i':
              return check_kw(start, len, 3, 2, "ng", TOKEN_KW_USING);
            }
          }
          break;
        }
      }
      break;
    case 'v':
      if (len > 1) {
        switch (start[1]) {
        case 'a':
          if (len > 2) {
            switch (start[2]) {
            case 'l':
              return check_kw(start, len, 3, 2, "ue", TOKEN_KW_VALUE);
            case 'r':
              if (len == 3) {
                return TOKEN_KW_VAR;
              }
              break;
            }
          }
          break;
        case 'i':
          return check_kw(start, len, 2, 5, "rtual", TOKEN_KW_VIRTUAL);
        case 'o':
          if (len > 2) {
            switch (start[2]) {
            case 'i':
              return check_kw(start, len, 3, 1, "d", TOKEN_KW_VOID);
            case 'l':
              return check_kw(start, len, 3, 5, "atile", TOKEN_KW_VOLATILE);
            }
          }
          break;
        }
      }
      break;
    case 'w':
      if (len > 1) {
        switch (start[1]) {
        case 'h':
          if (len > 2) {
            switch (start[2]) {
            case 'e':
              if (len > 3) {
                switch (start[3]) {
                case 'n':
                  if (len == 4) {
                    return TOKEN_KW_WHEN;
                  }
                  break;
                case 'r':
                  return check_kw(start, len, 4, 1, "e", TOKEN_KW_WHERE);
                }
              }
              break;
            case 'i':
              return check_kw(start, len, 3, 2, "le", TOKEN_KW_WHILE);
            }
          }
          break;
        }
      }
      break;
    case 'y':
      return check_kw(start, len, 1, 4, "ield", TOKEN_KW_YIELD);
    }
  }
  return TOKEN_IDENTIFIER;
}
