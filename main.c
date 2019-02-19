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
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "core.h"
#include "csharp.h"
#include "lexer.h"

void print_version_string() {
  printf("prettysharp C# Source Code Formatter 0.9.0\n"
         "Copyright (c) 2019 John Doty\n"
         "https://github.com/DeCarabas/PrettySharp\n\n"
         "This is free software; you are free to change and redistribute it\n"
         "under the terms of the GNU General Public License version 3 or\n"
         "later.\n");
}

void print_usage_message() {
  print_version_string();
  printf("\n"
         "Usage: prettysharp [options] [<input file>]\n\n"
         "By default, input is read from stdin, and formatted C# is written\n"
         "to stdout.\n\n"
         "Options:\n"
         "\n"
         "  --version      Print version information.\n"
         "  -h -? --help   Show this help.\n"
         "  <input file>   The name of the input file. If not provided, read\n"
         "                 from stdin.\n");
}

char *read_file(FILE *input) {
  const int chunk_size = 4 * 1024 * 1024;

  size_t buffer_size = chunk_size;
  char *buffer = malloc(buffer_size);
  char *end = buffer + buffer_size;
  char *cursor = buffer;

  for (;;) {
    size_t available = end - cursor;
    size_t read = fread(cursor, 1, available, input);
    cursor += read;

    if (feof(input)) {
      break;
    } else if (ferror(input)) {
      fprintf(stderr, "An error occurred reading the input file.\n");
      exit(ERR_CANNOT_READ);
    }

    if (cursor == end) {
      size_t new_buffer_size = buffer_size * 2;
      buffer = realloc(buffer, new_buffer_size);
      end = buffer + new_buffer_size;
      cursor = buffer + buffer_size;
      buffer_size = new_buffer_size;
    }
  }

  if (cursor == end) {
    size_t new_buffer_size = buffer_size + 1;
    buffer = realloc(buffer, new_buffer_size);
    end = buffer + new_buffer_size;
    cursor = buffer + buffer_size;
    buffer_size = new_buffer_size;
  }
  *cursor = '\0';

  return buffer;
}

FILE *open_input_file(const char *fname) {
  if (fname) {
    FILE *input = fopen(fname, "r");
    if (!input) {
      fprintf(stderr, "Unable to open input file '%s': %s", fname,
              strerror(errno));
      exit(ERR_CANNOT_OPEN);
    }
    return input;
  } else {
    return stdin;
  }
}

int main(int argc, const char *argv[]) {
  FILE *input;
  const char *fname = NULL;
  bool dump_tokens = false;
  bool dump_doc = false;
  bool print_version = false;
  bool show_help = false;

  for (int i = 1; i < argc; i++) {
    const char *arg = argv[i];
    if (arg[0] == '-') {
      if (strcmp(arg, "-dt") == 0) {
        dump_tokens = true;
      } else if (strcmp(arg, "-dd") == 0) {
        dump_doc = true;
      } else if (strcmp(arg, "--version") == 0) {
        print_version = true;
      } else if ((strcmp(arg, "--help") == 0) || (strcmp(arg, "-?") == 0) ||
                 (strcmp(arg, "-h") == 0)) {
        show_help = true;
      } else {
        fprintf(stderr, "Unrecognized switch: %s\n", arg);
        exit(ERR_INVALID_ARGS);
      }
    } else if (fname) {
      fprintf(stderr, "Duplicate input file: %s\n", arg);
      exit(ERR_INVALID_ARGS);
    } else {
      fname = arg;
    }
  }

  if (print_version) {
    print_version_string();
    exit(0);
  }

  if (show_help) {
    print_usage_message();
    exit(0);
  }

  input = open_input_file(fname);

  int rc = ERR_PARSE_ERROR;
  char *source = read_file(input);
  if (dump_tokens) {
    dump_lex(source);
    exit(0);
  }

  struct DocBuilder builder = builder_new(16);
  if (format_csharp(&builder, source)) {
    if (dump_doc) {
      dump_docs(builder.contents, builder.count);
      exit(0);
    }

    pretty(stdout, 80, builder.contents, builder.count);
    rc = 0;
  }
  builder_free(&builder);

  free(source);
  return rc;
}
