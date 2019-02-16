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

  for (int i = 1; i < argc; i++) {
    const char *arg = argv[i];
    if (arg[0] == '-') {
      if (strcmp(arg, "-dt") == 0) {
        dump_tokens = true;
      } else if (strcmp(arg, "-dd") == 0) {
        dump_doc = true;
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
