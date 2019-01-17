#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "core.h"
#include "csharp.h"

#define ERR_INVALID_ARGS 1
#define ERR_CANNOT_OPEN 2
#define ERR_CANNOT_READ 3

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

  return buffer;
}

int main(int argc, const char *argv[]) {
  FILE *input;
  if (argc == 1) {
    input = stdin;
  } else if (argc == 2) {
    input = fopen(argv[1], "r");
    if (!input) {
      fprintf(stderr, "Unable to open input file '%s': %s", argv[1],
              strerror(errno));
      exit(ERR_CANNOT_OPEN);
    }
  } else {
    fprintf(stderr, "Unrecognized command line arguments.\n");
    exit(ERR_INVALID_ARGS);
  }

  char *source = read_file(input);
  struct DocBuilder builder = builder_new(16);

  if (format_csharp(&builder, source)) {
    pretty(stdout, 80, builder.contents, builder.count);
  }

  builder_free(&builder);
  free(source);
  return 0;
}
