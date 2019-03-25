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
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "core.h"

static void builder_init(struct DocBuilder *docs, int capacity) {
  docs->count = 0;
  docs->capacity = capacity;
  docs->margin = 0;
  docs->indent = 4;
  docs->group_depth = 0;
  docs->last_group_start = (size_t)-1;
  docs->contents = malloc(sizeof(struct Doc) * docs->capacity);
}

struct DocBuilder builder_new(int capacity) {
  struct DocBuilder result;
  builder_init(&result, capacity);
  return result;
}

void builder_free(struct DocBuilder *builder) {
  free(builder->contents);
  builder->contents = NULL;
  builder->count = 0;
  builder->capacity = 0;
}

static struct Doc *builder_add(struct DocBuilder *builder) {
  builder->count += 1;
  if (builder->count > builder->capacity) {
    builder->capacity += (builder->capacity / 2);
    builder->contents =
        realloc(builder->contents, sizeof(struct Doc) * builder->capacity);
  }

  struct Doc *doc = builder->contents + (builder->count - 1);
  doc->margin = builder->margin;
  return doc;
}

void doc_indent(struct DocBuilder *builder) {
  builder->margin += builder->indent;
  assert(builder->margin >= 0);
}

void doc_dedent(struct DocBuilder *builder) {
  builder->margin -= builder->indent;
  assert(builder->margin >= 0);
}

void doc_text(struct DocBuilder *builder, const char *text, size_t length) {
  struct Doc *out = builder_add(builder);
  out->type = DOC_TEXT;
  out->length = length;
  out->string = text;
}

void doc_textz(struct DocBuilder *builder, const char *text) {
  struct Doc *out = builder_add(builder);
  out->type = DOC_TEXT;
  out->length = strlen(text);
  out->string = text;
}

void doc_line(struct DocBuilder *builder) {
  struct Doc *out = builder_add(builder);
  out->type = DOC_LINE;
  out->length = 1;
  out->string = " ";
}

void doc_softline(struct DocBuilder *builder) {
  struct Doc *out = builder_add(builder);
  out->type = DOC_LINE;
  out->length = 0;
  out->string = "";
}

void doc_breakparent(struct DocBuilder *builder) {
  struct Doc *out = builder_add(builder);
  out->type = DOC_BREAKPARENT;
}

void doc_group(struct DocBuilder *builder) {
  builder->group_depth += 1;
  assert(builder->group_depth > 0);

  struct Doc *result = builder_add(builder);
  result->type = DOC_GROUP;

  // Because this is open, we just stash the previous group start in here.
  result->length = builder->last_group_start;
  builder->last_group_start = builder->count - 1;
}

void doc_end(struct DocBuilder *builder) {
  assert(builder->group_depth > 0);
  builder->group_depth -= 1;

  struct Doc *result = builder_add(builder);
  result->type = DOC_END;

  // Patch up the length, so we can jump from the beginning of a group to the
  // end if we want to.
  struct Doc *group_start = &(builder->contents[builder->last_group_start]);
  assert(group_start->type == DOC_GROUP);
  builder->last_group_start = group_start->length;
  group_start->length = (result - group_start) + 1;
}

void doc_bracket_open(struct DocBuilder *builder, const char *left) {
  doc_group(builder);
  doc_text(builder, left, strlen(left));

  doc_indent(builder);
  doc_softline(builder);
}

void doc_bracket_close(struct DocBuilder *builder, const char *right) {
  doc_dedent(builder);
  doc_softline(builder);
  doc_text(builder, right, strlen(right));
  doc_end(builder);
}

static size_t doc_node_length(struct DocBuilder *builder, size_t start) {
  struct Doc *doc = &(builder->contents[start]);
  assert(doc->type != DOC_END);
  if (doc->type == DOC_GROUP) {
    return doc->length;
  } else {
    return 1;
  }
}

size_t doc_rotate_left(struct DocBuilder *builder, size_t start) {
  size_t x_start = start;
  size_t x_len = doc_node_length(builder, x_start);
  size_t y_start = x_start + x_len;

  if (y_start == builder->count) {
    // Nothing to do: this node has no right sibling.
    return x_start;
  }
  if (builder->contents[y_start].type != DOC_GROUP) {
    // Nothing to do; x is nestled as deep in the tree as it can go.
    return x_start;
  }

  // Z starts out as everything in the group....
  assert(builder->contents[y_start].type == DOC_GROUP);
  size_t z_len = builder->contents[y_start].length - 2;

  // ...but Y is the first thing in the group...
  y_start = y_start + 1;
  size_t y_len = doc_node_length(builder, y_start);

  // ...so Z is the remainder of stuff in the group.
  size_t z_start = y_start + y_len;
  z_len -= y_len;

  // z_len might be zero, but it better not be negative!
  assert(z_len >= 0);

  // Shift x to the right one, displacing the previous group start. The end of x
  // will clobber the group start between x and y.
  memmove(builder->contents + x_start + 1, builder->contents + x_start,
          sizeof(struct Doc) * x_len);

  // Rewrite a group start where x used to start. (You can think of this as
  // putting back the group start we clobbered.)
  struct Doc *xy_group = &builder->contents[x_start];
  xy_group->type = DOC_GROUP;
  xy_group->length = 1 + x_len + y_len + 1; // GROUP + x + y + END

  // Move z to the right by one, displacing the previous group end. The end of z
  // will clobber what used to be the end of (y z)
  memmove(builder->contents + z_start + 1, builder->contents + z_start,
          sizeof(struct Doc) * z_len);

  // Rewrite the corresponding group end where z used to start. (Again, think of
  // this as restoring the group end we had at the end of z.
  struct Doc *xy_end = &builder->contents[z_start];
  xy_end->type = DOC_END;
  xy_end->length = 0;
  xy_end->string = NULL;

  // Make sure we patched everything up properly!
  assert(xy_group + xy_group->length - 1 == xy_end);
  return x_start + 1;
}

size_t doc_rotate_left_deep(struct DocBuilder *builder, size_t start) {
  size_t new_start = start;
  do {
    start = new_start;
    new_start = doc_rotate_left(builder, start);
  } while (start != new_start);
  return new_start;
}

enum OutputDocType { OUT_TEXT, OUT_LINE };

struct OutputDoc {
  enum OutputDocType type;
  size_t length;
  const char *string;
};

struct OutputBuilder {
  int count;
  int capacity;
  struct OutputDoc *contents;
};

static void output_init(struct OutputBuilder *builder) {
  builder->count = 0;
  builder->capacity = 16;
  builder->contents = malloc(sizeof(struct OutputDoc) * 16);
}

static void output_free(struct OutputBuilder *builder) {
  builder->count = 0;
  builder->capacity = 0;
  free(builder->contents);
  builder->contents = NULL;
}

static struct OutputDoc *output_add(struct OutputBuilder *builder) {
  builder->count += 1;
  if (builder->count > builder->capacity) {
    builder->capacity += (builder->capacity / 2);
    builder->contents = realloc(builder->contents,
                                sizeof(struct OutputDoc) * builder->capacity);
  }

  return builder->contents + (builder->count - 1);
}

static void output_push_string(struct OutputBuilder *result, size_t length,
                               const char *string) {
  struct OutputDoc *doc = output_add(result);
  doc->type = OUT_TEXT;
  doc->length = length;
  doc->string = string;
}

static void output_push_line(struct OutputBuilder *result, int margin) {
  assert(margin >= 0);

  struct OutputDoc *doc = output_add(result);
  doc->type = OUT_LINE;
  doc->length = margin;
  doc->string = NULL;
}

static void best_rep(struct OutputBuilder *result, size_t width,
                     struct Doc *docs, int length) {
  struct Doc *saved_it = NULL;
  int saved_result_count = 0;
  size_t saved_used = 0;
  int group_depth = 0;

  struct Doc *end = docs + length;
  struct Doc *it = docs;
  size_t used = 0;
  while (it != end) {
    switch (it->type) {
    case DOC_LINE:
      if (group_depth == 0) {
        // We're not in a group, so emit a newline and margin and stuff.
        output_push_line(result, it->margin);
        used = it->margin;
        break;
      }

      // Otherwise, we're *not* breaking lines and we should treat this as a
      // TEXT instruction, in which case...
      // fallthrough
    case DOC_TEXT:
      output_push_string(result, it->length, it->string);
      used += it->length;
      if (used > width && group_depth > 0) {
        // Rewind output, and reset input; we're not grouped anymore.
        result->count = saved_result_count;
        it = saved_it;
        used = saved_used;
        group_depth = 0;
      }
      break;

    case DOC_BREAKPARENT:
      if (group_depth > 0) {
        // Immediately break all enclosing parents, as if we had overflowed
        // the text.
        result->count = saved_result_count;
        it = saved_it;
        used = saved_used;
        group_depth = 0;
      }
      break;

    case DOC_GROUP:
      group_depth += 1;
      assert(group_depth > 0);
      if (group_depth == 1) {
        // Transition from breaking to not breaking, so capture everything we
        // need in order to rewind out of this group. It's OK that we save 'it'
        // and not 'it + 1' because when we restore we will increment 'it' at
        // the end of the loop.
        saved_result_count = result->count;
        saved_it = it;
        saved_used = used;
      }
      break;

    case DOC_END:
      // We can have END without BEGIN if we skipped the parent BEGINS.
      if (group_depth > 0) {
        group_depth -= 1;
        if (group_depth == 0) {
          // Back to breaking again; clear these for diagnostic purposes. (We
          // don't really need to, of course.)
          saved_result_count = 0;
          saved_it = NULL;
          saved_used = used;
        }
      }
      break;
    }
    it++;
  }
}

static void layout(FILE *file, struct OutputDoc *docs, int length) {
  struct OutputDoc *end = docs + length;
  struct OutputDoc *it = docs;
  while (it != end) {
    switch (it->type) {
    case OUT_TEXT:
      fprintf(file, "%.*s", (int)it->length, it->string);
      break;

    case OUT_LINE:
      fputs("\n", file);

      // Don't write spaces if we're just going to write another blank line,
      // it's ugly.
      struct OutputDoc *next = it + 1;
      if (next != end && next->type != OUT_LINE) {
        for (size_t i = 0; i < it->length; i++) {
          fputc(' ', file);
        }
      }
      break;
    }
    it++;
  }
}

void pretty(FILE *file, size_t width, struct Doc *docs, int length) {
  struct OutputBuilder builder;
  output_init(&builder);

  best_rep(&builder, width, docs, length);
  layout(file, builder.contents, builder.count);

  output_free(&builder);
}

void dump_docs(struct Doc *docs, int length) {
  // Haha use ourselves to debug ourselves.
  struct DocBuilder builder = builder_new(16);
  builder.indent = 2;

  struct Doc *it = docs;
  struct Doc *end = docs + length;
  while (it != end) {
    switch (it->type) {
    case DOC_TEXT:
      doc_line(&builder);
      doc_textz(&builder, "TEXT \"");
      doc_text(&builder, it->string, it->length);
      doc_textz(&builder, "\";");
      break;
    case DOC_LINE:
      doc_line(&builder);
      doc_textz(&builder, "LINE;");
      break;
    case DOC_GROUP:
      doc_line(&builder);
      doc_group(&builder);
      doc_indent(&builder);
      doc_textz(&builder, "{");
      break;
    case DOC_END:
      doc_dedent(&builder);
      doc_line(&builder);
      doc_textz(&builder, "}");
      doc_end(&builder);
      break;
    case DOC_BREAKPARENT:
      doc_line(&builder);
      doc_textz(&builder, "BREAKPARENT;");
      break;
    }
    it++;
  }
  doc_line(&builder);

  pretty(stdout, 80, builder.contents, builder.count);
}
