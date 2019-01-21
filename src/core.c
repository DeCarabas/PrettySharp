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

void doc_text(struct DocBuilder *builder, const char *text, int length) {
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
}

void doc_end(struct DocBuilder *builder) {
  assert(builder->group_depth > 0);
  builder->group_depth -= 1;

  struct Doc *result = builder_add(builder);
  result->type = DOC_END;
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

enum OutputDocType { OUT_TEXT, OUT_LINE };

struct OutputDoc {
  enum OutputDocType type;
  int length;
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

static void output_push_string(struct OutputBuilder *result, int length,
                               const char *string) {
  struct OutputDoc *doc = output_add(result);
  doc->type = OUT_TEXT;
  doc->length = length;
  doc->string = string;
}

static void output_push_line(struct OutputBuilder *result, int margin) {
  struct OutputDoc *doc = output_add(result);
  doc->type = OUT_LINE;
  doc->length = margin;
  doc->string = NULL;
}

static void best_rep(struct OutputBuilder *result, int width, struct Doc *docs,
                     int length) {
  struct Doc *saved_it = NULL;
  int saved_result_count = 0;
  int saved_used = 0;
  int group_depth = 0;

  struct Doc *end = docs + length;
  struct Doc *it = docs;
  int used = 0;
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
        // need in order to rewind out of this group. Note that saved_it is it
        // + 1, so we don't revisit this 'GROUP' instruction if we have to
        // reset.
        saved_result_count = result->count;
        saved_it = it + 1;
        saved_used = used;
      }
      break;

    case DOC_END:
      assert(group_depth > 0);
      group_depth -= 1;
      if (group_depth == 0) {
        // Back to breaking again; clear these for diagnostic purposes. (We
        // don't really need to, of course.)
        saved_result_count = 0;
        saved_it = NULL;
        saved_used = used;
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
      fwrite(it->string, 1, it->length, file);
      break;

    case OUT_LINE:
      fputs("\n", file);
      for (int i = 0; i < it->length; i++) {
        fputc(' ', file);
      }
      break;
    }
    it++;
  }
}

void pretty(FILE *file, int width, struct Doc *docs, int length) {
  struct OutputBuilder builder;
  output_init(&builder);

  best_rep(&builder, width, docs, length);
  layout(file, builder.contents, builder.count);

  output_free(&builder);
}

void dump_docs(struct Doc *docs, int length) {
  // Haha use ourselves to debug ourselves.
  struct DocBuilder builder = builder_new(16);

  struct Doc *it = docs;
  struct Doc *end = docs + length;
  while (it != end) {
    switch (it->type) {
    case DOC_TEXT:
      doc_textz(&builder, "TEXT \"");
      doc_text(&builder, it->string, it->length);
      doc_textz(&builder, "\";");
      doc_line(&builder);
      break;
    case DOC_LINE:
      doc_textz(&builder, "LINE;");
      doc_line(&builder);
      break;
    case DOC_GROUP:
      doc_group(&builder);
      doc_indent(&builder);
      doc_textz(&builder, "BEGIN");
      doc_line(&builder);
      break;
    case DOC_END:
      doc_dedent(&builder);
      doc_textz(&builder, "END");
      doc_end(&builder);
      doc_line(&builder);
      break;
    case DOC_BREAKPARENT:
      doc_textz(&builder, "BREAKPARENT");
      doc_line(&builder);
      break;
    }
    it++;
  }

  pretty(stdout, 80, builder.contents, builder.count);
}
