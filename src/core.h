#ifndef CORE_H
#define CORE_H

#include <stdio.h>

enum DocType { DOC_TEXT, DOC_LINE, DOC_GROUP, DOC_END, DOC_BREAKPARENT };

struct Doc {
  enum DocType type;
  int margin;
  int length;
  const char *string;
};

struct DocBuilder {
  int count;
  int capacity;
  int margin;
  int indent;
  int group_depth;
  struct Doc *contents;
};

struct DocBuilder builder_new(int capacity);
void builder_free(struct DocBuilder *builder);

void doc_indent(struct DocBuilder *builder);
void doc_dedent(struct DocBuilder *builder);
void doc_text(struct DocBuilder *builder, const char *text, int length);
void doc_line(struct DocBuilder *builder);
void doc_softline(struct DocBuilder *builder);
void doc_breakparent(struct DocBuilder *builder);
void doc_group(struct DocBuilder *builder);
void doc_end(struct DocBuilder *builder);
void doc_bracket_open(struct DocBuilder *builder, const char *left);
void doc_bracket_close(struct DocBuilder *builder, const char *right);

void pretty(FILE *file, int width, struct Doc *docs, int length);
void dump_docs(struct Doc *docs, int length);

#endif
