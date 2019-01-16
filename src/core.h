#ifndef CORE_H
#define CORE_H

enum DocType { DOC_TEXT, DOC_LINE, DOC_GROUP, DOC_END, DOC_BREAKPARENT };

struct doc {
  enum DocType type;
  int margin;
  int length;
  const char *string;
};

struct docbuilder {
  int count;
  int capacity;
  int margin;
  int indent;
  struct doc *contents;
};

void doc_indent(struct docbuilder *builder);
void doc_dedent(struct docbuilder *builder);
void doc_text(struct docbuilder *builder, const char *text);
void doc_line(struct docbuilder *builder);
void doc_softline(struct docbuilder *builder);
void doc_breakparent(struct docbuilder *builder);
void doc_group(struct docbuilder *builder);
void doc_end(struct docbuilder *builder);
void doc_bracket_open(struct docbuilder *builder, const char *left);
void doc_bracket_close(struct docbuilder *builder, const char *right);

#endif
