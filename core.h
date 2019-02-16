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
#ifndef CORE_H
#define CORE_H

#include <stdio.h>

enum DocType { DOC_TEXT, DOC_LINE, DOC_GROUP, DOC_END, DOC_BREAKPARENT };

struct Doc {
  enum DocType type;
  int margin;
  size_t length;
  const char *string;
};

struct DocBuilder {
  int count;
  int margin;
  int indent;
  int group_depth;

  int capacity;
  struct Doc *contents;
};

struct DocBuilder builder_new(int capacity);
void builder_free(struct DocBuilder *builder);

void doc_indent(struct DocBuilder *builder);
void doc_dedent(struct DocBuilder *builder);
void doc_text(struct DocBuilder *builder, const char *text, size_t length);
void doc_textz(struct DocBuilder *builder, const char *text);
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
