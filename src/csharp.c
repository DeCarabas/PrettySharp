#include "core.h"

// N.B.: The numeric values in this enum correspond to slots in the
// visit_dispatch table, below.
enum SyntaxKind { Syntax_Block = 0, Syntax_VariableDeclaration = 1 };

struct syntaxnode {
  enum SyntaxKind kind;
};

struct variabledeclarationsyntax {
  struct syntaxnode node;
};

struct whilestatementsyntax {
  struct syntaxnode node;
  struct syntaxnode *condition;
  struct syntaxnode *body;
};

struct blocksyntnax {
  struct syntaxnode node;
};

typedef void (*visitor)(struct docbuilder *builder, struct syntaxnode *node);

void visit(struct docbuilder *builder, struct syntaxnode *node);

void visit_body(struct docbuilder *builder, struct syntaxnode *node) {
  if (node->kind != Syntax_Block) {
    doc_indent(builder);
  }

  doc_line(builder);
  visit(builder, node);

  if (node->kind != Syntax_Block) {
    doc_dedent(builder);
  }
}

void visit_block(struct docbuilder *builder, struct blocksyntax *node) {}

void visit_variabledeclaration(struct docbuilder *builder,
                               struct variabledeclarationsyntax *node) {
  doc_group(builder);
  {
    doc_group(builder);

    doc_end(builder);
  }
  doc_end(builder);
}

void visit_while(struct docbuilder *builder,
                 struct whilestatementsyntax *node) {
  doc_breakparent(builder);
  doc_group(builder);

  // while (condition)
  doc_bracket_open(builder, "while (");
  visit(builder, node->condition);
  doc_bracket_close(builder, ")");

  // { ... }
  visit_body(builder, node->body);

  doc_end(builder);
}

void visit(struct docbuilder *builder, struct syntaxnode *node) {
  // One per SyntaxKind value.
  static visitor visit_dispatch[] = {(visitor)visit_block,
                                     (visitor)visit_variabledeclaration};

  visit_dispatch[node->kind](builder, node);
}
