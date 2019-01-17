#include "core.h"

// N.B.: The numeric values in this enum correspond to slots in the
// visit_dispatch table, below.
enum SyntaxKind { Syntax_Block = 0, Syntax_VariableDeclaration = 1 };

struct SyntaxNode {
  enum SyntaxKind kind;
};

struct VariableDeclarationSyntax {
  struct SyntaxNode node;
};

struct WhileStatementSyntax {
  struct SyntaxNode node;
  struct SyntaxNode *condition;
  struct SyntaxNode *body;
};

struct BlockSyntax {
  struct SyntaxNode node;
};

typedef void (*visitor)(struct DocBuilder *builder, struct SyntaxNode *node);

void visit(struct DocBuilder *builder, struct SyntaxNode *node);

void visit_body(struct DocBuilder *builder, struct SyntaxNode *node) {
  if (node->kind != Syntax_Block) {
    doc_indent(builder);
  }

  doc_line(builder);
  visit(builder, node);

  if (node->kind != Syntax_Block) {
    doc_dedent(builder);
  }
}

void visit_block(struct DocBuilder *builder, struct BlockSyntax *node) {}

void visit_variabledeclaration(struct DocBuilder *builder,
                               struct VariableDeclarationSyntax *node) {
  doc_group(builder);
  {
    doc_group(builder);

    doc_end(builder);
  }
  doc_end(builder);
}

void visit_while(struct DocBuilder *builder,
                 struct WhileStatementSyntax *node) {
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

void visit(struct DocBuilder *builder, struct SyntaxNode *node) {
  // One per SyntaxKind value.
  static visitor visit_dispatch[] = {(visitor)visit_block,
                                     (visitor)visit_variabledeclaration};

  visit_dispatch[node->kind](builder, node);
}
