class Foo {
    void Bar() {
        switch (isolated)
        {
        case TerminatorState.IsNamespaceMemberStartOrStop:
            if (this.IsNamespaceMemberStartOrStop())
            {
                return true;
            }

            break;
        case TerminatorState.IsAttributeDeclarationTerminator:
            if (this.IsAttributeDeclarationTerminator())
            {
                return true;
            }

            break;
        }
    }

    private CSharpSyntaxNode CheckRecursivePatternFeature(CSharpSyntaxNode node)
    {
        switch (node.Kind)
        {
        case SyntaxKind.RecursivePattern:
        case SyntaxKind.DiscardPattern:
        case SyntaxKind.VarPattern when ((VarPatternSyntax)node).Designation.Kind == SyntaxKind.ParenthesizedVariableDesignation:
            return this.CheckFeatureAvailability(node, MessageID.IDS_FeatureRecursivePatterns);
        default:
            return node;
        }
    }
}
