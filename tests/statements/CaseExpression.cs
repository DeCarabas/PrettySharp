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

            // Haha this too!
        case DeclarationModifiers.Protected | DeclarationModifiers.Internal:
            // the two keywords "protected" and "internal" together are treated as one modifier.
            result &= ~DeclarationModifiers.AccessibilityMask;
            result |= DeclarationModifiers.ProtectedInternal;
            break;

        default:
            return node;
        }
    }
}
