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
}