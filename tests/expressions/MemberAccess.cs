class Foo {
    public IEnumerable<SyntaxNode> Ancestors(bool ascendOutOfTrivia = true)
    {
        return this.Parent?
            .AncestorsAndSelf(ascendOutOfTrivia) ??
            SpecializedCollections.EmptyEnumerable<SyntaxNode>();
    }
}
