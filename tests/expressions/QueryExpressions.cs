class Foo {
    public override ImmutableArray<NamedTypeSymbol> GetTypeMembers()
    {
        return (from c in _children
                where c is NamedTypeSymbol
                select (NamedTypeSymbol)c).ToArray().AsImmutableOrNull();
    }
}
