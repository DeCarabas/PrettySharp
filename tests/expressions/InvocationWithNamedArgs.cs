class Foo{
    void Bar() {
        return CreateBoundDynamicMemberAccessOperation(implicitReceiver, typeArgumentsOpt: ImmutableArray<TypeSymbol>.Empty, memberName: "Add",
                                                       implicitReceiver.Syntax, type: null, value: default, isImplicit: true);
    }
}
