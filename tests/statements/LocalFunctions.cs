class Foo {
    void Bar() {
        IOperation createReceiver() => memberSymbol?.IsStatic == true ?
            null :
            CreateImplicitReceiver(boundObjectInitializerMember.Syntax, boundObjectInitializerMember.ReceiverType);
    }
}
