class Foo {
    void Bar() {
        // Do not recurse into constant expressions. Their children do not push any values.
        var result = node.ConstantValue == null ?
            node = (BoundExpression)base.Visit(node) :
            node;
    }
}
