class Foo {
    public override BoundNode VisitMethodGroup(BoundMethodGroup node) => throw ExceptionUtilities.Unreachable;
}
