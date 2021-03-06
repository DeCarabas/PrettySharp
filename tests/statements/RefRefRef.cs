class Foo {
    void Bar() {
        ref LocalState stateToUpdate = ref (op == BinaryOperatorKind.Equal) ? ref this.StateWhenFalse : ref this.StateWhenTrue;
    }

    SpanLikeType M1(ref SpanLikeType x, Span<byte> y)
    {
        // this is all valid, unconcerned with stack-referring stuff
        var local = new SpanLikeType(y);
        x = local;
        return x;
    }

    void Test1(ref SpanLikeType param1, Span<byte> param2)
    {
        Span<byte> stackReferring1 = stackalloc byte[10];
        var stackReferring2 = new SpanLikeType(stackReferring1);

        // this is allowed
        stackReferring2 = M1(ref stackReferring2, stackReferring1);

        // this is NOT allowed
        stackReferring2 = M1(ref param1, stackReferring1);

        // this is NOT allowed
        param1 = M1(ref stackReferring2, stackReferring1);

        // this is NOT allowed
        param2 = stackReferring1.Slice(10);

        // this is allowed
        param1 = new SpanLikeType(param2);

        // this is allowed
        stackReferring2 = param1;
    }

    ref SpanLikeType M2(ref SpanLikeType x)
    {
        return ref x;
    }

    ref SpanLikeType Test2(ref SpanLikeType param1, Span<byte> param2)
    {
        Span<byte> stackReferring1 = stackalloc byte[10];
        var stackReferring2 = new SpanLikeType(stackReferring1);

        ref var stackReferring3 = M2(ref stackReferring2);

        // this is allowed
        stackReferring3 = M1(ref stackReferring2, stackReferring1);

        // this is allowed
        M2(ref stackReferring3) = stackReferring2;

        // this is NOT allowed
        M1(ref param1) = stackReferring2;

        // this is NOT allowed
        param1 = stackReferring3;

        // this is NOT allowed
        return ref stackReferring3;

        // this is allowed
        return ref param1;
    }
}
