class Foo {
    void Bar() {
        var await = (BoundAwaitExpression)boundNode;
        isDynamic = await.AwaitableInfo.IsDynamic;
    }

    async Task Baz() {
        await SomethingFancy();
    }
}
