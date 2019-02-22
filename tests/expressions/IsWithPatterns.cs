class Foo {
    void Bar() {
        if (_lazyDiscardSymbol is null)
        {
            Debug.Assert(!(this.InputType is null));
            _lazyDiscardSymbol = new DiscardSymbol(this.InputType);
        }
    }
}
