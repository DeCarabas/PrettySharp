class Foo {
    // This dumbness is apparently legal.
    internal virtual bool IsDirectlyExcludedFromCodeCoverage { get => false; }
}
