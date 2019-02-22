class Foo {
    void Bar() {
        foreach (var (value, label) in node.Cases)
        {
            casesBuilder.Add((value, GetLabelClone(label)));
        }
    }
}
