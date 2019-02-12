class Foo {
    public Matrix Prop1
    {
        get
        {
            return this.viewMatrix.GetValueMatrix();
        }
        set
        {
            this.viewMatrix.SetValue(value);
        }
    }
}
