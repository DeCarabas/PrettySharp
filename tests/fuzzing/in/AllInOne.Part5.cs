namespace ConsoleApplication1
{
    class Test
    {
        void Bar3()
        {
            var x = new Boo.Bar<int>.Foo<object>();
            x.Method<string, string>(" ", 5, new object());

            var q = from i in new int[] { 1, 2, 3, 4 }
                    where i > 5
                    select i;
        }

        public static implicit operator Test(string s)
        {
            return new ConsoleApplication1.Test();
        }
        public static explicit operator Test(string s = "")
        {
            return new Test();
        }

        public int foo = 5;
        void Bar2()
        {
            foo = 6;
            this.Foo = 5.GetType(); Test t = "sss";
        }

        public event EventHandler MyEvent = delegate { };

        void Blah()
        {
            int i = 5;
            int? j = 6;

            Expression<Func<int>> e = () => i;
            Expression<Func<bool, Action>> e2 = b => () => { return; };
            Func<bool, bool> f = async delegate (bool a)
            {
                return await !a;
            };
            Func<int, int, int> f2 = (a, b) => 0;
            f2 = (int a, int b) => 1;
            Action a = Blah;
            f2 = () => {};
            f2 = () => {;};
        }

        delegate Recursive Recursive(Recursive r);
        delegate Recursive Recursive<A,R>(Recursive<A,R> r);

        public Type Foo
        {
            [Obsolete("Name", error = false)]
            get
            {
                var result = typeof(IEnumerable<int>);
                var t = typeof(int?) == typeof(Nullable<int>);
                t = typeof(IEnumerable<int?[][][]>);
                return typeof(IEnumerable<>);
            }
            set
            {
                var t = typeof(System.Int32);
                t.ToString();
                t = value;
            }
        }

        public void Constants()
        {
            int i = 1 + 2 + 3 + 5;
            global::System.String s = "a" + (System.String)"a" + "a" + "a" + "a" + "A";
        }

        public void ConstructedType()
        {
            List<int> i = null;
            int c = i.Count;
        }
    }
}
