class A
{
        ~A()
        {
        }
        private readonly int f1;
        [Obsolete]
        [NonExisting]
        [Foo::NonExisting(var, 5)]
        [CLSCompliant(false)]
        [Obsolete, System.NonSerialized, NonSerialized, CLSCompliant(true || false & true)]
        private volatile int f2;
        [return: Obsolete]
        [method: Obsolete]
        public void Handler(object value)
        {
        }

        (string, string, long) LookupName(long id)
        {
            var unnamed = ("one", "two", "three");
            var named = (first: "one", second: "two");
            (string, long) foo = ("foo", 42);
            (int? a, int? b) nullableMembers = (5, 10);
            (int, (int, int)) nestedTuple = (1, (2, 3));
            var (a, b, c) = LookupName(42);
            (a, b, c) = (a, b, c);
            return (named.first, named.second, id);
        }
        public int m<T>(T t)
          where T : class, new()
        {
            base.m(t);
            return 1;
        }
        public string P
        {
            get
            {
                return "A";
            }
            set;
        }
        public abstract string P
        {
            get;
        }
        public abstract int this[int index]
        {
            protected internal get;
            internal protected set;
        }
        [method: Obsolete]
        [field: Obsolete]
        [event: Obsolete]
        public readonly event Event E;
        [event: Test]
        public event Action E1
        {
            [Obsolete]
            add { value = value; }
            [Obsolete]
            [return: Obsolete]
            remove { E += Handler; E -= Handler; }
        }
        public static A operator +(A first, A second)
        {
            Delegate handler = new Delegate(Handler);
            return first.Add(second);
        }
        [method: Obsolete]
        [return: Obsolete]
        public static bool operator true(A a)
        {
            return true;
        }
        public static bool operator false(A a)
        {
            return false;
        }
        class C
        {
        }
}
