namespace My.Moy
{
    using A.B;

    interface CoContra<out T, in K> { }
    delegate void CoContra2<[System.Obsolete()] out T, in K> (int i) where T : struct;

    public unsafe partial class A : C, I
    {
        [DllImport("kernel32", SetLastError = true)]
        static extern bool CreateDirectory(string name, SecurityAttribute sa);

        private const int global = int.MinValue - 1;

        static A()
        {
            a.a.d().e().a->c.a++.b().a;

            var x = 1 * 1 + 1 * 1 + (1+(1+1)) + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1  + 1 + 1 + 1;

            var y = a is Dictionary<string, object> dict ? 42 : 51;
        }

        [method: Obsolete]
        public A([param: Obsolete] int foo, [param: Obsolete] int foo, [param: Obsolete] int foo, [param: Obsolete] int foo) :
            base(1)
        {
            if (i > 0)
            {
                return;
            }
            else if (i == 0)
            {
                throw new Exception();
            }
            var o1 = new MyObject();
            var o2 = new MyObject(var);
            var o3 = new MyObject { A = i };
            var o4 = new MyObject(@dynamic)
            {
                A = 0,
                B = 0,
                C = 0
            };
            var o5 = new { A = 0 };
            var dictionaryInitializer = new Dictionary<int, string>
            {
                {1, ""},
                {2, "a"}
            };
            float[] a = new float[]
            {
                0f,
                1.1f
            };
            int[, ,] cube = { { { 1111, 1121, }, { 1211, 1221 } }, { { 2111, 2121 }, { 2211, 2221 } } };
            int[][] jagged = { { 111 }, { 121, 122 } };
            int[][,] arr = new int[5][,]; // as opposed to new int[][5,5]
            arr[0] = new int[5,5];  // as opposed to arr[0,0] = new int[5];
            arr[0][0,0] = 47;
            int[] arrayTypeInference = new[] { 0, 1, };
            switch (3) { }
            switch (i)
            {
                case 0:
                case 1:
                    {
                        goto case 2;
                    }
                case 2 + 3:
                    {
                        goto default;
                        break;
                    }
                default:
                    {
                        return;
                    }
            }
            switch(shape)
            {
                case Circle c:
                    WriteLine($"circle with radius {c.Radius}");
                    break;
                case Rectangle s when (s.Length == s.Height):
                    WriteLine($"{s.Length} x {s.Height} square");
                    break;
                case null:
                    throw new ArgumentNullException(nameof(shape));
            }
            while (i < 10)
            {
                ++i;
                if (true) continue;
                break;
            }
            do
            {
                ++i;
                if (true) continue;
                break;
            }
            while (i < 10);
            for (int j = 0; j < 100; ++j)
            {
                for(;;)
                {
                    for (int i = 0, j = 0; i < length; i++, j++) { }
                    if (true) continue;
                    break;
                }
            }
            label:
            goto label;
            label2: ;
            foreach (var i in Items())
            {
                if (i == 7)
                    return;
                else
                    continue;
            }
            checked
            {
                checked(++i);
            }
            unchecked
            {
                unchecked(++i);
            }
            lock (sync)
                process();
            using (var v = BeginScope())
            using (A a = new A())
            using (A a = new A(), b = new A())
            using (BeginScope())
                return;
            yield return this.items[3];
            yield break;
            fixed (int* p = stackalloc int[100], q = &y)
            {
                *intref = 1;
            }
            fixed (int* p = stackalloc int[100])
            {
                *intref = 1;
            }
            unsafe
            {
                int* p = null;
            }
            try
            {
                throw null;
            }
            catch (System.AccessViolationException av)
            {
                throw av;
            }
            catch (Exception)
            {
                throw;
            }
            finally
            {
                try { } catch { }
            }
            var anonymous =
            {
                A = 1,
                B = 2,
                C = 3,
            };
            var query = from c in customers
                        let d = c
                        where d != null
                        join c1 in customers on c1.GetHashCode() equals c.GetHashCode()
                        join c1 in customers on c1.GetHashCode() equals c.GetHashCode() into e
                        group c by c.Country
                            into g
                            orderby g.Count() ascending
                            orderby g.Key descending
                            select new { Country = g.Key, CustCount = g.Count() };
            query = from c in customers
                    select c into d
                    select d;
        }
    }
}
