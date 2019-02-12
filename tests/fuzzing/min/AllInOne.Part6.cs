namespace Comments.XmlComments.UndocumentedKeywords
{
    /// <summary>
    /// Whatever
    /// </summary>
    /// <!-- c -->
    /// <![CDATA[c]]> //
    /// <c></c> /* */
    /// <code></code>
    /// <example></example>
    /// <exception cref="bla"></exception>
    /// <include file='' path='[@name=""]'/>
    /// <permission cref=" "></permission>
    /// <remarks></remarks>
    /// <see cref=""/>
    /// <seealso cref=" "/>
    /// <value></value>
    /// <typeparam name="T"></typeparam>
    class /*///*/C<T>
    {
        void M<U>(T t, U u)
        {
            // comment
            /* *** / */
            /* //
             */
            /*s*///comment
            // /***/
            /*s*/int /*s*/intValue = 0;
            intValue = intValue /*s*/+ 1;
            string strValue = /*s*/"hello";
            /*s*/MyClass c = new MyClass();
            string verbatimStr = /*s*/@"\\\\";
            int i = 0 // foo
                + 1 // bar
                + 2; // qux
            i = 3 // foo
                + 4 // bar
                + 5; // qux
            i = strValue != null ?
                6 : // bar
                7; // qux
        }
    }

    //General Test F. Type a very long class name, verify colorization happens correctly only upto the correct size (118324)
    class TestClassXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX/*Scen8*/{ }

    class TestClassXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX22/*Scen9*/{ }

    class yield
    {
        void Foo<U>(__arglist)
        {
            C<U> c = null;
            c.M<int>(5, default(U));
            TypedReference tr = __makeref(c);
            Type t = __reftype(tr);
            int j = __refvalue(tr, int);
            Params(a: t, b: t);
            Params(ref c, out c);
            Params(out var d, out Test d);
        }
        void Params(ref dynamic a, out dynamic b, params dynamic[] c) {}
        void Params(out dynamic a = 2, ref dynamic c = default(dynamic), params dynamic[][] c) {}

        public override string ToString() { return base.ToString(); }

        public partial void OnError();

        public partial void method()
        {
            int?[] a = new int?[5];/*[] bug*/ // YES []
            int[] var = { 1, 2, 3, 4, 5 };/*,;*/
            int i = a[i];/*[]*/
            Foo<T> f = new Foo<int>();/*<> ()*/
            f.method();/*().*/
            i = i + i - i * i / i % i & i | i ^ i;/*+ - * / % & | ^*/
            bool b = true & false | true ^ false;/*& | ^*/
            b = !b;/*!*/
            i = ~i;/*~i*/
            b = i < i && i > i;/*< && >*/
            int? ii = 5;/*? bug*/ // NO ?
            int f = true ? 1 : 0;/*? :*/   // YES :
            i++;/*++*/
            i--;/*--*/
            b = true && false || true;/*&& ||*/
            i << 5;/*<<*/
            i >> 5;/*>>*/
            b = i == i && i != i && i <= i && i >= i;/*= == && != <= >=*/
            i += 5.0;/*+=*/
            i -= i;/*-=*/
            i *= i;/**=*/
            i /= i;/*/=*/
            i %= i;/*%=*/
            i &= i;/*&=*/
            i |= i;/*|=*/
            i ^= i;/*^=*/
            i <<= i;/*<<=*/
            i >>= i;/*>>=*/
            object s = x => x + 1;/*=>*/
            double d = .3;
            ;
            Point point;
            unsafe
            {
                Point* p = &point;/** &*/
                p->x = 10;/*->*/
            }
            IO::BinaryReader br = null;
            x[i: 1] = 3;
            x[i: 1, j: 5] = "str";
        }

        struct Point { public int X; public int Y; public void ThisAccess() { this = this; } }
    }
}
