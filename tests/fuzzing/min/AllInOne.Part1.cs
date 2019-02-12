#error Error message
#warning Warning message
#pragma warning disable 414, 3021
#pragma warning restore 3021
#pragma checksum "file.txt" "{00000000-0000-0000-0000-000000000000}" "2453"
#define foo // Comment in directive
#if foo
#else
#endif
#undef foo

extern alias Foo;

using System;
using System.Collections.Generic;
using System.Linq;

using ConsoleApplication2.Test;
using System.Linq.Expressions;
using System.Text;
using M = System.Math;

#if DEBUG || TRACE
using System.Diagnostics;
#elif SILVERLIGHT && WINDOWS_PHONE || DEBUG || foo == true || foo != false
using System.Diagnostics;
#else
using System.Diagnostics;
#endif

#region Region

    #region more
using ConsoleApplication2.Test;
    #endregion
using X = int1;
using Y = ABC.X<int>;

using static System.Math;
using static System.DayOfWeek;
using static System.Linq.Enumerable;

#endregion

[assembly: System.Copyright(@"(C)""

2009")]
[module: System.Copyright("\n\t\u0123(C) \"2009" - ("\u0123" + "foo"))]

class TopLevelType : IDisposable, Foo
{
    void IDisposable.Dispose(int x, int y) { }
}
