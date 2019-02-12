namespace Comments.XmlComments.UndocumentedKeywords
{
    // From here:https://github.com/dotnet/roslyn/wiki/New-Language-Features-in-C%23-6
    class CSharp6Features
    {
        // Initializers for auto-properties
        public string First { get; set; } = "Jane";
        public string Last { get; set; } = "Doe";

        // Getter-only auto-properties
        public string Third { get; } = "Jane";
        public string Fourth { get; } = "Doe";

        // Expression bodies on method-like members
        public Point Move(int dx, int dy) => new Point(x + dx, y + dy);
        public static Complex operator +(Complex a, Complex b) => a.Add(b);
        public static implicit operator string(Person p) => p.First + " " + p.Last;
        public void Print() => Console.WriteLine(First + " " + Last);

        // Expression bodies on property-like function members
        public string Name => First + " " + Last;
        public int this[long id] => id;

        private void NoOp()
        {
            // Empty body.
        }

        async void Test()
        {
            // Using static
            WriteLine(Sqrt(3*3 + 4*4));
            WriteLine(Friday - Monday);
            var range = Range(5, 17);                // Ok: not extension
            var even = range.Where(i => i % 2 == 0); // Ok

            // Null-conditional operators
            int? length = customers?.Length; // null if customers is null
            Customer first = customers?[0];  // null if customers is null
            int length = customers?.Length ?? 0; // 0 if customers is null
            int? first = customers?[0]?.Orders?.Count();
            PropertyChanged?.Invoke(this, args);

            // String interpolation
            string s = $"{p.Name, 20} is {p.Age:D3} year{{s}} old #";
            s = $"{p.Name} is \"{p.Age} year{(p.Age == 1 ? "" : "s")} old";
            s = $"{(p.Age == 2 ? $"{new Person { } }" : "")}";
            s = $@"\{p.Name}
                                   ""\";
            s = $"Color [ R={func(b: 3):#0.##}, G={G:#0.##}, B={B:#0.##}, A={A:#0.##} ]";

            // nameof expressions
            if (x == null)
                throw new ArgumentNullException(nameof(x));
            WriteLine(nameof(person.Address.ZipCode)); // prints "ZipCode"

            // Index initializers
            var numbers = new Dictionary<int, string> {
                [7] = "seven",
                [9] = "nine",
                [13] = "thirteen"
            };

            // Exception filters
            try {}
            catch (MyException e) when (myfilter(e))
            { }

            // Await in catch and finally blocks
            Resource res = null;
            try
            {
                res = await Resource.OpenAsync();       // You could do this.
            }
            catch(ResourceException e)
            {
                await Resource.LogAsync(res, e);         // Now you can do this …
            }
            finally
            {
                if (res != null)
                    await res.CloseAsync(); // … and this.
            }
        }
    }
}
#line 6
#line 2 "test.cs"
#line default
#line hidden
