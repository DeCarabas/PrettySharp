This is my previous implementation, using F# and Roslyn.

It has the advantages of (a) being closer to Wadler's algorithm, (b) being somewhat terser, and (c) using Roslyn to parse, which means that it's using the same C# parser/lexer as the official C# compiler.

The disadvantage is that the official parser is SLOW: 360ms to parse a modest file, which is WAY too slow for me.
(The C implementation takes 6ms to run end-to-end ont he same file, which is 60x faster than just parsing.)
