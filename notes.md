# Notes

These are just some random notes I took while fixing bugs in the C# parser.
(I had more thoughts during the initial work but failed to write them down.)

## Nullable Types

Holy moly, the spec doesn't talk about this but it's super complicated.
Things to think about, from reading the Roslyn parser:

- The syntax explicitly disallows '??' or '*?'.
  This is in the C# 6 spec.

- '?[' is always allowed.
  This is also in the C# 6 spec.

- Expressions like `new (int,int)?(t)` which constructs a new nullable tuple with an existing non-nullable tuple, and `new (int, int) ? x : y` which is a ternary expression, need to be considered.
  Tuples are part of C# 7.x, and so are not yet in a formal spec.

- There are special rules for parsing types in certain situations.
  If you're in an 'is' expression or an 'as' expression or in a pattern you can't parse a nullable if the next token can start an expression, so that the conditional operator parses correctly.
  (This is the bug in my parser that caused me to start this.)
  Even though this comes up in plain old C# 6, it is *not* in the spec as far as I can see.

- If you're in an object creation expression then the only things allowed after a `?` are `(` or `[` or `{`, otherwise the `?` isn't part of the type.
  I have no idea what affect this has on parsing, but the Roslyn enforces it.
