# PrettySharp

This is a C# pretty-printer ala clang-format and prettier.

[![Build Status](https://dev.azure.com/doty/PrettySharp/_apis/build/status/PrettySharp-CI?branchName=master)](https://dev.azure.com/doty/PrettySharp/_build/latest?definitionId=2&branchName=master)

## Installing

### Windows
If you have Visual Studio, make sure you have the C++ compiler installed.
Then:

- Clone this repository.

- Open up a command prompt (`cmd.exe`) and navigate to the directory you cloned this code into.

- Make sure you have the environment variables set up for Visual C++.
  This means you'll want to run one of these batch files, depending on what edition you have:

  ```
  REM If you have VS Enterprise
  "C:\Program Files (x86)\Microsoft Visual Studio\2017\Enterprise\VC\Auxiliary\Build\vcvarsall.bat" x64

  REM If you have VS Professional
  "C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\VC\Auxiliary\Build\vcvarsall.bat" x64

  REM If you have VS Community
  "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
  ```

- Run `build.bat` from this directory to build prettysharp.exe.

(If you have MinGW installed and on your path, then you can more or less follow the Unix instructions, below, except that you won't be able to `make install`.)

### MacOS X
Install the "command line developer tools," which are part of XCode.
Then you should be able to:

- Clone this repository

- Open a terminal and navigate to the folder you cloned into

- Run `make` to build the `prettysharp` binary.

- Run `sudo make install` to copy the binary to `/usr/local/bin`, which should be on your path.


### Unices
You'll need `make` and some kind of C compiler.
(There are many ways to get these; probably as many ways as there are unix variants.
On debian linux distributions, you'll want something like `sudo apt get build-essential`.
I don't know what you want on Redhat or any of the BSDs or anything else, I'm sorry.)

From this directory, run `make` to build the `prettysharp` binary.

Running `sudo make install` will copy it into `/usr/local/bin`.
If you don't want it there, feel free to copy it wherever you like.

## Running
To format a C# file, run:

```
prettysharp <path to C# file>
```

If run without arguments, it will read the C# from stdin.

If it can parse the file successfully, it will print formatted C# to stdout, and exit with error code 0.
If not, it will print an error indicating what went wrong to stderr, and exit with a non-zero error code.

## The Algorithm
It started out using Wadler's pretty-printer algorithm, but it was converted into an imperative form and tweaked a bit.
As a result, it is no longer strictly equivalent to the one described in "A Prettier Printer".

The main difference is that we don't have a tree of `union` operators.
Instead, we have explicitly delimited groups, marked with a "begin" and an "end".
When laying out text, we consider each group first in "flattened" mode first.
In "flattened" mode, line breaks are replaced with alternate text and nested groups are ignored.
If it doesn't fit in "flattened" mode then we revisit the group in normal mode, and treat everything normally.

This is equivalent in operation to the algorithm described by Wadler, but only if we constrain ourselves to the `group` constructor.
We do not have a general-purpose `union` operator.

However, the fact that we're a sequence of operations rather than a strict tree does give us more flexibility in composing `nest` with `group`, something we use to great effect to make sure some of our sub-expressions are not too indented.

We also pick up the "break parent" operation from [prettier](https://prettier.io), as it's pretty useful for dealing with lambdas and the like.

## The Implementation
It's in C, with a hand-written recursive descent parser.

Why?
For speed.

As a counter-example, you can see the first version, written in F#, in the 'managed' directory.
That one has a more faithful implementation of Wadler's algorithm.
It also uses the Roslyn C# parser.

Roslyn is nice because it keeps all the trivia, and of course it's the official C# compiler these days, so what it parses is 100% official.

The problem is that Roslyn is *s-l-o-w*.
Take a look at the "ILog.cs" file in the tests directory.
Believe it or not, Roslyn takes more than 300ms just to parse that file on my computer.
Even if my layout algorithm were free, it would still be *way* too slow for me to run it on every save in my editor.

As a comparison, the last time I timed the tests they took 156ms end-to-end.

That's 156ms to:
 - start the make program and parse the makefile
 - start the python interpreter and load the test script
 - find, load, parse, format, and diff all of the C# test files against their baselines.
