#r "netstandard"
#r "System.Text.Encoding"
#r "../packages/Microsoft.CodeAnalysis.Common/lib/netstandard1.3/Microsoft.CodeAnalysis.dll"
#r "../packages/Microsoft.CodeAnalysis.CSharp/lib/netstandard1.3/Microsoft.CodeAnalysis.CSharp.dll"

open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

// TEST SETUP

let path = @"./tests/source/KateGame.cs.ignore"
let parseOpts = CSharpParseOptions.Default.WithKind(SourceCodeKind.Script)
// let tree = CSharpSyntaxTree.ParseText (System.IO.File.ReadAllText path)
let tree = CSharpSyntaxTree.ParseText ("
    var y = delegate (int x) { return x + 1; }
", parseOpts)
let root = tree.GetRoot()


#load "../src/Core.fs"

#load "../src/PrettierCS.fs"

open PrettySharp.Core
open PrettySharp.CS

let doc = visit root

printfn "\n%s" (pretty 80 doc)

printfn "\n%s" (pretty 20 doc)
