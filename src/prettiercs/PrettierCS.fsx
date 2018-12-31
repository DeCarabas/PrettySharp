#r "netstandard"
#r "System.Text.Encoding"
#r "../../packages/Microsoft.CodeAnalysis.Common/lib/netstandard1.3/Microsoft.CodeAnalysis.dll"
#r "../../packages/Microsoft.CodeAnalysis.CSharp/lib/netstandard1.3/Microsoft.CodeAnalysis.CSharp.dll"

open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

// TEST SETUP
let path = @"c:\src\onceandfuture\onceandfuture\syndication\feedparser.cs"
let tree = CSharpSyntaxTree.ParseText (System.IO.File.ReadAllText path)

#load "./Core.fs"

#load "./PrettierCS.fs"

open PrettySharp.Core
open PrettySharp.CS

let doc = visit tree
Console.WriteLine(pretty 500 doc)
