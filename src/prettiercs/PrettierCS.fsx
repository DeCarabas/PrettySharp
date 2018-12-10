#r "netstandard"
#r @"System.Text.Encoding"
#r @"../../packages/Microsoft.CodeAnalysis.Common/lib/netstandard1.3/Microsoft.CodeAnalysis.dll"
#r @"../../packages/Microsoft.CodeAnalysis.CSharp/lib/netstandard1.3/Microsoft.CodeAnalysis.CSharp.dll"
#r @"./bin/Debug/netcoreapp2.2/PrettierCS.dll"

open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open PrettierCS

// let text = System.IO.File.ReadAllText(@"c:\src\onceandfuture\onceandfuture\syndication\feedparser.cs")
// let tree = CSharpSyntaxTree.ParseText text
// let root = tree.GetRoot ()

// type TreeNode = Token of string | Node of seq<TreeNode>

// let rec dumpNodeOrToken (n:SyntaxNodeOrToken) =
//     if n.IsToken
//     then Token (n.AsToken().Text)
//     else Node (Seq.map dumpNodeOrToken (n.ChildNodesAndTokens()))

// Node (Seq.map dumpNodeOrToken (root.ChildNodesAndTokens()))


// Dumb Trees and Formatting Them
type Tree = Node of string*Tree list

let rec showTree t =
    match t with
    | Node (s,ts) -> group  ((text s) <+> (nest s.Length (showBracket ts)))
and showBracket ts =
    match ts with
    | [] -> nil
    | ts -> text "[" <+> nest 1 (showTrees ts) <+> text "]"
and showTrees ts =
    match ts with
    | [t] -> showTree t
    | t::ts -> (showTree t) <+> text "," <+> line <+> showTrees ts
    | [] -> nil

// PRINTING THEM
System.Console.WriteLine(
    pretty 20 <|
    showTree (
        Node ("aaa", [
            Node ("bbb", [
                Node ("ccc", []);
                Node ("dd", []);
            ]);
            Node ("eee", []);
            Node ("ffff", [
                Node ("gg", []);
                Node ("hhh", []);
                Node ("ii", []);
            ])])))