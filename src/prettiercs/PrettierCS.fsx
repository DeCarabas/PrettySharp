#r "netstandard"
#r @"System.Text.Encoding"
#r @"../../packages/Microsoft.CodeAnalysis.Common/lib/netstandard1.3/Microsoft.CodeAnalysis.dll"
#r @"../../packages/Microsoft.CodeAnalysis.CSharp/lib/netstandard1.3/Microsoft.CodeAnalysis.CSharp.dll"
#r @"../../bin/PrettierCS.dll"

open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open PrettierCS.Core

// let path = @"c:\src\onceandfuture\onceandfuture\syndication\feedparser.cs"
// let tree = CSharpSyntaxTree.ParseText (System.IO.File.ReadAllText path)
// let root = tree.GetRoot ()

// HMMMMMMMMM WHAT.
// There are hundreds of node types; I want to avoid writing handlers for every
// single node type. But that's how these things work, I think. Of course, I
// have the literal text, so if the general plan is to just stick groups in
// places where line breaks need to happen, then woohoo! I think the prettier
// design is probably right; with hard breaks and stuff like that.
// let fmt (x:SyntaxNode) =
//     match x.Kind() with
//     | SyntaxKind.None -> nil
//     | SyntaxKind.AbstractKeyword -> text "abstract"
//     | SyntaxKind.AccessorList -> nil
//     | SyntaxKind.AddAccessorDeclaration -> nil
//     | SyntaxKind.AddAssignmentExpression -> nil
//     | SyntaxKind.AddExpression -> nil
//     | SyntaxKind.AddKeyword -> nil
//     | SyntaxKind.AddressOfExpression -> nil
//     | SyntaxKind.AliasKeyword -> nil
//     | SyntaxKind.AliasQualifiedName -> nil
//     | SyntaxKind.AmpersandAmpersandToken -> nil
//     | SyntaxKind.AndAssignmentExpression -> nil

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
    | t::ts -> (showTree t) <+> text ", " <+> line <+> showTrees ts
    | [] -> nil

let testTree =
    Node(
        "aaa", [
            Node("bbb", [ Node ("ccc", []); Node ("dd", []) ]);
            Node("eee", []);
            Node("ffff", [ Node ("gg", []); Node ("hhh", []); Node ("ii", []) ])
        ]
    )

// PRINTING THEM
System.Console.WriteLine (pretty 40 (showTree testTree))