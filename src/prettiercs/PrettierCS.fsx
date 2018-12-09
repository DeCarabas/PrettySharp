open Microsoft.CodeAnalysis
open System
#r "netstandard"
#r @"System.Text.Encoding"
#r @"..\..\packages\Microsoft.CodeAnalysis.Common\lib\netstandard1.3\Microsoft.CodeAnalysis.dll"
#r @"..\..\packages\Microsoft.CodeAnalysis.CSharp\lib\netstandard1.3\Microsoft.CodeAnalysis.CSharp.dll"

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

// let text = System.IO.File.ReadAllText(@"c:\src\onceandfuture\onceandfuture\syndication\feedparser.cs")
// let tree = CSharpSyntaxTree.ParseText text
// let root = tree.GetRoot ()

// type TreeNode = Token of string | Node of seq<TreeNode>

// let rec dumpNodeOrToken (n:SyntaxNodeOrToken) =
//     if n.IsToken
//     then Token (n.AsToken().Text)
//     else Node (Seq.map dumpNodeOrToken (n.ChildNodesAndTokens()))

// Node (Seq.map dumpNodeOrToken (root.ChildNodesAndTokens()))

type Doc = Nil
         | Text of string*Doc
         | Line of int*Doc

let nil = Nil
let text str = Text (str,nil)
let line = Line (0,nil)
let rec concat x y =
    match x with
    | Text (s,xi) -> Text (s, concat xi y)
    | Line (i,xi) -> Line (i, concat xi y)
    | Nil -> y

let rec concatAll xs = Seq.reduce (concat) xs

let rec nest i x =
    match x with
    | Text (s,xi) -> Text (s, nest i xi)
    | Line (j,xi) -> Line (i+j, nest i xi)
    | Nil -> Nil

let rec layout x =
    match x with
    | Text (s,xi) -> s + layout xi
    | Line (i,xi) -> "\n" + (new string(' ', i)) + layout xi
    | Nil -> ""

// Dumb Trees
type Tree = Node of string*Tree list

let rec showTree t =
    match t with
    | Node (s,ts) -> concat (text s) (nest s.Length (showBracket ts))
and showBracket ts =
    match ts with
    | [] -> nil
    | ts -> concatAll [text "["; nest 1 (showTrees ts); text "]"]
and showTrees ts =
    match ts with
    | [t] -> showTree t
    | t::ts -> concatAll [showTree t; text ","; line; showTrees ts]
    | [] -> nil

// PRINTING THEM
System.Console.WriteLine(
    layout <|
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