#r "netstandard"
#r "System.Text.Encoding"
#load "./Core.fs"

open PrettierCS.Core
open System


// Dumb Trees and Formatting Them
type Tree = Node of string * Tree list

let rec showTree t =
    match t with
    | Node(s, ts) -> group ((text s) <+> (nest s.Length (showBracket ts)))

and showBracket ts =
    match ts with
    | [] -> nil
    | ts -> text "[" <+> nest 1 (showTrees ts) <+> text "]"

and showTrees ts =
    match ts with
    | [ t ] -> showTree t
    | t :: ts -> (showTree t) <+> text ", " <+> line <+> showTrees ts
    | [] -> nil

let testTree =
    Node("aaa",
         [ Node("bbb",
                [ Node("ccc", [])
                  Node("dd", []) ])
           Node("eee", [])
           Node("ffff",
                [ Node("gg", [])
                  Node("hhh", [])
                  Node("ii", []) ]) ])

// PRINTING THEM
System.Console.WriteLine(pretty 40 (showTree testTree))
