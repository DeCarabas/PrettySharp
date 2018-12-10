module PrettierCS
open System.Xml.Serialization

type DOC = NIL
         | CONCAT of DOC*DOC
         | NEST of int*DOC
         | TEXT of string
         | LINE
         | UNION of DOC*DOC

type Doc = Nil
         | Text of string*Doc
         | Line of int*Doc

let nil = NIL
let (<+>) x y  = CONCAT (x,y)
let nest i x = NEST (i,x)
let text str = TEXT str
let line = LINE

let rec group x = flatten <| UNION (x,x)
and flatten x =
    match x with
    | NIL -> NIL
    | CONCAT (xi, yi) -> CONCAT ((flatten xi), (flatten yi))
    | NEST (_, xi) -> flatten xi
    | TEXT str -> TEXT str
    | LINE -> NIL
    | UNION (xi, _) -> flatten xi

let rec fits w x =
    if w < 0 then false 
    else match x with
         | Nil -> true
         | Text (s, xi) -> fits (w - s.Length) xi
         | Line (i, xi) -> true

let better w k x y = if fits (w-k) x then x else y

let rec be w k x =
    match x with
    | [] -> Nil
    | (i, NIL)::z -> be w k z
    | (i, CONCAT(xi, yi))::z -> be w k ((i,xi)::(i,yi)::z)
    | (i, NEST (j, xi))::z -> be w k ((i + j, xi)::z)
    | (i, TEXT s)::z -> Text (s, (be w (k+s.Length) z))
    | (i, LINE)::z -> Line (i, (be w i z))
    | (i, UNION(xi, yi))::z -> better w k (be w k ((i, xi)::z))
                                          (be w k ((i, yi)::z))

let best w k x = be w k [(0,x)]

let rec layout x =
    match x with
    | Text (s,xi) -> s + layout xi
    | Line (i,xi) -> "\n" + (new string(' ', i)) + layout xi
    | Nil -> ""

let pretty w x = layout (best w 0 x)

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
