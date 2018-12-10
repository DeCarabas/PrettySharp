module PrettierCS
open System.Xml.Serialization

type Doc = Nil
         | Text of string*Doc
         | Line of int*Doc
         | Union of Doc*Doc

let nil = Nil
let text str = Text (str,nil)
let line = Line (0,nil)
let rec concat x y =
    match x with
    | Text (s,xi) -> Text (s, concat xi y)
    | Line (i,xi) -> Line (i, concat xi y)
    | Nil -> y
    | Union(xi, yi) -> Union ((concat xi y), (concat yi y))

let rec concatAll xs = Seq.reduce (concat) xs

let rec nest i x =
    match x with
    | Text (s,xi) -> Text (s, nest i xi)
    | Line (j,xi) -> Line (i+j, nest i xi)
    | Nil -> Nil
    | Union (xi, yi) -> Union((nest i xi), (nest i yi))

let rec group x = 
    match x with
    | Nil -> Nil
    | Line (i,xi) -> Union ((Text (" ", flatten xi)), x)
    | Text (s,xi) -> Text (s, group xi)
    | Union (xi, yi) -> Union((group xi), yi)
and flatten x = 
    match x with
    | Nil -> Nil
    | Line (i,xi) -> Text (" ", flatten xi)
    | Text (s, xi) -> Text (s, flatten xi)
    | Union (xi, yi) -> flatten xi

let rec fits w x =
    if w < 0 then false 
    else match x with
         | Nil -> true
         | Text (s, xi) -> fits (w - s.Length) xi
         | Line (i, xi) -> true
         | Union (_) -> failwith "Not Implemented"

let better w k x y = if fits (w-k) x then x else y

let rec best w k x =
    match x with
    | Nil -> Nil
    | Line (i,xi) -> Line (i, best w i xi)
    | Text (s, xi) -> Text (s, best w (k + s.Length) xi)
    | Union (xi, yi) -> better w k (best w k xi) (best w k yi)

let rec layout x =
    match x with
    | Text (s,xi) -> s + layout xi
    | Line (i,xi) -> "\n" + (new string(' ', i)) + layout xi
    | Nil -> ""
    | Union (_) -> failwith "Not Implemented"

let pretty w x = layout (best w 0 x)

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
