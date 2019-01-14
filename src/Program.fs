open Microsoft.CodeAnalysis.CSharp
open PrettySharp.Core
open PrettySharp.CS

[<EntryPoint>]
let main argv =
    match Array.toList argv with
    | path::_ ->
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        let tree = CSharpSyntaxTree.ParseText (System.IO.File.ReadAllText path)
        eprintfn "Parsed in %i ms" timer.ElapsedMilliseconds
        timer.Restart()
        let doc = visit (tree.GetRoot())
        eprintfn "Visited in %i ms" timer.ElapsedMilliseconds
        timer.Restart()
        let formatted = pretty 80 doc
        eprintfn "Formatted in %i ms" timer.ElapsedMilliseconds
        printfn "%s" formatted
        0
    | _ ->
        printfn "Unknown command line arguments!"
        -1
