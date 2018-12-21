#r "netstandard"
#r "System.Text.Encoding"
#r "../../packages/FSharp.Compiler.Service/lib/netstandard2.0/FSharp.Compiler.Service.dll"

open Microsoft.FSharp.Compiler.SourceCodeServices

let checker = FSharpChecker.Create()
let fname = "./src/prettiercs/Core.fs"
let fsource = System.IO.File.ReadAllText(fname)

let parseOptions =
    checker.GetProjectOptionsFromScript(fname, fsource, System.DateTime.Now)
    |> (Async.RunSynchronously
        >> fst
        >> checker.GetParsingOptionsFromProjectOptions
        >> fst)

let parsed =
    checker.ParseFile(fname, fsource, parseOptions) |> Async.RunSynchronously
