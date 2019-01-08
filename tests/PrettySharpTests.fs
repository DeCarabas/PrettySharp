module PrettySharp.Tests.Program

open PrettySharp.Tests.Stuff
open PrettySharp.Tests.SnapshotTests

let parseCommandLine args =
    let rec parseInternal opts args =
        match args with
        | [] -> opts
        | "-u"::xs -> parseInternal { opts with updateSnapshots = true } xs
        | x::xs ->
            eprintfn "Unrecognized option: '%s'" x
            { opts with hasCommandLineErrors = true }
    parseInternal defaultOptions (Array.toList args)


[<EntryPoint>]
let main argv =
    let options = parseCommandLine argv
    if options.hasCommandLineErrors
    then -1
    else
        let results = testExamples options

        printf "\n"
        results |> List.map (showResult options) |> ignore
        if Seq.exists (isFailure) results then 1 else 0