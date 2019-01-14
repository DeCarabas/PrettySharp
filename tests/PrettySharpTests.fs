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

        let failures = results |> Seq.filter (isFailure) |> Seq.length
        printf "%d failure%s\n"
            failures
            (if failures > 1 || failures = 0 then "s" else "")
        if failures > 0 then 1 else 0
