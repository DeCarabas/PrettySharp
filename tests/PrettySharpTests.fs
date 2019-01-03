module PrettySharp.Tests

open System
open System.IO

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

open PrettySharp.Core
open PrettySharp.CS
open DiffPlex.Model

type Options = {
    hasCommandLineErrors:bool;
    updateSnapshots:bool;
}
let defaultOptions = {
    hasCommandLineErrors = false;
    updateSnapshots = false;
}

let rec getFiles dir searchPattern =
    seq {
        yield! Directory.GetFiles(dir, searchPattern)
        for subdir in Directory.GetDirectories(dir) do
            yield! getFiles subdir searchPattern
    }

let prettyFile fileName =
    let parseOpts = CSharpParseOptions.Default.WithKind(SourceCodeKind.Script)
    let fileText = File.ReadAllText fileName
    (CSharpSyntaxTree.ParseText (fileText, parseOpts)).GetRoot()
        |> visit
        |> pretty 80

let snapFile fileName = fileName + ".expected"

let loadSnapshot fileName =
    let snapfile = snapFile fileName
    if File.Exists snapfile
    then (File.ReadAllText snapfile).Trim()
    else ""

let writeSnapshot actual fileName =
    File.WriteAllText ((snapFile fileName), actual)


type TestResult =
    | Pass of fname:string
    | Fail of fname:string * actual:string * diff:string

let isFailure = function
    | Pass _ -> false
    | Fail _ -> true

let showResult (options:Options) result =
    match result with
    | Pass _ -> ()
    | Fail (fileName, actual, diff) ->
        printfn "%s failed:\n%s\n" fileName diff
        if options.updateSnapshots
        then
            printfn "Do you want to update the snapshot? [y/N]"
            match Console.ReadLine().ToLower() with
            | "yes" | "y" ->
                printfn "Updating snapshot..."
                writeSnapshot actual fileName
            | _ -> printfn "Not updating snapshot."


type DiffLine =
    | Unchanged of string
    | Inserted of string
    | Deleted of string

let diffStrings left right =
    let diff = (new DiffPlex.Differ()).CreateLineDiffs(left, right, false)
    let rec handleBlock start (block:DiffBlock) =
        let unchanged =
            seq { start .. block.InsertStartB - 1 }
            |> Seq.map (fun i -> diff.PiecesNew.[i])
            |> Seq.map (Unchanged)
        let deleted =
            seq { 0 .. block.DeleteCountA - 1 }
            |> Seq.map (fun i -> diff.PiecesOld.[i + block.DeleteStartA])
            |> Seq.map (Deleted)
        let inserted =
            seq { 0 .. block.InsertCountB - 1 }
            |> Seq.map (fun i -> diff.PiecesNew.[i + block.InsertStartB])
            |> Seq.map (Inserted)

        Seq.concat [unchanged; deleted; inserted]

    let rec handleDiff pos (blocks:DiffBlock list) =
        match blocks with
        | [] ->
            seq { pos .. diff.PiecesNew.Length - 1 }
            |> Seq.map (fun i -> diff.PiecesNew.[i])
            |> Seq.map (Unchanged)
        | block::z ->
            let endpos = block.InsertStartB + block.InsertCountB
            Seq.append (handleBlock pos block) (handleDiff endpos z)

    handleDiff 0 (Seq.toList diff.DiffBlocks) |> Seq.toList

let rec hasDifferences diff =
    match diff with
    | [] -> false
    | (Unchanged _)::z -> hasDifferences z
    | (Inserted _)::_ -> true
    | (Deleted _)::_ -> true

let testFile options fileName =
    let actual = (prettyFile fileName).Trim()
    let expected = loadSnapshot fileName

    let diff = diffStrings expected actual
    if hasDifferences diff
    then
        let formatLine line =
            match line with
            | Inserted (s) -> "  + " + s
            | Deleted (s) -> "  - " + s
            | Unchanged (s) -> "    " + s
        let formattedDiff =
            diff
            |> List.map formatLine
            |> String.concat "\n"
        printf "F"
        Fail (fileName, actual, formattedDiff)
    else
        printf "."
        Pass fileName

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
        let results =
            getFiles AppDomain.CurrentDomain.BaseDirectory "*.cs"
            |> Seq.map (testFile options)
            |> Seq.toList

        printf "\n"
        results |> List.map (showResult options) |> ignore
        if Seq.exists (isFailure) results then 1 else 0