module PrettySharp.Tests.Stuff

open DiffPlex.Model

type Options = {
    hasCommandLineErrors:bool;
    updateSnapshots:bool;
}
let defaultOptions = {
    hasCommandLineErrors = false;
    updateSnapshots = false;
}

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

let formatDiff difflines =
    let formatLine line =
        match line with
        | Inserted (s) -> "  + " + s
        | Deleted (s) -> "  - " + s
        | Unchanged (s) -> "    " + s

    difflines |> List.map formatLine |> String.concat "\n"