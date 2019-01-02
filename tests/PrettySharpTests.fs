open System
open System.IO

open Microsoft.CodeAnalysis.CSharp

open PrettySharp.Core
open PrettySharp.CS
open DiffPlex.DiffBuilder
open DiffPlex.DiffBuilder.Model

let rec getFiles dir searchPattern =
    seq {
        yield! Directory.GetFiles(dir, searchPattern)
        for subdir in Directory.GetDirectories(dir) do
            yield! getFiles subdir searchPattern
    }

let prettyFile fileName =
    CSharpSyntaxTree.ParseText (File.ReadAllText fileName)
        |> visit
        |> pretty 80

let loadSnapshot fileName =
    let snapfile = fileName + ".expected"
    printfn "-> %s" snapfile
    if File.Exists snapfile
    then (File.ReadAllText snapfile).Trim()
    else ""

type TestResult =
    | Pass of string
    | Fail of string * string

let showResult result =
    match result with
    | Pass _ -> ()
    | Fail (fileName, diff) -> printfn "%s failed:\n%s" fileName diff

let diffStrings left right =
    (new InlineDiffBuilder(new DiffPlex.Differ())).BuildDiffModel(left, right)

let hasDifferences (diff:DiffPaneModel) =
    diff.Lines |> Seq.exists (fun line -> line.Type <> ChangeType.Unchanged)

let testFile fileName =
    let actual = (prettyFile fileName).Trim()
    let expected = loadSnapshot fileName

    let diff = diffStrings expected actual
    if hasDifferences diff
    then
        let formatLine (line:DiffPiece) =
            match line.Type with
            | ChangeType.Inserted -> " + " + line.Text
            | ChangeType.Deleted -> " - " + line.Text
            | _ -> "    " + line.Text
        let formattedDiff =
            diff.Lines
            |> Seq.map formatLine
            |> String.concat "\n"
        Fail (fileName, formattedDiff)
    else Pass fileName

[<EntryPoint>]
let main argv =
    getFiles AppDomain.CurrentDomain.BaseDirectory "*.cs"
    |> Seq.map (testFile)
    |> Seq.map (showResult)
    |> Seq.toList
    |> ignore
    0