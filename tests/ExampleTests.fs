module PrettySharp.Tests.SnapshotTests

open System
open System.IO

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

open PrettySharp.Core
open PrettySharp.CS
open PrettySharp.Tests.Stuff

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

let testFile options fileName =
    let actual = (prettyFile fileName).Trim()
    let expected = loadSnapshot fileName

    let diff = diffStrings expected actual
    if hasDifferences diff
    then
        printf "F"
        Fail (fileName, actual, formatDiff diff)
    else
        printf "."
        Pass fileName

let testExamples options =
    getFiles AppDomain.CurrentDomain.BaseDirectory "*.cs"
    |> Seq.map (testFile options)
    |> Seq.toList