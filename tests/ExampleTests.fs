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

let parseText (text:string) =
    let parseOpts = CSharpParseOptions.Default.WithKind(SourceCodeKind.Script)
    CSharpSyntaxTree.ParseText (text, parseOpts)

let parseFile fileName =
    File.ReadAllText fileName |> parseText


let prettyFile fileName =
    let root = (parseFile fileName).GetRoot()
    root |> visit |> pretty 80

let snapFile fileName = fileName + ".expected"

let loadSnapshot fileName =
    let snapfile = snapFile fileName
    if File.Exists snapfile
    then (File.ReadAllText snapfile).Trim()
    else ""

let normalize (tree:SyntaxTree) =
    let newRoot = tree.GetRoot().NormalizeWhitespace("  ", true)
    tree.WithRootAndOptions(newRoot, tree.Options)

let testFile options fileName =
    let actual = (prettyFile fileName).Trim()
    let expected = loadSnapshot fileName

    let diff = diffStrings expected actual
    if hasDifferences diff
    then
        printf "F"
        Fail (fileName, actual, formatDiff diff)
    else
        let normActual = parseText actual |> normalize
        let normExpected = parseFile fileName |> normalize
        if normActual.IsEquivalentTo(normExpected)
        then
            printf "."
            Pass fileName
        else
            printf "F"
            Fail (fileName, actual, "Something was lost in translation!")

let testExamples options =
    getFiles AppDomain.CurrentDomain.BaseDirectory "*.cs"
    |> Seq.map (testFile options)
    |> Seq.toList
