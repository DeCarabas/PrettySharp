#r "netstandard"

#load "../Core.fs"
open PrettySharp.Core

type TestResult =
    | Pass
    | Fail of string

let be expected actual =
    if (expected = actual)
    then Pass
    else Fail (
            sprintf "%A does not equal %A" actual expected
        )

let should expectation value =
    expectation value


let simple = (pretty 100 (text "foo")) |> should be "foo"

let ``group flat`` =
    (pretty 100 (group (text "foo" <+> line <+> text "bar")))
        |> should be "foo bar"
let ``group fold`` =
    (pretty 3 (group (text "foo" <+> line <+> text "bar")))
        |> should be "foo\nbar"

let tests = [simple, ``group flat``, ``group fold``]