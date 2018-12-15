#r "netstandard"

#load "../Core.fs"
open PrettierCS.Core

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

5 |> should be 7