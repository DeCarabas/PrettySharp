#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.DotNet.MSBuild
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators

let buildDir = "./bin/"

Target.create "Clean" (fun _ ->
    Shell.cleanDir buildDir
)

Target.create "BuildApp" (fun _ ->
    !! "src/**/*.fsproj"
        |> MSBuild.runRelease id buildDir "Build"
        |> Trace.logItems "AppBuild-Output: "
)

Target.create "Default" (fun _ ->
    Trace.trace "Hello World from FAKE"
)

open Fake.Core.TargetOperators

"Clean"
    ==> "BuildApp"
    ==> "Default"

Target.runOrDefault "Default"