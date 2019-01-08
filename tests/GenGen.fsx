open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp.Syntax
#r "netstandard"
#r "System.Text.Encoding"
#r "System.Xml.Linq"
#r "../packages/Microsoft.CodeAnalysis.Common/lib/netstandard1.3/Microsoft.CodeAnalysis.dll"
#r "../packages/Microsoft.CodeAnalysis.CSharp/lib/netstandard1.3/Microsoft.CodeAnalysis.CSharp.dll"

open System
open System.Collections.Generic
open System.Reflection
open System.Xml.Linq
open Microsoft.CodeAnalysis.CSharp


type XNames() =
    static member Name = XName.Get("Name")
    static member Type = XName.Get("Type")
    static member Kind = XName.Get("Kind")
    static member Field = XName.Get("Field")
    static member Node = XName.Get("Node")

let syntaxFieldsToTokenKinds (syntaxNodeElement:XElement) =
    let isTokenField (f:XElement) =
        f.Attribute(XNames.Type).Value = "SyntaxToken"

    let fieldTokenKinds (f:XElement) =
        f.Elements(XNames.Kind)
        |> Seq.map (fun k -> k.Attribute(XNames.Name).Value)
        |> Seq.toArray

    let fieldName (f:XElement) = f.Attribute(XNames.Name).Value

    syntaxNodeElement.Elements(XNames.Field)
    |> Seq.filter (isTokenField)
    |> Seq.map (fun f -> (fieldName f, fieldTokenKinds f))
    |> Map.ofSeq

let syntaxNodesToTokenFieldKinds (url:string) =
    let syntaxNodeName (e:XElement) = e.Attribute(XNames.Name).Value

    XDocument.Load(url).Root.Elements(XNames.Node)
    |> Seq.map(fun e -> (syntaxNodeName e, syntaxFieldsToTokenKinds e))
    |> Map.ofSeq

let syntaxMap =
    let url = "https://raw.githubusercontent.com/dotnet/roslyn/master/src/Compilers/CSharp/Portable/Syntax/Syntax.xml"
    syntaxNodesToTokenFieldKinds url


let isNodeFactoryMethod (m:MethodInfo) =
    typeof<CSharpSyntaxNode>.IsAssignableFrom(m.ReturnType) &&
    not (m.Name.StartsWith("get_")) &&
    not (m.Name.StartsWith("Get"))

let factoryType = typeof<SyntaxFactory>
let methods = factoryType.GetMethods(BindingFlags.Static ||| BindingFlags.Public)

let isTypeIEnumerable (t:Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<IEnumerable<_>>

let hasEnumerableParameter (m:MethodInfo) =
    m.GetParameters()
    |> Array.exists (fun p -> p.ParameterType |> isTypeIEnumerable)

let isTypeTrivia (t:Type) = typeof<StructuredTriviaSyntax>.IsAssignableFrom(t)
let isReturnTypeTrivia (m:MethodInfo) = m.ReturnType |> isTypeTrivia

let safeName (p:ParameterInfo) = if p.Name ="type" then "type_" else p.Name

let genParameter (p:ParameterInfo) =
    let pval =
        if p.ParameterType = typeof<SyntaxToken>
        then "??"
        else "Arb.generate<" + p.ParameterType.Name + ">"
    "let " + (safeName p) + " = " + pval

let makeInvocation (m:MethodInfo) =
    let pvals =
        let ps = m.GetParameters() |> Array.map (genParameter)
        if Array.isEmpty ps
        then ""
        else (Array.reduce (fun l r -> l + "\n" + r) ps) + "\n"
    let pnames =
        let names = m.GetParameters() |> Array.map (safeName)
        if Array.isEmpty names
        then ""
        else Array.reduce (fun l r -> l + ", " + r) names
    let pnamespace =
        let names = m.GetParameters() |> Array.map (safeName)
        if Array.isEmpty names
        then ""
        else Array.reduce (fun l r -> l + " " + r) names
    let gen =
        if m.GetParameters().Length = 0
        then "Gen.map (fun _ -> SyntaxFactory." + m.Name + "()) ()"
        else "Gen.map (fun " + pnamespace + " -> SyntaxFactory." + m.Name + "(" + pnames + ")) " + pnamespace

    pvals + gen

let concrete =
    methods
    |> Array.filter (isNodeFactoryMethod)
    |> Array.filter (hasEnumerableParameter >> not)
    |> Array.filter (isReturnTypeTrivia >> not)
    |> Array.map (makeInvocation)

concrete.[3]