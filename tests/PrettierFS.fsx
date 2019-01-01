#r "netstandard"
#r "System.Text.Encoding"
#r "../packages/FSharp.Compiler.Service/lib/netstandard2.0/FSharp.Compiler.Service.dll"
#load "./Core.fs"

open PrettySharp.Core
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

let checker = FSharpChecker.Create()

/// Get untyped tree for a specified input
let getUntypedTree (file, input) =
    // Get compiler options for the 'project' implied by a single script file
    let projectOptions, _errors =
        checker.GetProjectOptionsFromScript(file, input)
        |> Async.RunSynchronously
    let parsingOptions, _errors =
        checker.GetParsingOptionsFromProjectOptions(projectOptions)
    // Run the first phase (untyped parsing) of the compiler
    let parseFileResults =
        checker.ParseFile(file, input, parsingOptions) |> Async.RunSynchronously
    if parseFileResults.ParseHadErrors
    then failwith "Something went wrong during parsing!"
    else match parseFileResults.ParseTree with
         | Some tree -> tree
         | None -> failwith "Something went wrong during parsing!"


// Standard indentation for our F# formatter. (Any color you want, so long
// as...)
let INDENT = 4
let indent = nest INDENT
let ( <+/+> ) x y = x <+> line <+> y
let ( <+/!+> ) x y = x <+> indent (line <+> y)

let visitLongIdent il =
    il
    |> List.map (fun (i:Ident) -> text i.idText)
    |> List.reduce (fun l r -> l <+> text "." <+> softline <+> r)


let rec visitConst c =
    text "??CONST??"

let rec visitType typ =
    text "??TYPE??"

let rec visitBinding bind =
    text "??SYNBINDING??"

let rec visitInterfaceImpl impl =
    text "??INTERFACEIMPL??"

let rec visitPattern pat =
    match pat with
    | SynPat.Const(c,_) -> visitConst c
    | SynPat.Wild(_) -> text "_"
    | SynPat.Named(pat, name, _, _, _) ->
        group (
            visitPattern pat <+> indent (
                line <+> text "as " <+> text name.idText
            )
        )
    | SynPat.Typed(pat, typ, _) ->
        group (
            visitPattern pat <+> indent (
                softline <+> text ":" <+> visitType typ
            )
        )
    | SynPat.Attrib(_,_,_) -> failwith "TODO: What is Attrib?"
    | SynPat.Or(pl, pr, _) ->
        group (visitPattern pl <+> text " |" <+> line <+> visitPattern pr)
    | SynPat.Ands(pl,_) ->
        List.map (visitPattern) pl
        |> List.reduce (fun l r -> l <+> text " &" <+> line <+> r)
        |> group
    | SynPat.LongIdent(LongIdentWithDots(il, _), _, _, _, _, _) ->
        visitLongIdent il
    | SynPat.Tuple(pl,_) ->
        List.map (visitPattern) pl
        |> List.reduce (fun l r -> l <+> text "," <+> line <+> r)
        |> group
    | SynPat.StructTuple(pl, _) ->
        List.map (visitPattern) pl
        |> List.reduce (fun l r -> l <+> text "," <+> line <+> r)
        |> bracket INDENT "(" ")"
    | SynPat.Paren(p, _) -> visitPattern p |> bracket INDENT "(" ")"
    | SynPat.ArrayOrList(isList, pl, _) ->
        let left = if isList then "[" else "[|"
        let right = if isList then "]" else "|]"
        List.map (visitPattern) pl
        |> List.reduce (fun l r -> l <+> text "," <+> line <+> r)
        |> bracket INDENT left right
    | SynPat.Record(m, _) ->
        m
        |> List.map (fun ((li, _), pat) ->
            group (
                visitLongIdent li <+>
                text "=" <+>
                softline <+>
                visitPattern pat))
        |> List.reduce (fun l r -> l <+> text ";" <+> line <+> r)
        |> bracket INDENT "{" "}"
    | SynPat.Null(_) -> text "null"
    | SynPat.OptionalVal(i, _) -> text ("?" + i.idText)
    | SynPat.IsInst(t, _) -> group (text ":?" <+>  indent (line <+> visitType t))
    | SynPat.QuoteExpr(e, _) -> visitExpression e |> bracket INDENT "<@" "@>"
    | SynPat.DeprecatedCharRange(c1, c2, _) -> failwith "TODO: What is this?"
    | SynPat.InstanceMember(_) -> failwith "Unsupported: InstanceMember"
    | SynPat.FromParseError(_) -> failwith "Failing on parse error."

/// Walk over an expression - if expression contains two or three
/// sub-expressions (two if the 'else' branch is missing), let expression
/// contains pattern and two sub-expressions
and visitExpression expr =
    match expr with
    | SynExpr.Paren(e, _, _, _) -> visitExpression e |> bracket INDENT "(" ")"
    | SynExpr.Quote(operator, isRaw, quotedSynExpr, isFromQueryExpression, _) ->
        failwith "TODO: What is this?"
    | SynExpr.Const(constant, _) -> visitConst constant
    | SynExpr.Typed(expr, typ, _) ->
        group (
            visitExpression expr <+> indent (
                softline <+> text ":" <+> visitType typ
            )
        )
    | SynExpr.Tuple(exprs, _, _) ->
        group (
            exprs
            |> List.map (visitExpression)
            |> List.reduce (fun l r -> l <+> text "," <+> line <+> r)
        )
    | SynExpr.StructTuple(exprs, _, _) ->
        bracket INDENT "(" ")" (
            exprs
            |> List.map (visitExpression)
            |> List.reduce (fun l r -> l <+> text "," <+> line <+> r)
        )
    | SynExpr.ArrayOrList(isList, exprs, _) ->
        let left = if isList then "[" else "[|"
        let right = if isList then "[" else "[|"
        bracket INDENT left right (
            List.map (visitExpression) exprs
            |> List.reduce (fun l r -> l <+> text "," <+> line <+> r)
        )
    | SynExpr.Record(baseInfo, copyInfo, fields, _) ->
        let fieldPart =
            fields
            |> List.map (fun ((LongIdentWithDots(li, _), _), optExpr, _) ->
                group (
                    visitLongIdent li <+>
                    match optExpr with
                    | Some expr ->
                        text " =" <+> indent (line <+> visitExpression expr)
                    | None -> nil
                ))
            |> List.reduce (fun l r -> l <+> text ";" <+> line <+> r)
        let copyPart =
            match copyInfo with
            | Some (copyExpr, _) ->
                group (
                    visitExpression copyExpr <+> line <+> text "with"
                ) <+> line
            | None -> nil
        let basePart =
            match baseInfo with
            | Some (_) -> failwith "TODO: Base what now?"
            | None -> nil
        bracket INDENT "{" "}" (basePart <+> copyPart <+> fieldPart)
    | SynExpr.New(_, typ, exp, _) ->
        group (
            text "new" <+> indent (
                line <+> visitType typ <+> bracket INDENT "(" ")" (
                    visitExpression exp
                )
            )
        )
    | SynExpr.ObjExpr(objType, argOptions, bindings, extraImpls, _, _) ->
        let opts =
            match argOptions with
            | Some (_) -> failwith "TODO: What is argOptions?"
            | None -> nil
        let binds =
            bindings
            |> List.map (visitBinding)
            |> List.reduce (<+/+>)
        let extras =
            extraImpls
            |> List.map (visitInterfaceImpl)
            |> List.reduce (<+/+>)
        group (
            text "new" <+/!+> visitType objType <+/+>
            text "with" <+/!+> (opts <+> binds <+> extras)
        )
    | SynExpr.While(_, whileExpr, doExpr, _) ->
        group(
            group (text "while" <+/!+> visitExpression whileExpr) <+/+>
            group (text "do" <+/!+> visitExpression doExpr)
        )
    | SynExpr.For(_, ident, identBody, b, toBody, doBody, _) ->
        group (
            group (
                text "for" <+/!+>
                text ident.idText <+/+>
                text "=" <+/+>
                visitExpression identBody <+/+>
                text "to" <+/+>
                visitExpression toBody
            ) <+/+>
            group (text "do" <+/!+> visitExpression doBody)
        )
    | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
        group (
            group (text "if" <+/!+> visitExpression cond) <+/+>
            group (text "then" <+/!+> visitExpression trueBranch) <+>
            match falseBranchOpt with
            | Some(falseBranch) ->
                line <+> group (text "else" <+/!+> visitExpression falseBranch)
            | None -> nil
        )
    | SynExpr.Ident(id) -> text id.idText
    | SynExpr.LongIdent(isOptional, LongIdentWithDots(li, _), _, _) ->
        let lidoc = visitLongIdent li
        if isOptional then text "?" <+> lidoc else lidoc
    | SynExpr.LongIdentSet(LongIdentWithDots(li, _), expr, _) ->
        group (
            visitLongIdent li <+/+>
            text "<-" <+/!+>
            group (visitExpression expr)
        )
    | SynExpr.DotGet(expr, _, LongIdentWithDots(li, _), _) ->
        visitExpression expr <+> text "." <+> visitLongIdent li
    | SynExpr.DotSet(getexpr, LongIdentWithDots(li, _), valexpr, _) ->
        group (
            group (
                (visitExpression getexpr <+> text "." <+> visitLongIdent li) <+/+>
                text "<-"
            ) <+/!+>
            visitExpression valexpr
        )
    | SynExpr.Set(lval, rval, _) ->
        group (
            group ( visitExpression lval <+/+> text "<-" ) <+/!+>
            group ( visitExpression rval )
        )
    | SynExpr.LetOrUse(_, _, bindings, body, _) ->
        // Visit bindings (there may be multiple
        // for 'let .. = .. and .. = .. in ...'
        printfn "LetOrUse with the following bindings:"
        for binding in bindings do
            let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat,
                         retInfo, init, m, sp)) = binding
            visitPattern pat
            visitExpression init
        // Visit the body expression
        printfn "And the following body:"
        visitExpression body
let fname = "./src/prettiercs/Core.fs"
let fsource = System.IO.File.ReadAllText(fname)
let parsed = getUntypedTree (fname, fsource)

