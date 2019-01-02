module PrettySharp.CS

open PrettySharp.Core
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

let ifNotNil x y xy =
    match x,y with
    | x, NIL -> x
    | NIL, y -> y
    | x, y -> xy

let indentLevel = 4
let bracket = PrettySharp.Core.bracket indentLevel
let indent = nest indentLevel
let ( <+/+> ) x y = ifNotNil x y (x <+> line <+> y)
let ( <+/!+> ) x y = ifNotNil x y (x <+> indent (line <+> y))
let ( <+/*+> ) x y = ifNotNil x y (x <+> line <+> breakParent <+> y)

let join sep seq =
    if Seq.isEmpty seq
    then nil
    else
        let join x y = ifNotNil x y (x <+> sep <+> y)
        seq |> Seq.reduce join

let listJoin sep = join (text sep <+> line)

let visitToken (token : SyntaxToken) =
    if token.Span.IsEmpty
    then nil
    else text token.Text

let visitModifiers mods =
    if Seq.isEmpty mods
    then nil
    else group (mods |> Seq.map (visitToken) |> Seq.reduce (<+/+>))

type PrintVisitor() =
    inherit CSharpSyntaxVisitor<DOC>()

    member this.BracketedList l s r x =
        let contents = Seq.map (this.Visit) x |> listJoin s
        bracket l r contents

    member this.VisitChunk seq =
        Seq.map (this.Visit) seq |> Seq.fold (<+/+>) nil

    member this.VisitOptional (node : SyntaxNode) =
        if node = null then nil else this.Visit node

    member this.VisitTypeDeclaration (node:TypeDeclarationSyntax) =
        let attribs = this.VisitChunk node.AttributeLists
        let modifiers = visitModifiers node.Modifiers
        let kw = visitToken node.Keyword
        let id = visitToken node.Identifier
        let typeParams = this.VisitOptional node.TypeParameterList
        let baseList = this.VisitOptional node.BaseList
        let constraints = this.VisitChunk node.ConstraintClauses

        group (
            attribs <+/+>
            modifiers <+/+>
            kw <+/+>
            id <+/+>
            typeParams <+/+>
            baseList <+/+>
            constraints)

    member this.VisitBody (node:SyntaxNode) =
        if node.IsKind(SyntaxKind.Block)
        then line <+> this.Visit node
        else indent (line <+> this.Visit node)

    override this.DefaultVisit node =
        failwith (sprintf "Could not visit a %A %A" (node.Kind()) node)

    override this.VisitArgument node =
        let namecol_ = this.VisitOptional node.NameColon
        let refkind = visitToken node.RefKindKeyword
        let expr = this.Visit node.Expression

        group (namecol_ <+/+> refkind <+/+> expr)

    override this.VisitArgumentList node =
        let args = Seq.map (this.Visit) node.Arguments |> listJoin ","
        text "(" <+>
        group (indent (softline <+> group(args <+> text ")")))

    override this.VisitArrayType node =
        let ranks =
            node.RankSpecifiers |> Seq.map (this.Visit) |> Seq.reduce (<+>)
        this.Visit node.ElementType <+> ranks

    override this.VisitArrayRankSpecifier node =
        this.BracketedList "[" "," "]" node.Sizes

    override this.VisitAssignmentExpression node =
        let left = this.Visit node.Left
        let op = visitToken node.OperatorToken
        let right = this.Visit node.Right

        group (left <+> text " " <+> op <+/+> right)

    override this.VisitBinaryExpression node =
        let left = this.Visit node.Left
        let op = visitToken node.OperatorToken
        let right = this.Visit node.Right

        group (left <+> text " " <+> op <+/+> right)

    override this.VisitBlock node =
        if Seq.isEmpty node.Statements
        then text "{}"
        else
            let block =
                node.Statements
                |> Seq.map (this.Visit)
                |> Seq.reduce (<+/+>)
                |> bracket "{" "}"
            breakParent <+> block

    override this.VisitClassDeclaration node =
        let decl = this.VisitTypeDeclaration node
        let members =
            node.Members
            |> Seq.map (this.Visit)
            |> Seq.fold (<+/+>) nil
            |> bracket "{" "}"

        group (decl <+/+> members)

    override this.VisitConditionalAccessExpression node =
        let left = this.Visit node.Expression
        let op = visitToken node.OperatorToken
        let right = this.Visit node.WhenNotNull

        group (left <+> softline <+> group(op <+> softline <+> right))

    override this.VisitCompilationUnit node =
        let attribs = this.VisitChunk node.AttributeLists
        let usings = this.VisitChunk node.Usings
        let externs = this.VisitChunk node.Externs
        let members = this.VisitChunk node.Members

        attribs <+/+> usings <+/+> externs <+/+> members

    override this.VisitEnumDeclaration node =
        let decl =
            let attribs = this.VisitChunk node.AttributeLists
            let modifiers = visitModifiers node.Modifiers
            let kw = visitToken node.EnumKeyword
            let id = visitToken node.Identifier
            let baseList = this.VisitOptional node.BaseList

            group (attribs <+/+> modifiers <+/+> kw <+/+> id <+/+> baseList)

        let members = this.BracketedList "{" "," "}" node.Members

        breakParent <+> group (decl <+/+> members)

    override this.VisitEnumMemberDeclaration node =
        let equalsValue = this.VisitOptional node.EqualsValue
        let id = visitToken node.Identifier

        group (id <+/+> equalsValue)

    override this.VisitEqualsValueClause node =
        text "=" <+/!+> this.Visit node.Value

    override this.VisitExpressionStatement node =
        breakParent <+> this.Visit node.Expression <+> text ";"

    override this.VisitFieldDeclaration node =
        let attribs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let decl = this.Visit node.Declaration

        breakParent <+> attribs <+/+> mods <+/+> decl <+> text ";"

    override this.VisitForEachStatement node =
        // let await = visitToken node.AwaitKeyword
        let type_ = this.Visit node.Type
        let id = visitToken node.Identifier
        let expr = this.Visit node.Expression
        let body = this.VisitBody node.Statement

        breakParent <+>
        group (
            text "foreach" <+/+>
            bracket "(" ")" (
                group (type_ <+/+> id <+/+> text "in") <+/+> indent (expr)
            )
        ) <+>
        body

    override this.VisitGenericName node =
        let id = visitToken node.Identifier
        let args = this.Visit node.TypeArgumentList

        group (id <+> softline <+> args)

    override this.VisitIdentifierName node =
        visitToken node.Identifier

    override this.VisitIfStatement node =
        let format (node:IfStatementSyntax) =
            text "if " <+>
            bracket "(" ")" (this.Visit node.Condition) <+>
            this.VisitBody node.Statement

        let rec gather chain (node:SyntaxNode) =
            match node with
            | :? IfStatementSyntax as if_ -> gather (if_::chain) if_.Else
            | :? ElseClauseSyntax as else_ -> gather chain else_.Statement
            | last ->
                let trailingElse =
                    match last with
                    | null -> nil
                    | _ -> text "else" <+> this.VisitBody last

                let ifElseChain =
                    chain
                    |> List.rev
                    |> List.map (format)
                    |> List.reduce (fun x y -> x <+/+> text "else " <+> y)

                ifElseChain <+> trailingElse

        gather [] node

    override this.VisitInitializerExpression node =
        let left = node.OpenBraceToken.Text
        let right = node.CloseBraceToken.Text
        this.BracketedList left "," right node.Expressions

    override this.VisitInvocationExpression node =
        let expr = this.Visit node.Expression
        let args = this.Visit node.ArgumentList
        group (expr <+> args)

    override this.VisitLiteralExpression node = visitToken node.Token

    override this.VisitLocalDeclarationStatement node =
        let mods = visitModifiers node.Modifiers
        let decl = this.Visit node.Declaration

        breakParent <+> mods <+/+> decl <+> text ";"

    override this.VisitMemberAccessExpression node =
        let formatMember (maes:MemberAccessExpressionSyntax) =
            let op = visitToken maes.OperatorToken
            let name = this.Visit maes.Name
            softline <+> op <+> name

        // Explicitly Recursive here to group properly.
        let rec gather memberList (node:SyntaxNode) =
            match node with
            | :? MemberAccessExpressionSyntax as maes ->
                gather (maes::memberList) maes.Expression
            | _ ->
                let root = this.Visit node
                let members =
                    memberList
                    |> List.map (formatMember)
                    |> List.reduce (<+>)
                group (root <+> indent (members))

        gather [] node

    override this.VisitMemberBindingExpression node =
        group (visitToken node.OperatorToken <+> this.Visit node.Name)

    override this.VisitMethodDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let returnType = this.Visit node.ReturnType
        let name =
            let explicitInterface =
                this.VisitOptional node.ExplicitInterfaceSpecifier
            let id = visitToken node.Identifier
            let typeParams = this.VisitOptional node.TypeParameterList
            group (explicitInterface <+> id <+> softline <+> typeParams)
        let parameterList = this.Visit node.ParameterList
        let constraints = this.VisitChunk node.ConstraintClauses
        let body = this.VisitOptional node.Body
        let expressionBody = this.VisitOptional node.ExpressionBody
        let semi = visitToken node.SemicolonToken // Because of expr!

        breakParent <+>
        group (
            group (
                attrs <+/+>
                group (mods <+/+> returnType) <+/+>
                name <+>
                softline <+>
                parameterList <+/+>
                constraints
            ) <+/+>
            body <+/+>
            group (expressionBody <+> semi)
        )

    override this.VisitNameColon node =
        this.Visit node.Name <+> text ":"

    override this.VisitNamespaceDeclaration node =
        let name = this.Visit node.Name
        let contents =
            let usings = this.VisitChunk node.Usings
            let externs = this.VisitChunk node.Externs
            let members = this.VisitChunk node.Members
            bracket "{" "}" (usings <+/+> externs <+/+> members)

        group (text "namespace" <+/+> name) <+/+> contents

    override this.VisitNullableType node =
        group (this.Visit node.ElementType <+> text "?")

    override this.VisitObjectCreationExpression node =
        let type_ = this.VisitOptional node.Type
        let args = this.VisitOptional node.ArgumentList
        let initializer = this.VisitOptional node.Initializer
        group (
            group (text "new" <+/+> type_ <+/+> args) <+/+>
            initializer
        )

    override this.VisitOmittedArraySizeExpression _ = nil

    override this.VisitParameter node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let type_ = this.VisitOptional node.Type
        let id = visitToken node.Identifier
        let default_ = this.VisitOptional node.Default

        group (attrs <+/+> mods <+/+> type_ <+/+> id <+/+> default_)

    override this.VisitParameterList node =
        this.BracketedList "(" "," ")" node.Parameters

    override this.VisitParenthesizedExpression node =
        bracket "(" ")" (this.Visit node.Expression)

    override this.VisitParenthesizedLambdaExpression node =
        let async = visitToken node.AsyncKeyword
        let parameters = this.Visit node.ParameterList
        let arrow = visitToken node.ArrowToken
        let body = this.VisitBody node.Body

        group (group(async <+/+> parameters <+/+> arrow) <+> body)

    override this.VisitPredefinedType node = visitToken node.Keyword

    override this.VisitPrefixUnaryExpression node =
        visitToken node.OperatorToken <+> this.Visit node.Operand

    override this.VisitUsingDirective node =
        let name = this.Visit node.Name
        let alias = this.VisitOptional node.Alias
        let static_ = visitToken node.StaticKeyword

        breakParent <+>
        group (text "using" <+/+> static_ <+/+> alias <+/+> name <+> text ";")

    override this.VisitQualifiedName node =
        group(this.Visit node.Left <+> text "." <+> this.Visit node.Right)

    override this.VisitReturnStatement node =
        let expr = this.VisitOptional node.Expression
        breakParent <+> group (text "return" <+/+> indent (expr <+> text ";"))

    override this.VisitSimpleLambdaExpression node =
        let async = visitToken node.AsyncKeyword
        let param = this.Visit node.Parameter
        let arrow = visitToken node.ArrowToken
        let body = this.Visit node.Body
        group (async <+/+> param <+/+> arrow <+/+> body)

    override this.VisitTypeArgumentList node =
        this.BracketedList "<" "," ">" node.Arguments

    override this.VisitVariableDeclarator node =
        let name = visitToken node.Identifier
        match node.Initializer with
        | null -> name
        | init ->
            group (name <+/+> text "=") <+/!+> this.Visit init.Value

    override this.VisitVariableDeclaration node =
        let firstVar =
            let type_ = this.Visit node.Type
            let first = Seq.head node.Variables
            let name = visitToken first.Identifier
            match first.Initializer with
            | null -> group (type_ <+/!+> name)
            | init ->
                group (
                    group (type_ <+/!+> name <+/+> text "=") <+/!+>
                    this.Visit init.Value
                )

        let restVars =
            (Seq.tail node.Variables) |> Seq.map (this.Visit) |> listJoin ","
        firstVar <+/!+> restVars

let visit (tree:SyntaxTree) =
    let visitor = new PrintVisitor()
    visitor.Visit(tree.GetRoot())


[<EntryPoint>]
let main argv =
    match Array.toList argv with
    | path::_ ->
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let tree = CSharpSyntaxTree.ParseText (System.IO.File.ReadAllText path)
        printfn "Parsed in %i ms" timer.ElapsedMilliseconds
        timer.Restart()
        let doc = visit tree
        printfn "Visited in %i ms" timer.ElapsedMilliseconds
        timer.Restart()
        let formatted = pretty 80 doc
        printfn "Formatted in %i ms" timer.ElapsedMilliseconds
        printfn "%s" formatted
        0
    | _ ->
        printfn "Unknown command line arguments!"
        -1
