module PrettySharp.CS

open PrettySharp.Core
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open System.Linq.Expressions
open System
open Microsoft.CodeAnalysis.CSharp
open System.Linq.Expressions

let indentLevel = 4
let bracket = PrettySharp.Core.bracket indentLevel
let indent = nest indentLevel

/// Concatenate two documents with a line-break in between.
let ( <+/+> ) x y = ifNotNil x y (x <+> line <+> y)

/// Concatenate two documents with a line break, and indent the second one.
let ( <+/!+> ) x y = ifNotNil x y (x <+> indent (line <+> y))

let join sep seq =
    if Seq.isEmpty seq
    then nil
    else
        let join x y = ifNotNil x y (x <+> sep <+> y)
        seq |> Seq.reduce join

let listJoin sep = join (text sep <+> line)

type Visitor = SyntaxNode->DOC

let visitToken (token : SyntaxToken) =
    if token.Span.IsEmpty
    then nil
    else text token.Text

let visitMemberAccessOrConditional (visit:Visitor) node =
    let formatMember (maes:MemberAccessExpressionSyntax) =
        let op = visitToken maes.OperatorToken
        let right = visit maes.Name
        softline <+> op <+> right

    let formatConditional (caes:ConditionalAccessExpressionSyntax) =
        let op = visitToken caes.OperatorToken
        let right = visit caes.WhenNotNull
        softline <+> op <+> right

    // Explicitly Recursive here to group properly.
    let rec gather memberList (node:SyntaxNode) =
        match node with
        | :? MemberAccessExpressionSyntax as maes ->
            gather ((formatMember maes)::memberList) maes.Expression
        | :? ConditionalAccessExpressionSyntax as caes ->
            gather ((formatConditional caes)::memberList) caes.Expression
        | _ ->
            let root = visit node
            let members =
                memberList
                |> List.reduce (<+>)
            group (root <+> indent (members))

    gather [] node

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
        match node with
            | null -> nil
            | _ -> this.Visit node

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
            constraints
        )

    member this.VisitBody (node:SyntaxNode) =
        if node.IsKind(SyntaxKind.Block)
        then line <+> this.Visit node
        else indent (line <+> this.Visit node)

    member this.VisitParameterOrArgumentList lb rb list =
        if Seq.isEmpty list
        then text (lb + rb)
        else
            let args = Seq.map (this.Visit) list |> listJoin ","
            text lb <+>
            group (indent (softline <+> args <+> text rb))

    override this.DefaultVisit node =
        failwith (sprintf "Could not visit a %A %A" (node.Kind()) node)

    override this.VisitAccessorDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let body =
            match node.Body with
                | null ->
                    text " " <+> this.Visit node.ExpressionBody <+> text ";"
                | _ -> line <+> this.Visit node.Body

        group (
            attrs <+/+>
            group (mods <+/+> visitToken node.Keyword)
        ) <+/+>
        body

    override this.VisitAccessorList node =
        let accessors = Seq.map (this.Visit) node.Accessors |> join line
        text "{" <+/+>  indent(accessors) <+/+> text "}"

    override this.VisitAliasQualifiedName node = // #filler
        this.Visit node.Alias <+> text "::" <+> this.Visit node.Name

    override this.VisitAnonymousMethodExpression node = //#filler
        let asyncKw = visitToken node.AsyncKeyword
        let ps = this.Visit node.ParameterList
        let body = this.Visit node.Body
        group (asyncKw <+/+> text "delegate" <+> ps) <+/+> body

    override this.VisitAnonymousObjectCreationExpression node = //#filler
        this.BracketedList "new {" "," "}" node.Initializers

    override this.VisitAnonymousObjectMemberDeclarator node = //#filler
        group (this.Visit node.NameEquals <+/!+> this.Visit node.Expression)

    override this.VisitArgument node =
        let namecolon = this.VisitOptional node.NameColon
        let refkind = visitToken node.RefKindKeyword
        let expr = this.Visit node.Expression
        namecolon <++> group(refkind <+/+> expr)

    override this.VisitArgumentList node =
        this.VisitParameterOrArgumentList "(" ")" node.Arguments

    override this.VisitArrayCreationExpression node =
        text "new" <++>
        this.Visit node.Type <+>
        this.VisitOptional node.Initializer

    override this.VisitArrayRankSpecifier node =
        this.BracketedList "[" "," "]" node.Sizes

    override this.VisitArrayType node =
        let ranks =
            node.RankSpecifiers |> Seq.map (this.Visit) |> Seq.reduce (<+>)
        this.Visit node.ElementType <+> ranks

    override this.VisitArrowExpressionClause node =
        group(text "=>" <+/!+> this.Visit node.Expression)

    override this.VisitAssignmentExpression node =
        let left = this.Visit node.Left
        let op = visitToken node.OperatorToken
        let right = this.Visit node.Right

        group (left <+> text " " <+> op <+/!+> right)

    override this.VisitAttribute node = //#filler
        this.Visit node.Name <+> this.VisitOptional node.ArgumentList

    override this.VisitAttributeArgument node = //#filler
        this.VisitOptional node.NameEquals <+/+>
        this.VisitOptional node.NameColon <+/+>
        this.Visit node.Expression

    override this.VisitAttributeArgumentList node =
        this.VisitParameterOrArgumentList "(" ")" node.Arguments

    override this.VisitAttributeList node = //#filler
        let target = this.VisitOptional node.Target
        let attrList = node.Attributes |> Seq.map (this.Visit) |> listJoin ","
        bracket "[" "]" (target <+> attrList)

    override this.VisitAttributeTargetSpecifier node = //#filler
        visitToken node.Identifier <+> text ":"

    override this.VisitAwaitExpression node = //#filler
        text "await" <++> this.Visit node.Expression

    override this.VisitBaseExpression _ = text "base"

    override this.VisitBaseList node =
        let bases = group(node.Types |> Seq.map (this.Visit) |> listJoin ",")
        text ":" <+/!+> bases

    override this.VisitBinaryExpression node =
        let left = this.Visit node.Left
        let op = visitToken node.OperatorToken
        let right = this.Visit node.Right

        group (left <+> text " " <+> op <+/+> right)

    override this.VisitBlock node =
        if Seq.isEmpty node.Statements
        then text "{ }"
        else
            let getsNewline (node:SyntaxNode) =
                match node with
                | :? LocalDeclarationStatementSyntax -> false
                | :? ExpressionStatementSyntax -> false
                | _ -> true

            let combineStatements (prevDoc, prevNode) stat =
                let statDoc = this.Visit stat
                match prevDoc, getsNewline prevNode with
                | NIL, _ -> (statDoc, stat)
                | _, true -> (prevDoc <+/+> line <+> statDoc, stat)
                | _, false -> (prevDoc <+/+> statDoc, stat)

            let block =
                node.Statements
                |> Seq.fold (combineStatements) (NIL, null)
                |> fst
                |> bracket "{" "}"
            breakParent <+> block

    override this.VisitBracketedArgumentList node =
        this.VisitParameterOrArgumentList "[" "]" node.Arguments

    override this.VisitBracketedParameterList node = //#filler
        this.VisitParameterOrArgumentList "[" "]" node.Parameters

    override this.VisitBreakStatement _ = breakParent <+> text "break;"

    override this.VisitCasePatternSwitchLabel node = //#filler
        let pattern = this.Visit node.Pattern
        let whenClause = this.VisitOptional node.WhenClause
        group (
            text "case" <+/!+>
            (group (pattern <+/+> whenClause) <+> text ":")
        )

    override this.VisitCaseSwitchLabel node = //#filler
        let expr = this.Visit node.Value
        group (text "case" <+/!+> (expr <+> text":"))

    override this.VisitCastExpression node =
        let typ = bracket "(" ")" (this.Visit node.Type)
        typ <+> this.Visit node.Expression

    override this.VisitCatchClause node = //#filler
        let decl = this.VisitOptional node.Declaration
        let filter = this.VisitOptional node.Filter
        let block = this.Visit node.Block
        group(text "catch" <++> decl <+> indent(line <+> filter)) <+/+> block

    override this.VisitCatchDeclaration node = //#filler
        let typ = this.Visit node.Type
        let identifier = visitToken node.Identifier
        bracket "(" ")" (group (typ <+/+> identifier))

    override this.VisitCatchFilterClause node = //#filler
        bracket "when (" ")" (this.Visit node.FilterExpression)

    override this.VisitCheckedExpression node = //#filler
        let kw = visitToken node.Keyword
        kw <+> bracket "(" ")" (this.Visit node.Expression)

    override this.VisitCheckedStatement node = //#filler
        let kw = visitToken node.Keyword
        kw <+/+> this.Visit node.Block

    override this.VisitClassDeclaration node =
        let decl = this.VisitTypeDeclaration node

        let foldMember doc m =
            let memberDoc = this.Visit m
            if doc = NIL
            then memberDoc
            else
                match m.Kind() with
                | SyntaxKind.FieldDeclaration -> doc <+/+> memberDoc
                | _ -> doc <+/+> line <+> memberDoc

        let members =
            node.Members
            |> Seq.fold (foldMember) nil
            |> bracket "{" "}"

        group (decl <+/+> members)

    override this.VisitClassOrStructConstraint node =
        visitToken node.ClassOrStructKeyword

    override this.VisitCompilationUnit node =
        let attribs = this.VisitChunk node.AttributeLists
        let usings = this.VisitChunk node.Usings
        let externs = this.VisitChunk node.Externs
        let members = this.VisitChunk node.Members

        attribs <+/+> usings <+/+> externs <+/+> members

    override this.VisitConditionalAccessExpression node =
        visitMemberAccessOrConditional (this.Visit) node

    override this.VisitConditionalExpression node = //#filler
        let condition = this.Visit node.Condition
        let whenTrue = this.Visit node.WhenTrue
        let whenFalse = this.Visit node.WhenFalse
        group (
            condition <+/+>
            group(text "?" <++> whenTrue) <+/+>
            group(text ":" <++> whenFalse)
        )

    override this.VisitConstructorDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let name = visitToken node.Identifier
        let parameterList = this.Visit node.ParameterList
        let initializer = this.VisitOptional node.Initializer
        let body =
            match node.Body with
                | null ->
                    text " " <+> this.Visit node.ExpressionBody <+> text ";"
                | _ -> line <+> this.Visit node.Body

        breakParent <+>
        group (
            group (
                group (
                    attrs <+/+>
                    mods <+/+>
                    name
                ) <+>
                parameterList <+/!+>
                initializer
            ) <+/+>
            body
        )

    override this.VisitConstantPattern node = this.Visit node.Expression

    override this.VisitConstructorInitializer node =
        let thisOrBase = visitToken node.ThisOrBaseKeyword
        text ":" <++> thisOrBase <+> this.Visit node.ArgumentList

    override this.VisitElementAccessExpression node =
        let expr = this.Visit node.Expression
        let args = this.Visit node.ArgumentList

        group(expr <+> args)

    override this.VisitEnumDeclaration node =
        let decl =
            let attribs = this.VisitChunk node.AttributeLists
            let modifiers = visitModifiers node.Modifiers
            let kw = visitToken node.EnumKeyword
            let id = visitToken node.Identifier
            let baseList = this.VisitOptional node.BaseList

            group (attribs <+/+> modifiers <+/+> kw <+/+> id <+/+> baseList)

        let members = this.BracketedList "{" "," "}" node.Members

        breakParent <+> group(group (decl <+/+> members))

    override this.VisitEnumMemberDeclaration node =
        let equalsValue = this.VisitOptional node.EqualsValue
        let id = visitToken node.Identifier

        group (id <+/+> equalsValue)

    override this.VisitEventDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let typ = this.Visit node.Type
        let name =
            let explicitInterface =
                this.VisitOptional node.ExplicitInterfaceSpecifier
            let id = visitToken node.Identifier
            explicitInterface <+> id
        let body = this.Visit node.AccessorList

        breakParent <+>
        group (
            group (
                attrs <+/+>
                group(
                    group(mods <+/+> text "event") <+/+>
                    group(typ <+/+> name)
                )
            ) <+/+>
            body
        )

    override this.VisitEqualsValueClause node =
        text "=" <+/!+> this.Visit node.Value

    override this.VisitExpressionStatement node =
        breakParent <+> group(this.Visit node.Expression <+> text ";")

    override this.VisitFieldDeclaration node =
        let attribs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let decl = this.Visit node.Declaration

        breakParent <+> group(attribs <+/+> mods <+/+> decl <+> text ";")

    override this.VisitForEachStatement node =
        // let await = visitToken node.AwaitKeyword
        let typ = this.Visit node.Type
        let id = visitToken node.Identifier
        let expr = this.Visit node.Expression
        let body = this.VisitBody node.Statement

        breakParent <+>
        group (
            group (
                text "foreach" <+/+>
                bracket "(" ")" (
                    group (typ <+/+> id <+/+> text "in") <+/!+> expr
                )
            ) <+>
            body
        )

    override this.VisitForStatement node =
        let declarationOrInit =
            let decl = this.VisitOptional node.Declaration
            let initializers =
                group(node.Initializers |> Seq.map (this.Visit) |> listJoin ",")
            decl <+> initializers
        let condition = this.VisitOptional node.Condition
        let incrementors =
            group(node.Incrementors |> Seq.map (this.Visit) |> listJoin ",")
        let body = this.VisitBody node.Statement
        breakParent <+>
        group (
            group (
                text "for" <+/+>
                bracket "(" ")" (
                    group(
                        (declarationOrInit <+> text ";") <+/+>
                        (condition <+> text ";") <+/+>
                        incrementors
                    )
                )
            ) <+>
            body
        )

    override this.VisitGenericName node =
        // If you're mad that we're not breaking the line before the argument
        // list, don't have super-complicated argument lists.
        visitToken node.Identifier <+> this.Visit node.TypeArgumentList

    override this.VisitGlobalStatement node =
        this.Visit node.Statement

    override this.VisitIdentifierName node =
        visitToken node.Identifier

    override this.VisitIfStatement node =
        let format (node:IfStatementSyntax) =
            text "if " <+>
            bracket "(" ")" (this.Visit node.Condition) <+>
            this.VisitBody node.Statement

        let rec gather chain (node:SyntaxNode) =
            match node with
            | :? IfStatementSyntax as ifs -> gather (ifs::chain) ifs.Else
            | :? ElseClauseSyntax as els -> gather chain els.Statement
            | last ->
                let trailingElse =
                    match last with
                    | null -> nil
                    | _ -> line <+> text "else" <+> this.VisitBody last

                let ifElseChain =
                    chain
                    |> List.rev
                    |> List.map (format)
                    |> List.reduce (fun x y -> x <+/+> text "else " <+> y)

                ifElseChain <+> trailingElse

        gather [] node

    override this.VisitImplicitArrayCreationExpression node =
        let commas = node.Commas |> Seq.map visitToken |> Seq.fold (<+>) nil
        let initializer = this.Visit node.Initializer
        text "new [" <+> commas <+> text "]" <+/!+> initializer

    override this.VisitIndexerDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let typ = this.Visit node.Type
        let name =
            this.VisitOptional node.ExplicitInterfaceSpecifier <+> text "this"
        let body =
            if node.AccessorList <> null
            then line <+> this.Visit node.AccessorList
            else text " " <+> this.Visit node.ExpressionBody <+> text ";"

        breakParent <+>
        group (
            group (
                attrs <+/+>
                group(mods <+/+> group(typ <+/+> name))
            ) <+>
            body
        )


    override this.VisitInitializerExpression node =
        let left = node.OpenBraceToken.Text
        let right = node.CloseBraceToken.Text
        this.BracketedList left "," right node.Expressions

    override this.VisitInvocationExpression node =
        let expr = this.Visit node.Expression
        let args = this.Visit node.ArgumentList

        group(expr <+> args)


    override this.VisitLiteralExpression node = visitToken node.Token

    override this.VisitLocalDeclarationStatement node =
        let mods = visitModifiers node.Modifiers
        let decl = this.Visit node.Declaration

        breakParent <+> group(mods <+/+> decl <+> text ";")

    override this.VisitMemberAccessExpression node =
        visitMemberAccessOrConditional (this.Visit) node

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
        let body =
            if node.Body <> null
            then line <+> this.Visit node.Body
            else text " " <+> this.Visit node.ExpressionBody <+> text ";"

        breakParent <+>
        group (
            group (
                group (
                    attrs <+/+>
                    group (mods <+/+> returnType) <+/+>
                    name
                ) <+>
                parameterList <+/+>
                constraints
            ) <+>
            body
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
        let typ = this.VisitOptional node.Type
        let args = this.VisitOptional node.ArgumentList
        let initializer = this.VisitOptional node.Initializer

        group (
            group (text "new" <++> typ <+> args) <+/+>
            initializer
        )

    override this.VisitOmittedArraySizeExpression _ = nil

    override this.VisitParameter node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let typ = this.VisitOptional node.Type
        let id = visitToken node.Identifier
        let default_ = this.VisitOptional node.Default

        group (attrs <+/+> mods <+/+> typ <+/+> id <+/+> default_)

    override this.VisitParameterList node =
        this.VisitParameterOrArgumentList "(" ")" node.Parameters

    override this.VisitParenthesizedExpression node =
        bracket "(" ")" (this.Visit node.Expression)

    override this.VisitParenthesizedLambdaExpression node =
        let async = visitToken node.AsyncKeyword
        let parameters = this.Visit node.ParameterList
        let arrow = visitToken node.ArrowToken
        let body = this.VisitBody node.Body

        group (group(async <+/+> parameters <+/+> arrow) <+> body)

    override this.VisitPostfixUnaryExpression node =
        this.Visit node.Operand <+> visitToken node.OperatorToken

    override this.VisitPredefinedType node = visitToken node.Keyword

    override this.VisitPropertyDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let typ = this.Visit node.Type
        let name =
            let explicitInterface =
                this.VisitOptional node.ExplicitInterfaceSpecifier
            let id = visitToken node.Identifier
            explicitInterface <+> id
        let body =
            if node.AccessorList <> null
            then
                line <+>
                this.Visit node.AccessorList <+/!+>
                this.VisitOptional node.Initializer
            else text " " <+> this.Visit node.ExpressionBody

        breakParent <+>
        group (
            group (
                attrs <+/+>
                group(mods <+/+> group(typ <+/+> name))
            ) <+>
            body <+>
            visitToken node.SemicolonToken
        )

    override this.VisitPrefixUnaryExpression node =
        visitToken node.OperatorToken <+> this.Visit node.Operand

    override this.VisitThisExpression _ = text "this"

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
        breakParent <+>
        group (text "return" <++> expr <+> text ";")

    override this.VisitSimpleBaseType node = this.Visit node.Type

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
        | init -> group (name <+/+> text "=") <+/!+> this.Visit init.Value

    override this.VisitVariableDeclaration node =
        let firstVar =
            let typ = this.Visit node.Type
            let first = Seq.head node.Variables
            let name = visitToken first.Identifier
            match first.Initializer with
            | null -> group (typ <+/!+> name)
            | init ->
                let initValue = this.Visit init.Value
                // N.B.: There should be some cases where we can break the line
                //       (and indent) after the '='. Prettier does this, see
                //       the `printAssignmentRight` function in
                //       `printer-estree.js`. (https://bit.ly/2RCANP8)
                group (
                    // I'm sorry this looks so bizarre.
                    // NOTE: This is *wrong*: when the inner group breaks (and
                    //     adds the line break) then initValue should be
                    //     indented. We don't have the ability to describe
                    //     that right now, because the nesting of the
                    //     indentation plays against the nesting of the
                    //     groups. (See the tests `LongFieldInitializer`,
                    //     which is currently wrong, and
                    //     `LongMethodArguments`, which is right. Fixing one
                    //     breaks the other.)
                    group (group (typ <+/!+> name) <++> text "=" <+> line) <+>
                    initValue
                )

        let restVars =
            (Seq.tail node.Variables) |> Seq.map (this.Visit) |> listJoin ","
        match restVars with
        | NIL -> firstVar
        | _ -> (firstVar <+> text ",") <+/!+> restVars

let visit (tree:SyntaxNode) =
    let visitor = new PrintVisitor()
    visitor.Visit(tree)


[<EntryPoint>]
let main argv =
    match Array.toList argv with
    | path::_ ->
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let tree = CSharpSyntaxTree.ParseText (System.IO.File.ReadAllText path)
        printfn "Parsed in %i ms" timer.ElapsedMilliseconds
        timer.Restart()
        let doc = visit (tree.GetRoot())
        printfn "Visited in %i ms" timer.ElapsedMilliseconds
        timer.Restart()
        let formatted = pretty 80 doc
        printfn "Formatted in %i ms" timer.ElapsedMilliseconds
        printfn "%s" formatted
        0
    | _ ->
        printfn "Unknown command line arguments!"
        -1
