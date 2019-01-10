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

let notImplemented node = failwithf "%A not implemented" (node.GetType())

let notVisitingTrivia = failwith "Not visiting trivia explicitly!"

type PrintVisitor() =
    inherit CSharpSyntaxVisitor<DOC>()

    member this.BracketedList l s r x =
        let contents = Seq.map (this.Visit) x |> listJoin s
        bracket l r contents

    member this.VisitChunk seq =
        Seq.map (this.Visit) seq |> Seq.fold (<+/+>) nil

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

    override this.Visit node =
        match node with
            | null -> NIL
            | _ -> base.Visit node

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
        let namecolon = this.Visit node.NameColon
        let refkind = visitToken node.RefKindKeyword
        let expr = this.Visit node.Expression
        namecolon <++> group(refkind <+/+> expr)

    override this.VisitArgumentList node =
        this.VisitParameterOrArgumentList "(" ")" node.Arguments

    override this.VisitArrayCreationExpression node =
        text "new" <++>
        this.Visit node.Type <+>
        this.Visit node.Initializer

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
        this.Visit node.Name <+> this.Visit node.ArgumentList

    override this.VisitAttributeArgument node = //#filler
        this.Visit node.NameEquals <+/+>
        this.Visit node.NameColon <+/+>
        this.Visit node.Expression

    override this.VisitAttributeArgumentList node =
        this.VisitParameterOrArgumentList "(" ")" node.Arguments

    override this.VisitAttributeList node = //#filler
        let target = this.Visit node.Target
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
        let whenClause = this.Visit node.WhenClause
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
        let decl = this.Visit node.Declaration
        let filter = this.Visit node.Filter
        let block = this.Visit node.Block

        group(
            group(text "catch" <++> decl <+> indent(line <+> filter)) <+/+>
            block
        )

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

    override this.VisitConstantPattern node = this.Visit node.Expression

    override this.VisitConstructorConstraint node = text "new ()"

    override this.VisitConstructorDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let name = visitToken node.Identifier
        let parameterList = this.Visit node.ParameterList
        let initializer = this.Visit node.Initializer
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
            ) <+>
            body
        )

    override this.VisitConstructorInitializer node =
        let thisOrBase = visitToken node.ThisOrBaseKeyword
        text ":" <++> thisOrBase <+> this.Visit node.ArgumentList

    override this.VisitContinueStatement node = breakParent <+> text "continue;"

    override this.VisitConversionOperatorDeclaration node = //#filler
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let implicitOrExplicit = visitToken node.ImplicitOrExplicitKeyword
        let typ = this.Visit node.Type
        let parameterList = this.Visit node.ParameterList
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
                    implicitOrExplicit <+/+>
                    text "operator" <+/+>
                    typ
                ) <+> parameterList
            ) <+>
            body
        )

    override this.VisitConversionOperatorMemberCref node = notVisitingTrivia

    override this.VisitCrefBracketedParameterList node = notVisitingTrivia

    override this.VisitCrefParameter node = notVisitingTrivia

    override this.VisitCrefParameterList node = notVisitingTrivia

    override this.VisitDeclarationExpression node = //#filler
        this.Visit node.Type <++> this.Visit node.Designation

    override this.VisitDeclarationPattern node = //#filler
        this.Visit node.Type <++> this.Visit node.Designation

    override this.VisitDefaultExpression node = //#filler
        bracket "default(" ")" (this.Visit node.Type)

    override this.VisitDefaultSwitchLabel _ = text "default:"

    override this.VisitDefineDirectiveTrivia node = notVisitingTrivia

    override this.VisitDelegateDeclaration node = //#filler
        let attribs = this.VisitChunk node.AttributeLists
        let modifiers = visitModifiers node.Modifiers
        let typ = this.Visit node.ReturnType
        let name = visitToken node.Identifier
        let typeParams = this.Visit node.TypeParameterList
        let parameters = this.Visit node.ParameterList
        let constraints = this.VisitChunk node.ConstraintClauses

        breakParent <+>
        group (
            attribs <+/+>
            group (
                group (
                    modifiers <+/+>
                    text "delegate" <++>
                    typ <+/+>
                    name <+>
                    group (typeParams)
                ) <+>
                parameters <+/+>
                group (constraints)
            )
        )

    override this.VisitDestructorDeclaration node = //#filler
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let name = visitToken node.Identifier
        let parameterList = this.Visit node.ParameterList
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
                    text "~" <+>
                    name
                ) <+>
                parameterList
            ) <+>
            body
        )

    override this.VisitDiscardDesignation node = text "_"

    override this.VisitDocumentationCommentTrivia node = notVisitingTrivia

    override this.VisitDoStatement node = //#filler
        let condition = this.Visit node.Condition
        let body =
            // N.B.: This one is not like VisitBody because we *need* to wrap the
            //       body in curly-braces so we can put in the ... `while` condition.
            if node.Statement.IsKind(SyntaxKind.Block)
            then line <+> this.Visit node.Statement
            else bracket "{" "}" (this.Visit node)

        breakParent <+>
        group (
            text "do" <+>
            body <+/+>
            (bracket "while (" ")" condition) <+>
            text ";"
        )

    override this.VisitElementAccessExpression node =
        let expr = this.Visit node.Expression
        let args = this.Visit node.ArgumentList

        group(expr <+> args)

    override this.VisitElementBindingExpression node =
        this.Visit node.ArgumentList

    override this.VisitElifDirectiveTrivia node = notVisitingTrivia

    override this.VisitElseClause node =
        failwith "Should not happen: `else` is handled specially in `if`."

    override this.VisitElseDirectiveTrivia node = notVisitingTrivia

    override this.VisitEmptyStatement node = breakParent <+> text ";"

    override this.VisitEndIfDirectiveTrivia node = notVisitingTrivia

    override this.VisitEndRegionDirectiveTrivia node = notVisitingTrivia

    override this.VisitEnumDeclaration node =
        let decl =
            let attribs = this.VisitChunk node.AttributeLists
            let modifiers = visitModifiers node.Modifiers
            let kw = visitToken node.EnumKeyword
            let id = visitToken node.Identifier
            let baseList = this.Visit node.BaseList

            group (attribs <+/+> modifiers <+/+> kw <+/+> id <+/+> baseList)

        let members = this.BracketedList "{" "," "}" node.Members

        breakParent <+> group(group (decl <+/+> members))

    override this.VisitEnumMemberDeclaration node =
        let equalsValue = this.Visit node.EqualsValue
        let id = visitToken node.Identifier

        group (id <+/+> equalsValue)

    override this.VisitEqualsValueClause node =
        text "=" <+/!+> this.Visit node.Value

    override this.VisitErrorDirectiveTrivia node = notVisitingTrivia

    override this.VisitEventDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let typ = this.Visit node.Type
        let name =
            let explicitInterface =
                this.Visit node.ExplicitInterfaceSpecifier
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

    override this.VisitExplicitInterfaceSpecifier node =
        this.Visit node.Name <+> text "."

    override this.VisitExpressionStatement node =
        breakParent <+> group(this.Visit node.Expression <+> text ";")

    override this.VisitExternAliasDirective node = //#filler
        text "extern alias" <++> visitToken node.Identifier <+> text ":"

    override this.VisitFieldDeclaration node =
        let attribs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let decl = this.Visit node.Declaration

        breakParent <+> group(attribs <+/+> mods <+/+> decl <+> text ";")

    override this.VisitFinallyClause node =
        group (text "finally" <+/+> this.Visit node.Block)

    override this.VisitFixedStatement node =
        let decl = this.Visit node.Declaration
        let body = this.VisitBody node.Statement

        breakParent <+>
        group (
            (bracket "fixed (" ")" decl) <+> body
        )

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

    override this.VisitForEachVariableStatement node = //#filler
        // let await = visitToken node.AwaitKeyword
        let var = this.Visit node.Variable
        let expr = this.Visit node.Expression
        let body = this.VisitBody node.Statement

        breakParent <+>
        group (
            group (
                text "foreach" <+/+>
                bracket "(" ")" (
                    group (var <+/+> text "in") <+/!+> expr
                )
            ) <+>
            body
        )

    override this.VisitForStatement node =
        let declarationOrInit =
            let decl = this.Visit node.Declaration
            let initializers =
                group(node.Initializers |> Seq.map (this.Visit) |> listJoin ",")
            decl <+> initializers
        let condition = this.Visit node.Condition
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

    override this.VisitFromClause node = //#filler
        let typ = this.Visit node.Type
        let id = visitToken node.Identifier
        let expr = this.Visit node.Expression

        group (text "from" <++> typ <++> id <++> text "in" <++> expr)

    override this.VisitGenericName node =
        // If you're mad that we're not breaking the line before the argument
        // list, don't have super-complicated argument lists.
        visitToken node.Identifier <+> this.Visit node.TypeArgumentList

    override this.VisitGlobalStatement node =
        this.Visit node.Statement

    override this.VisitGotoStatement node =
        let caseOrDefault = visitToken node.CaseOrDefaultKeyword
        let expr = this.Visit node.Expression

        breakParent <+>
        group (text "goto" <++> caseOrDefault <++> expr <+> text ";")

    override this.VisitIdentifierName node =
        visitToken node.Identifier

    override this.VisitIfDirectiveTrivia node = notVisitingTrivia

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

    override this.VisitImplicitElementAccess node = this.Visit node.ArgumentList

    override this.VisitImplicitStackAllocArrayCreationExpression node = //#filler
        let initializer = this.Visit node.Initializer
        text "stackalloc[]" <+/!+> initializer

    override this.VisitIncompleteMember node =
        failwith "This isn't a real thing; the parse was broken."

    override this.VisitIndexerDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let typ = this.Visit node.Type
        let name =
            this.Visit node.ExplicitInterfaceSpecifier <+> text "this"
        let body =
            match node.AccessorList with
                | null -> text " " <+> this.Visit node.ExpressionBody <+> text ";"
                | _ -> line <+> this.Visit node.AccessorList

        breakParent <+>
        group (
            group (
                attrs <+/+>
                group(mods <+/+> group(typ <+/+> name))
            ) <+>
            body
        )

    override this.VisitIndexerMemberCref node = notVisitingTrivia

    override this.VisitInitializerExpression node =
        let left = node.OpenBraceToken.Text
        let right = node.CloseBraceToken.Text
        this.BracketedList left "," right node.Expressions

    override this.VisitInterfaceDeclaration node = notImplemented node

    override this.VisitInterpolatedStringExpression node = notImplemented node

    override this.VisitInterpolatedStringText node = notImplemented node

    override this.VisitInterpolation node = notImplemented node

    override this.VisitInterpolationAlignmentClause node = notImplemented node

    override this.VisitInterpolationFormatClause node = notImplemented node

    override this.VisitInvocationExpression node =
        let expr = this.Visit node.Expression
        let args = this.Visit node.ArgumentList

        group(expr <+> args)

    override this.VisitIsPatternExpression node = notImplemented node

    override this.VisitJoinClause node = notImplemented node

    override this.VisitJoinIntoClause node = notImplemented node

    override this.VisitLabeledStatement node = notImplemented node

    override this.VisitLetClause node = notImplemented node

    override this.VisitLineDirectiveTrivia node = notImplemented node

    override this.VisitLiteralExpression node = visitToken node.Token

    override this.VisitLoadDirectiveTrivia node = notImplemented node

    override this.VisitLocalDeclarationStatement node =
        let mods = visitModifiers node.Modifiers
        let decl = this.Visit node.Declaration

        breakParent <+> group(mods <+/+> decl <+> text ";")

    override this.VisitLocalFunctionStatement node = notImplemented node

    override this.VisitLockStatement node = notImplemented node

    override this.VisitMakeRefExpression node = notImplemented node

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
                this.Visit node.ExplicitInterfaceSpecifier
            let id = visitToken node.Identifier
            let typeParams = this.Visit node.TypeParameterList
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

    override this.VisitNameEquals node = notImplemented node

    override this.VisitNameMemberCref node = notImplemented node

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
        let typ = this.Visit node.Type
        let args = this.Visit node.ArgumentList
        let initializer = this.Visit node.Initializer

        group (
            group (text "new" <++> typ <+> args) <+/+>
            initializer
        )

    override this.VisitOmittedArraySizeExpression _ = nil

    override this.VisitOmittedTypeArgument _ = nil

    override this.VisitOperatorDeclaration node = notImplemented node

    override this.VisitOperatorMemberCref node = notImplemented node

    override this.VisitOrderByClause node = notImplemented node

    override this.VisitOrdering node = notImplemented node

    override this.VisitParameter node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let typ = this.Visit node.Type
        let id = visitToken node.Identifier
        let default_ = this.Visit node.Default

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

    override this.VisitParenthesizedVariableDesignation node =
        notImplemented node

    override this.VisitPointerType node = notImplemented node

    override this.VisitPostfixUnaryExpression node =
        this.Visit node.Operand <+> visitToken node.OperatorToken

    override this.VisitPragmaChecksumDirectiveTrivia node = notImplemented node

    override this.VisitPragmaWarningDirectiveTrivia node = notImplemented node

    override this.VisitPredefinedType node = visitToken node.Keyword

    override this.VisitPrefixUnaryExpression node =
        visitToken node.OperatorToken <+> this.Visit node.Operand

    override this.VisitPropertyDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let typ = this.Visit node.Type
        let name =
            let explicitInterface =
                this.Visit node.ExplicitInterfaceSpecifier
            let id = visitToken node.Identifier
            explicitInterface <+> id
        let body =
            if node.AccessorList <> null
            then
                line <+>
                this.Visit node.AccessorList <+/!+>
                this.Visit node.Initializer
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

    override this.VisitQualifiedCref node = notImplemented node

    override this.VisitQualifiedName node =
        group(this.Visit node.Left <+> text "." <+> this.Visit node.Right)

    override this.VisitQueryBody node = notImplemented node

    override this.VisitQueryContinuation node = notImplemented node

    override this.VisitQueryExpression node = notImplemented node

    override this.VisitReferenceDirectiveTrivia node = notImplemented node

    override this.VisitRefExpression node = notImplemented node

    override this.VisitRefType node = notImplemented node

    override this.VisitRefTypeExpression node = notImplemented node

    override this.VisitRefValueExpression node = notImplemented node

    override this.VisitRegionDirectiveTrivia node = notImplemented node

    override this.VisitReturnStatement node =
        let expr = this.Visit node.Expression
        breakParent <+>
        group (text "return" <++> expr <+> text ";")

    override this.VisitSelectClause node = notImplemented node

    override this.VisitShebangDirectiveTrivia node = notImplemented node

    override this.VisitSimpleBaseType node = this.Visit node.Type

    override this.VisitSimpleLambdaExpression node =
        let async = visitToken node.AsyncKeyword
        let param = this.Visit node.Parameter
        let arrow = visitToken node.ArrowToken
        let body = this.Visit node.Body
        group (async <+/+> param <+/+> arrow <+/+> body)

    override this.VisitSingleVariableDesignation node = notImplemented node

    override this.VisitSizeOfExpression node = notImplemented node

    override this.VisitSkippedTokensTrivia node = notImplemented node

    override this.VisitStackAllocArrayCreationExpression node = notImplemented node

    override this.VisitStructDeclaration node = notImplemented node

    override this.VisitSwitchSection node = notImplemented node

    override this.VisitSwitchStatement node = notImplemented node

    override this.VisitThisExpression _ = text "this"

    override this.VisitThrowExpression node = notImplemented node

    override this.VisitThrowStatement node = notImplemented node

    override this.VisitTryStatement node = notImplemented node

    override this.VisitTupleElement node = notImplemented node

    override this.VisitTupleExpression node = notImplemented node

    override this.VisitTupleType node = notImplemented node

    override this.VisitTypeArgumentList node =
        this.BracketedList "<" "," ">" node.Arguments

    override this.VisitTypeConstraint node = notImplemented node

    override this.VisitTypeCref node = notImplemented node

    override this.VisitTypeOfExpression node = notImplemented node

    override this.VisitTypeParameter node = notImplemented node

    override this.VisitTypeParameterList node = notImplemented node

    override this.VisitUndefDirectiveTrivia node = notImplemented node

    override this.VisitUnsafeStatement node = notImplemented node

    override this.VisitUsingDirective node =
        let name = this.Visit node.Name
        let alias = this.Visit node.Alias
        let static_ = visitToken node.StaticKeyword

        breakParent <+>
        group (text "using" <+/+> static_ <+/+> alias <+/+> name <+> text ";")

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

    override this.VisitVariableDeclarator node =
        let name = visitToken node.Identifier
        match node.Initializer with
        | null -> name
        | init -> group (name <+/+> text "=") <+/!+> this.Visit init.Value

    override this.VisitWarningDirectiveTrivia node = notImplemented node

    override this.VisitWhenClause node = notImplemented node

    override this.VisitWhereClause node = notImplemented node

    override this.VisitWhileStatement node = notImplemented node

    override this.VisitXmlCDataSection node = notImplemented node

    override this.VisitXmlComment node = notImplemented node

    override this.VisitXmlCrefAttribute node = notImplemented node

    override this.VisitXmlElement node = notImplemented node

    override this.VisitXmlElementEndTag node = notImplemented node

    override this.VisitXmlElementStartTag node = notImplemented node

    override this.VisitXmlEmptyElement node = notImplemented node

    override this.VisitXmlName node = notImplemented node

    override this.VisitXmlNameAttribute node = notImplemented node

    override this.VisitXmlPrefix node = notImplemented node

    override this.VisitXmlProcessingInstruction node = notImplemented node

    override this.VisitXmlText node = notImplemented node

    override this.VisitXmlTextAttribute node = notImplemented node

    override this.VisitYieldStatement node = notImplemented node


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
