module PrettySharp.CS

open System
open System.Linq.Expressions
open System.Linq.Expressions
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open PrettySharp.Core

let indentLevel = 4
let indent = nest indentLevel
let dedent = nest (-indentLevel)

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

let space = PrettySharp.Core.text " "

let visitTrivia (trivia:SyntaxTrivia) =
    let txt (t:SyntaxTrivia) = PrettySharp.Core.text (t.ToFullString().Trim())
    match trivia.Kind() with
        | SyntaxKind.EndOfLineTrivia | SyntaxKind.WhitespaceTrivia -> nil
        | SyntaxKind.SkippedTokensTrivia ->
            failwith "The parse actually failed."

        | SyntaxKind.DisabledTextTrivia
        | SyntaxKind.ConflictMarkerTrivia
        | SyntaxKind.PreprocessingMessageTrivia -> txt trivia

        | SyntaxKind.DocumentationCommentExteriorTrivia
        | SyntaxKind.MultiLineCommentTrivia
        | SyntaxKind.MultiLineDocumentationCommentTrivia ->
            breakParent <+> txt trivia

        // TODO: Push these around to other side of stuff like commas.
        | SyntaxKind.SingleLineDocumentationCommentTrivia
        | SyntaxKind.SingleLineCommentTrivia ->
            breakParent <+> txt trivia <+> line

        | SyntaxKind.BadDirectiveTrivia
        | SyntaxKind.DefineDirectiveTrivia
        | SyntaxKind.ElifDirectiveTrivia
        | SyntaxKind.ElseDirectiveTrivia
        | SyntaxKind.EndIfDirectiveTrivia
        | SyntaxKind.EndRegionDirectiveTrivia
        | SyntaxKind.ErrorDirectiveTrivia
        | SyntaxKind.IfDirectiveTrivia
        | SyntaxKind.LineDirectiveTrivia
        | SyntaxKind.LoadDirectiveTrivia
        | SyntaxKind.PragmaChecksumDirectiveTrivia
        | SyntaxKind.PragmaWarningDirectiveTrivia
        | SyntaxKind.ReferenceDirectiveTrivia
        | SyntaxKind.RegionDirectiveTrivia
        | SyntaxKind.ShebangDirectiveTrivia
        | SyntaxKind.UndefDirectiveTrivia
        | SyntaxKind.WarningDirectiveTrivia ->
            breakParent <+> setindent 0 (line <+> txt trivia) <+> line

        | _ -> failwith "Unexpected trivia type"

let visitTriviaList (trivia:SyntaxTriviaList) =
    if trivia.Count = 0
    then nil
    else trivia |> Seq.map (visitTrivia) |> Seq.fold (<+>) nil

let text (token:SyntaxToken) =
    // TODO: This is where all the trivia handling will go, eventually.
    let leading = visitTriviaList token.LeadingTrivia
    let trailing = visitTriviaList token.TrailingTrivia
    let tokenText =
        if token.Span.IsEmpty
        then nil
        else PrettySharp.Core.text token.Text
    leading <+> tokenText <+> trailing

let bracket (l:SyntaxToken) (r:SyntaxToken) =
    let left = text l
    let right = text r
    PrettySharp.Core.bracket indentLevel left right

type Visitor = SyntaxNode->DOC

let visitMemberAccessOrConditional (visit:Visitor) node =
    let formatMember (maes:MemberAccessExpressionSyntax) =
        let op = text maes.OperatorToken
        let right = visit maes.Name
        softline <+> op <+> right

    let formatConditional (caes:ConditionalAccessExpressionSyntax) =
        let op = text caes.OperatorToken
        let right = visit caes.WhenNotNull
        softline <+> op <+> right

    // Explicitly recursive here to group properly.
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
    else group (mods |> Seq.map (text) |> Seq.reduce (<+/+>))

let notVisitingTrivia = NIL

type PrintVisitor() =
    inherit CSharpSyntaxVisitor<DOC>()

    member this.VisitList<'t when 't :> SyntaxNode>
        (l:SeparatedSyntaxList<'t>) =
            let handleItem i =
                let sep =
                    if i < l.SeparatorCount
                    then text (l.GetSeparator(i))
                    else nil
                this.Visit l.[i] <+> sep

            seq { 0 .. l.Count - 1 }
            |> Seq.map (handleItem)
            |> Seq.fold (<+/+>) nil

    member this.BracketedList l r x =
        this.VisitList x |> bracket l r

    member this.VisitChunk seq =
        Seq.map (this.Visit) seq |> Seq.fold (<+/+>) nil

    member this.FoldMember doc m =
        let memberDoc = this.Visit m
        if doc = NIL
        then memberDoc
        else
            match m.Kind() with
                | SyntaxKind.FieldDeclaration -> doc <+/+> memberDoc
                | _ -> doc <+/+> line <+> memberDoc

    member this.FoldMembers left right members =
        members |> Seq.fold (this.FoldMember) nil |> bracket left right

    member this.VisitTypeDeclaration (node:TypeDeclarationSyntax) =
        let attribs = this.VisitChunk node.AttributeLists
        let modifiers = visitModifiers node.Modifiers
        let kw = text node.Keyword
        let id = text node.Identifier
        let typeParams = this.Visit node.TypeParameterList
        let baseList = this.Visit node.BaseList
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

    member this.VisitParameterOrArgumentList
        (lb:SyntaxToken) (rb:SyntaxToken) list =
            if Seq.isEmpty list
            then text lb <+> text rb
            else
                let args = this.VisitList list
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
            match node.Body, node.ExpressionBody with
                | null, null -> text node.SemicolonToken
                | null, expr ->
                    space <+> this.Visit expr <+> text node.SemicolonToken
                | body, _ -> line <+> this.Visit body

        group (
            attrs <+/+>
            group (mods <+/+> text node.Keyword)
        ) <+/+>
        body

    override this.VisitAccessorList node =
        let ob = text node.OpenBraceToken
        let accessors = Seq.map (this.Visit) node.Accessors |> join line
        let cb = text node.CloseBraceToken

        ob <+/+> indent(accessors) <+/+> cb

    override this.VisitAliasQualifiedName node = // #filler
        this.Visit node.Alias <+>
        text node.ColonColonToken <+>
        this.Visit node.Name

    override this.VisitAnonymousMethodExpression node = //#filler
        let async' = text node.AsyncKeyword
        let delegate' = text node.DelegateKeyword
        let ps = this.Visit node.ParameterList
        let body = this.Visit node.Body
        group (async' <+/+> delegate' <+> ps) <+/+> body

    override this.VisitAnonymousObjectCreationExpression node = //#filler
        text node.NewKeyword <++>
        this.BracketedList
            node.OpenBraceToken
            node.CloseBraceToken
            node.Initializers

    override this.VisitAnonymousObjectMemberDeclarator node = //#filler
        group (this.Visit node.NameEquals <+/!+> this.Visit node.Expression)

    override this.VisitArgument node =
        let namecolon = this.Visit node.NameColon
        let refkind = text node.RefKindKeyword
        let expr = this.Visit node.Expression
        namecolon <++> group(refkind <+/+> expr)

    override this.VisitArgumentList node =
        this.VisitParameterOrArgumentList
            node.OpenParenToken
            node.CloseParenToken
            node.Arguments

    override this.VisitArrayCreationExpression node =
        text node.NewKeyword <++>
        this.Visit node.Type <+>
        this.Visit node.Initializer

    override this.VisitArrayRankSpecifier node =
        this.BracketedList
            node.OpenBracketToken
            node.CloseBracketToken
            node.Sizes

    override this.VisitArrayType node =
        let ranks =
            node.RankSpecifiers |> Seq.map (this.Visit) |> Seq.reduce (<+>)
        this.Visit node.ElementType <+> ranks

    override this.VisitArrowExpressionClause node =
        group (text node.ArrowToken <+/!+> this.Visit node.Expression)

    override this.VisitAssignmentExpression node =
        let left = this.Visit node.Left
        let op = text node.OperatorToken
        let right = this.Visit node.Right

        group (left <++> op <+/!+> right)

    override this.VisitAttribute node = //#filler
        this.Visit node.Name <+> this.Visit node.ArgumentList

    override this.VisitAttributeArgument node = //#filler
        this.Visit node.NameEquals <+/+>
        this.Visit node.NameColon <+/+>
        this.Visit node.Expression

    override this.VisitAttributeArgumentList node =
        this.VisitParameterOrArgumentList
            node.OpenParenToken
            node.CloseParenToken
            node.Arguments

    override this.VisitAttributeList node = //#filler
        let target = this.Visit node.Target
        let attrList = this.VisitList node.Attributes
        bracket
            node.OpenBracketToken
            node.CloseBracketToken
            (target <+> attrList)

    override this.VisitAttributeTargetSpecifier node = //#filler
        text node.Identifier <+> text node.ColonToken

    override this.VisitAwaitExpression node = //#filler
        text node.AwaitKeyword <++> this.Visit node.Expression

    override this.VisitBaseExpression node = text node.Token

    override this.VisitBaseList node =
        let bases = group (this.VisitList node.Types)
        text node.ColonToken <+/!+> bases

    override this.VisitBinaryExpression node =
        let left = this.Visit node.Left
        let op = text node.OperatorToken
        let right = this.Visit node.Right

        group (left <++> op <+/+> right)

    override this.VisitBlock node =
        let ob = node.OpenBraceToken
        let cb = node.CloseBraceToken

        if Seq.isEmpty node.Statements
        then text ob <++> text cb
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
                |> bracket ob cb
            breakParent <+> block

    override this.VisitBracketedArgumentList node =
        this.VisitParameterOrArgumentList
            node.OpenBracketToken
            node.CloseBracketToken
            node.Arguments

    override this.VisitBracketedParameterList node = //#filler
        this.VisitParameterOrArgumentList
            node.OpenBracketToken
            node.CloseBracketToken
            node.Parameters

    override this.VisitBreakStatement node =
        breakParent <+> text node.BreakKeyword <+> text node.SemicolonToken

    override this.VisitCasePatternSwitchLabel node = //#filler
        let pattern = this.Visit node.Pattern
        let whenClause = this.Visit node.WhenClause
        group (
            text node.Keyword <+/!+>
            (
                group (pattern <+/+> whenClause) <+>
                text node.ColonToken
            )
        )

    override this.VisitCaseSwitchLabel node = //#filler
        let expr = this.Visit node.Value
        group (text node.Keyword <+/!+> (expr <+> text node.ColonToken))

    override this.VisitCastExpression node =
        let typ =
            bracket
                node.OpenParenToken
                node.CloseParenToken
                (this.Visit node.Type)
        typ <+> this.Visit node.Expression

    override this.VisitCatchClause node = //#filler
        let catch' = text node.CatchKeyword
        let decl = this.Visit node.Declaration
        let filter = this.Visit node.Filter
        let block = this.Visit node.Block

        group(
            group (
                catch' <++>
                decl <+/!+>
                filter
            ) <+/+>
            block
        )

    override this.VisitCatchDeclaration node = //#filler
        let typ = this.Visit node.Type
        let identifier = text node.Identifier

        bracket
            node.OpenParenToken
            node.CloseParenToken
            (group (typ <+/+> identifier))

    override this.VisitCatchFilterClause node = //#filler
        text node.WhenKeyword <++>
        bracket
            node.OpenParenToken
            node.CloseParenToken
            (this.Visit node.FilterExpression)

    override this.VisitCheckedExpression node = //#filler
        text node.Keyword <++>
        bracket
            node.OpenParenToken
            node.CloseParenToken
            (this.Visit node.Expression)

    override this.VisitCheckedStatement node = //#filler
        let kw = text node.Keyword
        kw <+/+> this.Visit node.Block

    override this.VisitClassDeclaration node =
        let decl = this.VisitTypeDeclaration node
        let members =
            let ob = node.OpenBraceToken
            let cb = node.CloseBraceToken
            this.FoldMembers ob cb node.Members

        group (decl <+/+> members)

    override this.VisitClassOrStructConstraint node =
        text node.ClassOrStructKeyword

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
        let qm' = text node.QuestionToken
        let whenTrue = this.Visit node.WhenTrue
        let colon' = text node.ColonToken
        let whenFalse = this.Visit node.WhenFalse

        group (
            condition <+/+>
            group (qm' <++> whenTrue) <+/+>
            group (colon' <++> whenFalse)
        )

    override this.VisitConstantPattern node = this.Visit node.Expression

    override this.VisitConstructorConstraint node =
        text node.NewKeyword <++>
        text node.OpenParenToken <+>
        text node.CloseParenToken

    override this.VisitConstructorDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let name = text node.Identifier
        let parameterList = this.Visit node.ParameterList
        let initializer = this.Visit node.Initializer
        let body =
            match node.Body, node.ExpressionBody with
                | null, null -> text node.SemicolonToken
                | null, expr ->
                    space <+> this.Visit expr <+> text node.SemicolonToken
                | body, _ -> line <+> this.Visit body

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
        let colon' = text node.ColonToken
        let thisOrBase = text node.ThisOrBaseKeyword

        colon' <++> thisOrBase <+> this.Visit node.ArgumentList

    override this.VisitContinueStatement node =
        breakParent <+> text node.ContinueKeyword <+> text node.SemicolonToken

    override this.VisitConversionOperatorDeclaration node = //#filler
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let implicitOrExplicit = text node.ImplicitOrExplicitKeyword
        let operator' = text node.OperatorKeyword
        let typ = this.Visit node.Type
        let parameterList = this.Visit node.ParameterList
        let body =
            match node.Body, node.ExpressionBody with
                | null, null -> text node.SemicolonToken
                | null, expr ->
                    space <+> this.Visit expr <+> text node.SemicolonToken
                | body, _ -> line <+> this.Visit body

        breakParent <+>
        group (
            group (
                group (
                    attrs <+/+>
                    mods <+/+>
                    implicitOrExplicit <+/+>
                    operator' <+/+>
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
        text node.Keyword <+>
        bracket
            node.OpenParenToken
            node.CloseParenToken
            (this.Visit node.Type)

    override this.VisitDefaultSwitchLabel node =
        text node.Keyword <+> text node.ColonToken

    override this.VisitDefineDirectiveTrivia node = notVisitingTrivia

    override this.VisitDelegateDeclaration node = //#filler
        let attribs = this.VisitChunk node.AttributeLists
        let modifiers = visitModifiers node.Modifiers
        let typ = this.Visit node.ReturnType
        let name = text node.Identifier
        let delegate' = text node.DelegateKeyword
        let typeParams = this.Visit node.TypeParameterList
        let parameters = this.Visit node.ParameterList
        let constraints = this.VisitChunk node.ConstraintClauses

        breakParent <+>
        group (
            attribs <+/+>
            group (
                group (
                    modifiers <+/+>
                    delegate' <++>
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
        let tilde' = text node.TildeToken
        let name = text node.Identifier
        let parameterList = this.Visit node.ParameterList
        let body =
            match node.Body, node.ExpressionBody with
                | null, null -> text node.SemicolonToken
                | null, expr ->
                    space <+> this.Visit expr <+> text node.SemicolonToken
                | body, _ -> line <+> this.Visit body

        breakParent <+>
        group (
            group (
                group (
                    attrs <+/+>
                    mods <+/+>
                    tilde' <+>
                    name
                ) <+>
                parameterList
            ) <+>
            body
        )

    override this.VisitDiscardDesignation node = text node.UnderscoreToken

    override this.VisitDocumentationCommentTrivia node = notVisitingTrivia

    override this.VisitDoStatement node = //#filler
        let do' = text node.DoKeyword
        // N.B.: This one is not like VisitBody because we *need* to wrap the
        //       body in curly-braces so we can put in the ... `while`
        //       condition.
        let block =
            if node.Statement.IsKind(SyntaxKind.Block)
            then node.Statement :?> BlockSyntax
            else SyntaxFactory.Block(node.Statement)
        let body = line <+> this.Visit block
        let while' = text node.WhileKeyword
        let condition = this.Visit node.Condition
        let semicolon' = text node.SemicolonToken

        breakParent <+>
        group (
            do' <+>
            body <+/+>
            (
                while' <+>
                bracket node.OpenParenToken node.CloseParenToken condition
            ) <+>
            semicolon'
        )

    override this.VisitElementAccessExpression node =
        let expr = this.Visit node.Expression
        let args = this.Visit node.ArgumentList

        group (expr <+> args)

    override this.VisitElementBindingExpression node =
        this.Visit node.ArgumentList

    override this.VisitElifDirectiveTrivia node = notVisitingTrivia

    override this.VisitElseClause node =
        failwith "Should not happen: `else` is handled specially in `if`."

    override this.VisitElseDirectiveTrivia node = notVisitingTrivia

    override this.VisitEmptyStatement node =
        breakParent <+> text node.SemicolonToken

    override this.VisitEndIfDirectiveTrivia node = notVisitingTrivia

    override this.VisitEndRegionDirectiveTrivia node = notVisitingTrivia

    override this.VisitEnumDeclaration node =
        let decl =
            let attribs = this.VisitChunk node.AttributeLists
            let modifiers = visitModifiers node.Modifiers
            let kw = text node.EnumKeyword
            let id = text node.Identifier
            let baseList = this.Visit node.BaseList

            group (attribs <+/+> modifiers <+/+> kw <+/+> id <+/+> baseList)

        let members =
            let ob = node.OpenBraceToken
            let cb = node.CloseBraceToken
            this.BracketedList ob cb node.Members

        breakParent <+> group(group (decl <+/+> members))

    override this.VisitEnumMemberDeclaration node =
        let equalsValue = this.Visit node.EqualsValue
        let id = text node.Identifier

        group (id <+/+> equalsValue)

    override this.VisitEqualsValueClause node =
        text node.EqualsToken <+/!+> this.Visit node.Value

    override this.VisitErrorDirectiveTrivia node = notVisitingTrivia

    override this.VisitEventDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let event' = text node.EventKeyword
        let mods = visitModifiers node.Modifiers
        let typ = this.Visit node.Type
        let name =
            let explicitInterface =
                this.Visit node.ExplicitInterfaceSpecifier
            let id = text node.Identifier
            explicitInterface <+> id
        let body = this.Visit node.AccessorList

        breakParent <+>
        group (
            group (
                attrs <+/+>
                group(
                    group(mods <+/+> event') <+/+>
                    group(typ <+/+> name)
                )
            ) <+/+>
            body
        )

    override this.VisitEventFieldDeclaration node =
        let attribs = this.VisitChunk node.AttributeLists
        let event' = text node.EventKeyword
        let mods = visitModifiers node.Modifiers
        let decl = this.Visit node.Declaration

        breakParent <+>
        group(
            attribs <+/+>
            mods <+/+>
            (event' <++> decl) <+>
            text node.SemicolonToken
        )

    override this.VisitExplicitInterfaceSpecifier node =
        this.Visit node.Name <+> text node.DotToken

    override this.VisitExpressionStatement node =
        breakParent <+>
        group (this.Visit node.Expression <+> text node.SemicolonToken)

    override this.VisitExternAliasDirective node = //#filler
        text node.ExternKeyword <++>
        text node.AliasKeyword <++>
        text node.Identifier <+>
        text node.SemicolonToken

    override this.VisitFieldDeclaration node =
        let attribs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let decl = this.Visit node.Declaration

        breakParent <+>
        group (attribs <+/+> mods <+/+> decl <+> text node.SemicolonToken)

    override this.VisitFinallyClause node =
        group (text node.FinallyKeyword <+/+> this.Visit node.Block)

    override this.VisitFixedStatement node =
        let decl = this.Visit node.Declaration
        let body = this.VisitBody node.Statement

        breakParent <+>
        group (
            (
                text node.FixedKeyword <++>
                bracket node.OpenParenToken node.CloseParenToken decl
            ) <+>
            body
        )

    override this.VisitForEachStatement node =
        // let await = text node.AwaitKeyword
        let foreach' = text node.ForEachKeyword
        let typ = this.Visit node.Type
        let id = text node.Identifier
        let in' = text node.InKeyword
        let expr = this.Visit node.Expression
        let body = this.VisitBody node.Statement

        breakParent <+>
        group (
            group (
                foreach'  <+/+>
                bracket node.OpenParenToken node.CloseParenToken (
                    group (typ <+/+> id <+/+> in') <+/!+> expr
                )
            ) <+>
            body
        )

    override this.VisitForEachVariableStatement node = //#filler
        // let await = text node.AwaitKeyword
        let foreach' = text node.ForEachKeyword
        let var = this.Visit node.Variable
        let in' = text node.InKeyword
        let expr = this.Visit node.Expression
        let body = this.VisitBody node.Statement

        breakParent <+>
        group (
            group (
                foreach' <+/+>
                bracket node.OpenParenToken node.CloseParenToken (
                    group (var <+/+> in') <+/!+> expr
                )
            ) <+>
            body
        )

    override this.VisitForStatement node =
        let for' = text node.ForKeyword
        let declarationOrInit =
            let decl = this.Visit node.Declaration
            let initializers = group (this.VisitList node.Initializers)
            decl <+> initializers
        let condition = this.Visit node.Condition
        let incrementors = group (this.VisitList node.Incrementors)
        let body = this.VisitBody node.Statement

        breakParent <+>
        group (
            group (
                for' <+/+>
                bracket node.OpenParenToken node.CloseParenToken (
                    group(
                        (
                            declarationOrInit <+>
                            text node.FirstSemicolonToken
                        ) <+/+>
                        (condition <+> text node.SecondSemicolonToken) <+/+>
                        incrementors
                    )
                )
            ) <+>
            body
        )

    override this.VisitFromClause node = //#filler
        let from' = text node.FromKeyword
        let typ = this.Visit node.Type
        let id = text node.Identifier
        let in' = text node.InKeyword
        let expr = this.Visit node.Expression

        group (from' <+/!+> group (typ <++> id <++> in' <++> expr))

    override this.VisitGenericName node =
        // If you're mad that we're not breaking the line before the argument
        // list, don't have super-complicated argument lists.
        text node.Identifier <+> this.Visit node.TypeArgumentList

    override this.VisitGlobalStatement node =
        this.Visit node.Statement

    override this.VisitGotoStatement node =
        let caseOrDefault = text node.CaseOrDefaultKeyword
        let expr = this.Visit node.Expression

        breakParent <+>
        group (
            text node.GotoKeyword <++>
            caseOrDefault <++>
            expr <+>
            text node.SemicolonToken
        )

    override this.VisitIdentifierName node =
        text node.Identifier

    override this.VisitIfDirectiveTrivia node = notVisitingTrivia

    override this.VisitIfStatement node =
        let parentElse (node:SyntaxNode) =
            match node.Parent with
                | :? ElseClauseSyntax as else' -> text else'.ElseKeyword
                | _ -> nil

        let format (node:IfStatementSyntax) =
            let else' = parentElse node
            let if' = text node.IfKeyword
            let op = node.OpenParenToken
            let cp = node.CloseParenToken
            let condition = this.Visit node.Condition
            let body = this.VisitBody node.Statement

            else' <++> if' <++> bracket op cp condition <+> body

        let rec gather chain (node:SyntaxNode) =
            match node with
            | :? IfStatementSyntax as ifs -> gather (ifs::chain) ifs.Else
            | :? ElseClauseSyntax as els -> gather chain els.Statement
            | last ->
                let trailingElse =
                    match last with
                    | null -> nil
                    | _ -> parentElse last <++> this.VisitBody last

                let ifElseChain =
                    chain
                    |> List.rev
                    |> List.map (format)
                    |> List.reduce (<+/+>)

                ifElseChain <+/+> trailingElse

        gather [] node

    override this.VisitImplicitArrayCreationExpression node =
        let new' = text node.NewKeyword
        let ob = text node.OpenBracketToken
        let commas = node.Commas |> Seq.map text |> Seq.fold (<+>) nil
        let cb = text node.CloseBracketToken
        let initializer = this.Visit node.Initializer

        new' <++> ob <+> commas <+> cb <+/!+> initializer

    override this.VisitImplicitElementAccess node = this.Visit node.ArgumentList

    override this.VisitImplicitStackAllocArrayCreationExpression node = //#filler
        let stackalloc' = text node.StackAllocKeyword
        let ob = text node.OpenBracketToken
        let cb = text node.CloseBracketToken
        let initializer = this.Visit node.Initializer

        stackalloc' <+> ob <+> cb <+/!+> initializer

    override this.VisitIncompleteMember node =
        failwith "This isn't a real thing; the parse was broken."

    override this.VisitIndexerDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let typ = this.Visit node.Type
        let name =
            this.Visit node.ExplicitInterfaceSpecifier <+>
            text node.ThisKeyword
        let body =
            match node.AccessorList, node.ExpressionBody with
                | null, null -> text node.SemicolonToken
                | null, expr ->
                    space <+> this.Visit expr <+> text node.SemicolonToken
                | body, _ -> line <+> this.Visit body

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
        let left = node.OpenBraceToken
        let right = node.CloseBraceToken
        this.BracketedList left right node.Expressions

    override this.VisitInterfaceDeclaration node = //#filler
        let decl = this.VisitTypeDeclaration node
        let members =
            let ob = node.OpenBraceToken
            let cb = node.CloseBraceToken
            this.FoldMembers ob cb node.Members

        group (decl <+/+> members)

    override this.VisitInterpolatedStringExpression node =
        let starting = text node.StringStartToken
        let contents = node.Contents |> Seq.map (this.Visit) |> Seq.reduce (<+>)
        let ending = text node.StringEndToken

        group (starting <+> contents <+> ending)

    override this.VisitInterpolatedStringText node = text node.TextToken

    override this.VisitInterpolation node = //#filler
        let ob = text node.OpenBraceToken
        let expr = this.Visit node.Expression
        let alignment = this.Visit node.AlignmentClause
        let format = this.Visit node.FormatClause
        let cb = text node.CloseBraceToken

        group (ob <+> expr <+> alignment <+> format <+> cb)

    override this.VisitInterpolationAlignmentClause node = //#filler
        text node.CommaToken <+> this.Visit node.Value

    override this.VisitInterpolationFormatClause node = //#filler
        text node.ColonToken <+> text node.FormatStringToken

    override this.VisitInvocationExpression node =
        let expr = this.Visit node.Expression
        let args = this.Visit node.ArgumentList

        group (expr <+> args)

    override this.VisitIsPatternExpression node = //#filler
        let expr = this.Visit node.Expression
        let is' = text node.IsKeyword
        let pattern = this.Visit node.Pattern

        group (expr <+/+> is' <+/+> pattern)

    override this.VisitJoinClause node = //#filler
        let join' = text node.JoinKeyword
        let typ = this.Visit node.Type
        let id = text node.Identifier
        let in' = text node.InKeyword
        let inexpr = this.Visit node.InExpression
        let on' = text node.OnKeyword
        let leftexpr = this.Visit node.LeftExpression
        let equals' = text node.EqualsKeyword
        let rightexpr = this.Visit node.RightExpression
        let into = this.Visit node.Into

        group (
            join' <+/!+>
            group (
                group (typ <++> id <++> in' <++> inexpr) <+/!+>
                group (
                    group (on' <++> leftexpr) <+/+>
                    group (equals' <++> rightexpr)
                )
            ) <++>
            into
        )

    override this.VisitJoinIntoClause node =
        text node.IntoKeyword <++> text node.Identifier

    override this.VisitLabeledStatement node =
        let id = text node.Identifier
        let colon' = text node.ColonToken
        let stat = this.Visit node.Statement

        breakParent <+> group (id <+> colon' <++> stat)

    override this.VisitLetClause node = //#filler
        let let' = text node.LetKeyword
        let id = text node.Identifier
        let equals' = text node.EqualsToken
        let expr = this.Visit node.Expression

        group (
            let' <+/!+>
            group (id <++> equals' <+/!+> group (expr))
        )

    override this.VisitLineDirectiveTrivia node = notVisitingTrivia

    override this.VisitLiteralExpression node = text node.Token

    override this.VisitLoadDirectiveTrivia node = notVisitingTrivia

    override this.VisitLocalDeclarationStatement node =
        let mods = visitModifiers node.Modifiers
        let decl = this.Visit node.Declaration

        breakParent <+> group(mods <+/+> decl <+> text node.SemicolonToken)

    override this.VisitLocalFunctionStatement node = //#filler
        let mods = visitModifiers node.Modifiers
        let returnType = this.Visit node.ReturnType
        let name =
            let id = text node.Identifier
            let typeParams = this.Visit node.TypeParameterList
            group (id <+> softline <+> typeParams)
        let parameterList = this.Visit node.ParameterList
        let constraints = this.VisitChunk node.ConstraintClauses
        let body =
            match node.Body, node.ExpressionBody with
                | null, null -> text node.SemicolonToken
                | null, expr ->
                    space <+> this.Visit expr <+> text node.SemicolonToken
                | body, _ -> line <+> this.Visit body

        breakParent <+>
        group (
            group (
                group (
                    group (mods <+/+> returnType) <+/+>
                    name
                ) <+>
                parameterList <+/+>
                constraints
            ) <+>
            body
        )

    override this.VisitLockStatement node =
        let lock' = text node.LockKeyword
        let op = node.OpenParenToken
        let expr = this.Visit node.Expression
        let cp = node.CloseParenToken
        let body = this.VisitBody node.Statement

        breakParent <+>
        group (lock' <+/+> bracket op cp expr) <+>
        body

    override this.VisitMakeRefExpression node = //#filler
        text node.Keyword <+>
        bracket
            node.OpenParenToken
            node.CloseParenToken
            (this.Visit node.Expression)

    override this.VisitMemberAccessExpression node =
        visitMemberAccessOrConditional (this.Visit) node

    override this.VisitMemberBindingExpression node =
        group (text node.OperatorToken <+> this.Visit node.Name)

    override this.VisitMethodDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let returnType = this.Visit node.ReturnType
        let name =
            let explicitInterface =
                this.Visit node.ExplicitInterfaceSpecifier
            let id = text node.Identifier
            let typeParams = this.Visit node.TypeParameterList
            group (explicitInterface <+> id <+> softline <+> typeParams)
        let parameterList = this.Visit node.ParameterList
        let constraints = this.VisitChunk node.ConstraintClauses
        let body =
            match node.Body, node.ExpressionBody with
                | null, null -> text node.SemicolonToken
                | null, expr ->
                    space <+> this.Visit expr <+> text node.SemicolonToken
                | body, _ -> line <+> this.Visit body

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
        this.Visit node.Name <+> text node.ColonToken

    override this.VisitNameEquals node =
        this.Visit node.Name <++> text node.EqualsToken

    override this.VisitNameMemberCref node = notVisitingTrivia

    override this.VisitNamespaceDeclaration node =
        let namespace' = text node.NamespaceKeyword
        let name = this.Visit node.Name
        let contents =
            let usings = this.VisitChunk node.Usings
            let externs = this.VisitChunk node.Externs
            let members = this.VisitChunk node.Members
            let ob = node.OpenBraceToken
            let cb = node.CloseBraceToken

            bracket ob cb (usings <+/+> externs <+/+> members)

        group (namespace' <+/+> name) <+/+> contents

    override this.VisitNullableType node =
        group (this.Visit node.ElementType <+> text node.QuestionToken)

    override this.VisitObjectCreationExpression node =
        let new' = text node.NewKeyword
        let typ = this.Visit node.Type
        let args = this.Visit node.ArgumentList
        let initializer = this.Visit node.Initializer

        group (
            group (new' <++> typ <+> args) <+/+>
            initializer
        )

    override this.VisitOmittedArraySizeExpression _ = nil

    override this.VisitOmittedTypeArgument _ = nil

    override this.VisitOperatorDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let returnType = this.Visit node.ReturnType
        let name = text node.OperatorToken
        let parameterList = this.Visit node.ParameterList
        let body =
            match node.Body, node.ExpressionBody with
                | null, null -> text node.SemicolonToken
                | null, expr ->
                    space <+> this.Visit expr <+> text node.SemicolonToken
                | body, _ -> line <+> this.Visit body

        breakParent <+>
        group (
            group (
                group (
                    attrs <+/+>
                    group (mods <+/+> returnType) <+/+>
                    name
                ) <+>
                parameterList
            ) <+>
            body
        )

    override this.VisitOperatorMemberCref node = notVisitingTrivia

    override this.VisitOrderByClause node =
        let orderings = this.VisitList node.Orderings

        group (text node.OrderByKeyword <+/!+> group (orderings))

    override this.VisitOrdering node =
        let ascdesc = text node.AscendingOrDescendingKeyword
        let expr = this.Visit node.Expression

        expr <++> ascdesc

    override this.VisitParameter node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let typ = this.Visit node.Type
        let id = text node.Identifier
        let defaultValue = this.Visit node.Default

        group (attrs <+/+> mods <+/+> typ <+/+> id <+/+> defaultValue)

    override this.VisitParameterList node =
        this.VisitParameterOrArgumentList
            node.OpenParenToken
            node.CloseParenToken
            node.Parameters

    override this.VisitParenthesizedExpression node =
        bracket
            node.OpenParenToken
            node.CloseParenToken
            (this.Visit node.Expression)

    override this.VisitParenthesizedLambdaExpression node =
        let async = text node.AsyncKeyword
        let parameters = this.Visit node.ParameterList
        let arrow = text node.ArrowToken
        let body = this.VisitBody node.Body

        group (group(async <+/+> parameters <+/+> arrow) <+> body)

    override this.VisitParenthesizedVariableDesignation node =
        bracket
            node.OpenParenToken
            node.CloseParenToken
            (this.VisitList node.Variables)

    override this.VisitPointerType node =
        this.Visit node.ElementType <+> text node.AsteriskToken

    override this.VisitPostfixUnaryExpression node =
        this.Visit node.Operand <+> text node.OperatorToken

    override this.VisitPragmaChecksumDirectiveTrivia node = notVisitingTrivia

    override this.VisitPragmaWarningDirectiveTrivia node = notVisitingTrivia

    override this.VisitPredefinedType node = text node.Keyword

    override this.VisitPrefixUnaryExpression node =
        text node.OperatorToken <+> this.Visit node.Operand

    override this.VisitPropertyDeclaration node =
        let attrs = this.VisitChunk node.AttributeLists
        let mods = visitModifiers node.Modifiers
        let typ = this.Visit node.Type
        let name =
            let explicitInterface =
                this.Visit node.ExplicitInterfaceSpecifier
            let id = text node.Identifier
            explicitInterface <+> id
        let body =
            // N.B.: This is different from the other "body/expressionbody"
            //       switches because of the weirdness of initialziers and
            //       trailing semicolons.
            match node.AccessorList with
                | null -> space <+> this.Visit node.ExpressionBody
                | _ ->
                    line <+>
                    this.Visit node.AccessorList <+/!+>
                    this.Visit node.Initializer

        breakParent <+>
        group (
            group (
                attrs <+/+>
                group(mods <+/+> group(typ <+/+> name))
            ) <+>
            body <+>
            text node.SemicolonToken
        )

    override this.VisitQualifiedCref node = notVisitingTrivia

    override this.VisitQualifiedName node =
        group (
            this.Visit node.Left <+>
            text node.DotToken <+>
            this.Visit node.Right
        )

    override this.VisitQueryBody node =
        let clauses = node.Clauses |> Seq.map (this.Visit) |> join line
        let selectOrGroup = this.Visit node.SelectOrGroup
        let continuation = this.Visit node.Continuation

        clauses <+/+> (selectOrGroup <++> continuation)

    override this.VisitQueryContinuation node =
        group (text node.IntoKeyword <+/!+> text node.Identifier) <+/+>
        this.Visit node.Body

    override this.VisitQueryExpression node = //#filler
        let from = this.Visit node.FromClause
        let body = this.Visit node.Body

        group (indent(line <+> from <+/+> body))

    override this.VisitReferenceDirectiveTrivia node = notVisitingTrivia

    override this.VisitRefExpression node =
        text node.RefKeyword <++> this.Visit (node.Expression)

    override this.VisitRefType node =
        group (text node.RefKeyword <+/+> text node.ReadOnlyKeyword) <+/+>
        this.Visit node.Type

    override this.VisitRefTypeExpression node =
        text node.Keyword <+>
        bracket
            node.OpenParenToken
            node.CloseParenToken
            (this.Visit node.Expression)

    override this.VisitRefValueExpression node =
        let kw = text node.Keyword
        let op = node.OpenParenToken
        let cp = node.CloseParenToken

        kw <+> bracket op cp (
            this.Visit node.Expression <+>
            text node.Comma <+/+>
            this.Visit node.Type
        )

    override this.VisitRegionDirectiveTrivia node = notVisitingTrivia

    override this.VisitReturnStatement node =
        let expr = this.Visit node.Expression
        breakParent <+>
        group (text node.ReturnKeyword <++> expr <+> text node.SemicolonToken)

    override this.VisitSelectClause node =
        group (
            text node.SelectKeyword <+/!+>
            group (this.Visit node.Expression)
        )

    override this.VisitShebangDirectiveTrivia node = notVisitingTrivia

    override this.VisitSimpleBaseType node = this.Visit node.Type

    override this.VisitSimpleLambdaExpression node =
        let async = text node.AsyncKeyword
        let param = this.Visit node.Parameter
        let arrow = text node.ArrowToken
        let body = this.Visit node.Body
        group (async <+/+> param <+/+> arrow <+/+> body)

    override this.VisitSingleVariableDesignation node =
        text node.Identifier

    override this.VisitSizeOfExpression node =
        text node.Keyword <+>
        bracket node.OpenParenToken node.CloseParenToken (this.Visit node.Type)

    override this.VisitSkippedTokensTrivia node = notVisitingTrivia

    override this.VisitStackAllocArrayCreationExpression node =
        group (
            group(text node.StackAllocKeyword <+/+> this.Visit node.Type) <+>
            this.Visit node.Initializer
        )

    override this.VisitStructDeclaration node =
        let decl = this.VisitTypeDeclaration node
        let members =
            let ob = node.OpenBraceToken
            let cb = node.CloseBraceToken
            this.FoldMembers ob cb node.Members

        group (decl <+/+> members)

    override this.VisitSwitchSection node =
        let labels = node.Labels |> Seq.map (this.Visit) |> join line
        let statements = node.Statements |> Seq.map (this.Visit) |> join line

        labels <+/!+> (statements)

    override this.VisitSwitchStatement node =
        let switch' = text node.SwitchKeyword
        let expr =
            let op = node.OpenParenToken
            let cp = node.CloseParenToken
            bracket op cp (this.Visit node.Expression)

        let sections =
            node.Sections |> Seq.map (this.Visit) |> join (line <+> line)

        breakParent <+>
        group (
            switch' <++>
            group (expr) <+/+>
            text node.OpenBraceToken <+/+>
            sections <+/+>
            text node.CloseBraceToken
        )


    override this.VisitThisExpression node = text node.Token

    override this.VisitThrowExpression node =
        text node.ThrowKeyword <++> this.Visit node.Expression

    override this.VisitThrowStatement node =
        let throw' = text node.ThrowKeyword
        let semicolon = text node.SemicolonToken

        breakParent <+>
        group (
            match node.Expression with
            | null -> throw' <+> semicolon
            | _ -> throw' <+/!+> (this.Visit node.Expression <+> semicolon)
        )

    override this.VisitTryStatement node =
        let try' = text node.TryKeyword
        let block = this.Visit node.Block
        let catches = node.Catches |> Seq.map (this.Visit) |> join line
        let finally' = this.Visit node.Finally

        breakParent <+> group (try' <+/+> block <+/+> catches <+/+> finally')

    override this.VisitTupleElement node =
        group (this.Visit node.Type <+/!+> text node.Identifier)

    override this.VisitTupleExpression node =
        this.VisitParameterOrArgumentList
            node.OpenParenToken
            node.CloseParenToken
            node.Arguments

    override this.VisitTupleType node =
        this.BracketedList
            node.OpenParenToken
            node.CloseParenToken
            node.Elements

    override this.VisitTypeArgumentList node =
        this.BracketedList
            node.LessThanToken
            node.GreaterThanToken
            node.Arguments

    override this.VisitTypeConstraint node = this.Visit node.Type

    override this.VisitTypeCref node = notVisitingTrivia

    override this.VisitTypeOfExpression node =
        text node.Keyword <+>
        bracket node.OpenParenToken node.CloseParenToken (this.Visit node.Type)

    override this.VisitTypeParameter node =
        let attrs = this.VisitChunk node.AttributeLists
        let variance = text node.VarianceKeyword
        let id = text node.Identifier

        group (attrs <+/+> variance <++> id)

    override this.VisitTypeParameterConstraintClause node =
        let where' = text node.WhereKeyword
        let name = this.Visit node.Name
        let colon' = text node.ColonToken
        let constraints = this.VisitList node.Constraints

        group (
            group (where' <++> name <++> colon') <+/!+>
            group (constraints)
        )

    override this.VisitTypeParameterList node =
        this.VisitParameterOrArgumentList node.LessThanToken node.GreaterThanToken node.Parameters

    override this.VisitUndefDirectiveTrivia node = notVisitingTrivia

    override this.VisitUnsafeStatement node =
        breakParent <+> (text node.UnsafeKeyword <+/+> this.Visit node.Block)

    override this.VisitUsingDirective node =
        let using' = text node.UsingKeyword
        let static' = text node.StaticKeyword
        let name = this.Visit node.Name
        let alias = this.Visit node.Alias

        breakParent <+>
        group (
            using' <+/+>
            static' <+/+>
            alias <+/+>
            name <+>
            text node.SemicolonToken
        )

    override this.VisitVariableDeclaration node =
        let firstVar =
            let typ = this.Visit node.Type
            let first = Seq.head node.Variables
            let name = text first.Identifier
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
                    group (
                        group (typ <+/!+> name) <++>
                        text init.EqualsToken <+>
                        line
                    ) <+>
                    initValue
                )

        let restVars = this.VisitList (node.Variables.RemoveAt(0))
        match restVars with
        | NIL -> firstVar
        | _ ->
            let firstComma = text (node.Variables.GetSeparator(0))
            (firstVar <+> firstComma) <+/!+> restVars

    override this.VisitVariableDeclarator node =
        let name = text node.Identifier
        match node.Initializer with
        | null -> name
        | init ->
            group (name <+/+> text init.EqualsToken) <+/!+>
            this.Visit init.Value

    override this.VisitWarningDirectiveTrivia node = notVisitingTrivia

    override this.VisitWhenClause node = //#filler
        let when' = text node.WhenKeyword
        group (when' <+/!+> group (this.Visit node.Condition))

    override this.VisitWhereClause node =
        let where' = text node.WhereKeyword
        group (where' <+/!+> group (this.Visit node.Condition))

    override this.VisitWhileStatement node =
        let while' = text node.WhileKeyword
        let op = node.OpenParenToken
        let cp = node.CloseParenToken

        breakParent <+>
        group (
            while' <++>
            bracket op cp (this.Visit node.Condition) <+>
            this.VisitBody node.Statement
        )

    override this.VisitXmlCDataSection node = notVisitingTrivia

    override this.VisitXmlComment node = notVisitingTrivia

    override this.VisitXmlCrefAttribute node = notVisitingTrivia

    override this.VisitXmlElement node = notVisitingTrivia

    override this.VisitXmlElementEndTag node = notVisitingTrivia

    override this.VisitXmlElementStartTag node = notVisitingTrivia

    override this.VisitXmlEmptyElement node = notVisitingTrivia

    override this.VisitXmlName node = notVisitingTrivia

    override this.VisitXmlNameAttribute node = notVisitingTrivia

    override this.VisitXmlPrefix node = notVisitingTrivia

    override this.VisitXmlProcessingInstruction node = notVisitingTrivia

    override this.VisitXmlText node = notVisitingTrivia

    override this.VisitXmlTextAttribute node = notVisitingTrivia

    override this.VisitYieldStatement node =
        breakParent <+>
        group (
            text node.YieldKeyword <+/!+>
            group (this.Visit node.Expression) <+>
            text node.SemicolonToken
        )


let visit (tree:SyntaxNode) =
    let visitor = PrintVisitor()
    visitor.Visit(tree)
