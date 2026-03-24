/// Core T-SQL formatter: parses SQL via ScriptDOM, transforms AST to Doc, renders to string.
module TSqlFormatter.Formatter

open System
open System.IO
open System.Collections.Generic
open Microsoft.SqlServer.TransactSql.ScriptDom
open TSqlFormatter.Doc
open TSqlFormatter.Trivia
open TSqlFormatter.Config
open TSqlFormatter.Keywords

// ─── Helpers ───

let private keyword (cfg: FormattingStyle) (s: string) = text (caseKeyword cfg.casing s)
let private functionName (cfg: FormattingStyle) (s: string) = text (caseFunction cfg.casing s)
let private dataType (cfg: FormattingStyle) (s: string) = text (caseDataType cfg.casing s)
let private indentWidth (cfg: FormattingStyle) = cfg.whitespace.numberOfSpacesInTabs

type private SingleLineMeasure = { length: int; hasComments: bool }

let private fragmentSingleLineMeasure (frag: TSqlFragment) : SingleLineMeasure =
    if frag = null || frag.ScriptTokenStream = null then
        { length = 0; hasComments = false }
    else
        let mutable length = 0
        let mutable hasComments = false
        let mutable pendingSpace = false
        let mutable emittedToken = false

        for i = frag.FirstTokenIndex to frag.LastTokenIndex do
            let tok = frag.ScriptTokenStream.[i]

            match tok.TokenType with
            | TSqlTokenType.WhiteSpace ->
                if emittedToken then
                    pendingSpace <- true
            | TSqlTokenType.SingleLineComment
            | TSqlTokenType.MultilineComment -> hasComments <- true
            | TSqlTokenType.EndOfFile -> ()
            | _ ->
                if pendingSpace then
                    length <- length + 1
                    pendingSpace <- false

                length <- length + tok.Text.Length
                emittedToken <- true

        { length = length
          hasComments = hasComments }

let private canCollapseFragment (enabled: bool) (maxLength: int) (frag: TSqlFragment) =
    if not enabled then
        false
    else
        let measure = fragmentSingleLineMeasure frag
        not measure.hasComments && measure.length <= maxLength

let private fragmentHasComments (frag: TSqlFragment) =
    fragmentSingleLineMeasure frag |> fun measure -> measure.hasComments

let private canCollapseFragments (enabled: bool) (maxLength: int) (fragments: TSqlFragment seq) =
    if not enabled then
        false
    else
        let measures = fragments |> Seq.map fragmentSingleLineMeasure |> Seq.toList
        let hasComments = measures |> List.exists (fun m -> m.hasComments)
        let separatorsLength = max 0 ((List.length measures - 1) * 2)
        let totalLength = measures |> List.sumBy (fun m -> m.length) |> (+) separatorsLength
        not hasComments && totalLength <= maxLength

let private canCollapseList (cfg: FormattingStyle) (items: Doc list) =
    match items with
    | []
    | [ _ ] -> false
    | _ ->
        match cfg.lists.placeSubsequentItemsOnNewLines with
        | PlaceOnNewLine.Never -> true
        | PlaceOnNewLine.Always -> false
        | _ -> false

// Lists, boolean chains, and CASE bodies all share the same broad continuation shape:
// a construct keyword followed by one or more continuation items.
//
// ToConstruct:
//   WHERE a = 1
//   AND b = 2
//
// Indented:
//   WHERE a = 1
//       AND b = 2
//
// ToFirstItem:
//   WHERE a = 1
//         AND b = 2
//
// We intentionally do not support right-aligned variants such as:
//   WHERE a = 1
//     AND b = 2
// where the right edge of AND aligns to the right edge of WHERE. The current
// Doc model handles fixed indentation well via Nest, but it does not express
// right-edge anchoring cleanly.
type SequencePolicy =
    { placeFirstItemOnNewLine: bool
      firstItemIndent: int option
      subsequentItemsIndent: int option }

// sequenceDoc:
//   a = 1
//   AND b = 2
//   AND c = 3
let sequenceDoc (policy: SequencePolicy) (items: Doc list) : Doc =
    let nestIfIndented indent doc =
        match indent with
        | Some spaces -> nest spaces doc
        | None -> doc

    let subsequentIndent = defaultArg policy.subsequentItemsIndent 0

    match items with
    | [] -> empty
    | [ singleItem ] -> nestIfIndented policy.firstItemIndent singleItem
    | firstItem :: remainingItems ->
        let firstItemDoc = nestIfIndented policy.firstItemIndent firstItem
        let remainingItemsDoc = join line remainingItems
        firstItemDoc <+> nest subsequentIndent (line <+> remainingItemsDoc)

// headedSequenceDoc:
//   SELECT a,
//       b,
//       c
//
//   CASE
//       WHEN x = 1 THEN 'a'
//       ELSE 'b'
//   END
let headedSequenceDoc (policy: SequencePolicy) (headDoc: Doc) (items: Doc list) : Doc =
    let subsequentIndent = defaultArg policy.subsequentItemsIndent 0

    match items with
    | [] -> headDoc
    | _ when policy.placeFirstItemOnNewLine -> headDoc <+> nest subsequentIndent (line <+> join line items)
    | _ -> headDoc <++> sequenceDoc policy items

let listSequencePolicy (cfg: FormattingStyle) =
    let indent = indentWidth cfg

    { placeFirstItemOnNewLine = cfg.lists.placeFirstItemOnNewLine = PlaceOnNewLine.Always
      firstItemIndent = if cfg.lists.indentListItems then Some indent else None
      subsequentItemsIndent = Some indent }

let andOrSequencePolicy (cfg: FormattingStyle) =
    let indent = indentWidth cfg

    { placeFirstItemOnNewLine = false
      firstItemIndent = None
      subsequentItemsIndent =
        match cfg.operators.andOr.alignment with
        | Alignment.Indented -> Some indent
        | _ -> None }

let withFirstItemIndent (indent: int) (policy: SequencePolicy) =
    { policy with
        firstItemIndent = if indent > 0 then Some indent else None }

let private renderComment (comment: Comment) : Doc =
    match comment with
    | SingleLineComment commentText -> text commentText
    | MultilineComment commentText -> text commentText

type private CommaListItem =
    { Doc: Doc
      ForcesExpandedLayout: bool
      AfterCommaComments: Comment list }

let private afterCommaComments (leftFrag: TSqlFragment) (rightFrag: TSqlFragment) : Comment list =
    let _, afterCommaComments, _ = splitCommaInterComments leftFrag rightFrag

    afterCommaComments

let private plainCommaListItems (items: Doc list) : CommaListItem list =
    items
    |> List.map (fun doc ->
        { Doc = doc
          ForcesExpandedLayout = false
          AfterCommaComments = [] })

let private decoratedCommaItemDoc (isLast: bool) (item: CommaListItem) : Doc =
    if isLast then
        item.Doc
    else
        match item.AfterCommaComments with
        | [] -> item.Doc <+> text ","
        | comments -> item.Doc <+> text ", " <+> join (text " ") (comments |> List.map renderComment)

let private expandedCommaListDoc (cfg: FormattingStyle) (keyword: Doc) (items: CommaListItem list) : Doc =
    let decoratedItems =
        items
        |> List.mapi (fun i item -> decoratedCommaItemDoc (i = List.length items - 1) item)

    headedSequenceDoc (listSequencePolicy cfg) keyword decoratedItems

let private flatCommaListDoc (keyword: Doc) (items: CommaListItem list) : Doc =
    let decoratedItems =
        items
        |> List.mapi (fun i item -> decoratedCommaItemDoc (i = List.length items - 1) item)

    keyword <++> join (text " ") decoratedItems |> flatten

let private formatCommaList (cfg: FormattingStyle) (keyword: Doc) (items: CommaListItem list) : Doc =
    match items with
    | [] -> keyword
    | [ single ] when not cfg.lists.indentListItems -> keyword <++> single.Doc
    | _ when
        not (items |> List.exists _.ForcesExpandedLayout)
        && canCollapseList cfg (items |> List.map _.Doc)
        ->
        let flatDoc = flatCommaListDoc keyword items
        let expandedDoc = expandedCommaListDoc cfg keyword items
        TSqlFormatter.Doc.Doc.Union(flatDoc, expandedDoc)
    | _ -> expandedCommaListDoc cfg keyword items

let private formatList (cfg: FormattingStyle) (keyword: Doc) (items: Doc list) : Doc =
    formatCommaList cfg keyword (plainCommaListItems items)

/// Get the raw SQL text of a fragment from its token stream.
let private fragmentText (frag: TSqlFragment) : string =
    if frag = null then
        ""
    else
        let stream = frag.ScriptTokenStream

        if stream = null then
            ""
        else
            let sb = System.Text.StringBuilder()

            for i = frag.FirstTokenIndex to frag.LastTokenIndex do
                sb.Append(stream.[i].Text) |> ignore

            sb.ToString().Trim()

/// Collect comments between two token indices from the token stream.
let private collectComments (stream: IList<TSqlParserToken>) (fromIdx: int) (toIdx: int) : string list =
    if stream = null then
        []
    else
        [ for i in fromIdx..toIdx do
              let tok = stream.[i]

              if
                  tok.TokenType = TSqlTokenType.SingleLineComment
                  || tok.TokenType = TSqlTokenType.MultilineComment
              then
                  yield tok.Text.Trim() ]

/// Emit a fragment's tokens as raw text (fallback for unhandled node types).
let private tokenStreamDoc (cfg: FormattingStyle) (frag: TSqlFragment) : Doc =
    if frag = null then
        empty
    else
        let stream = frag.ScriptTokenStream

        if stream = null then
            text (fragmentText frag)
        else
            let result, _ =
                seq { frag.FirstTokenIndex .. frag.LastTokenIndex }
                |> Seq.map (fun i -> stream.[i])
                |> Seq.fold
                    (fun (sb: System.Text.StringBuilder, prevWs) tok ->
                        match tok.TokenType with
                        | TSqlTokenType.WhiteSpace ->
                            if not prevWs then
                                sb.Append(' ') |> ignore

                            (sb, true)
                        | TSqlTokenType.EndOfFile -> (sb, prevWs)
                        | TSqlTokenType.SingleLineComment ->
                            sb.Append(tok.Text.TrimEnd()) |> ignore
                            (sb, false)
                        | TSqlTokenType.MultilineComment ->
                            sb.Append(tok.Text) |> ignore
                            (sb, false)
                        | _ ->
                            sb.Append(caseToken cfg.casing tok.Text) |> ignore
                            (sb, false))
                    (System.Text.StringBuilder(), false)

            text (result.ToString().Trim())

/// Attach trailing comment on the same line.
let private trailingComment (frag: TSqlFragment) : Doc =
    if frag = null then
        empty
    else
        let stream = frag.ScriptTokenStream

        if stream = null then
            empty
        else
            let rec scan idx =
                if idx >= stream.Count then
                    empty
                else
                    let tok = stream.[idx]

                    match tok.TokenType with
                    | TSqlTokenType.WhiteSpace ->
                        if tok.Text.Contains('\n') || tok.Text.Contains('\r') then
                            empty
                        else
                            scan (idx + 1)
                    | TSqlTokenType.SingleLineComment -> text " " <+> text (tok.Text.TrimEnd())
                    | TSqlTokenType.MultilineComment -> text " " <+> text tok.Text
                    | _ -> empty

            scan (frag.LastTokenIndex + 1)

let private withTrailingComment (frag: TSqlFragment) (doc: Doc) : Doc = doc <+> trailingComment frag

let private keywordWithTrailingCommentDoc
    (cfg: FormattingStyle)
    (keywordText: string)
    (tokenIndex: int option)
    (stmt: TSqlStatement)
    : Doc =
    let stream = stmt.ScriptTokenStream

    if stream = null then
        keyword cfg keywordText
    else
        match tokenIndex with
        | Some idx -> keyword cfg keywordText <+> trailingCommentAfterTokenIndex stream idx
        | None -> keyword cfg keywordText

let private trailingCommentForToken (tokenIndex: int option) (stmt: TSqlStatement) : Comment option =
    tokenIndex |> Option.bind (trailingTriviaAfterTokenIndex stmt.ScriptTokenStream)

let private returnsKeywordTokenIndex (stmt: TSqlStatement) = tokenIndexOfIdentifier stmt "RETURNS"

let private returnsKeyword (cfg: FormattingStyle) (stmt: TSqlStatement) : Comment option * Doc =
    let comment = trailingCommentForToken (returnsKeywordTokenIndex stmt) stmt

    let doc =
        keywordWithTrailingCommentDoc cfg "RETURNS" (returnsKeywordTokenIndex stmt) stmt

    comment, doc

let private returnKeywordTokenIndex (stmt: TSqlStatement) =
    tokenIndexOfType stmt TSqlTokenType.Return

let private returnKeyword (cfg: FormattingStyle) (stmt: TSqlStatement) : Comment option * Doc =
    let comment = trailingCommentForToken (returnKeywordTokenIndex stmt) stmt

    let doc =
        keywordWithTrailingCommentDoc cfg "RETURN" (returnKeywordTokenIndex stmt) stmt

    comment, doc

type private TrailingFragmentDoc = { Comment: Comment option; Doc: Doc }

// StatementContext controls semicolon behavior for a particular statement occurrence:
// whether it carries a trailing semicolon and whether a top-level CTE starts with ';WITH'
// or plain 'WITH'. We emit ';WITH' for standalone statements because the semicolon
// terminates the previous statement, not the CTE itself. See:
// https://learn.microsoft.com/en-us/sql/t-sql/queries/with-common-table-expression-transact-sql
type private StatementContext =
    { HasTrailingSemicolon: bool
      HasLeadingCteSemicolon: bool }

// Standalone statements own both trailing ';' and leading ';WITH'.
let private standaloneStatementContext =
    { HasTrailingSemicolon = true
      HasLeadingCteSemicolon = true }

// Embedded statements rely on the enclosing construct for those semicolons.
let private embeddedStatementContext =
    { HasTrailingSemicolon = false
      HasLeadingCteSemicolon = false }

let private trailingFragmentDoc (frag: TSqlFragment) (doc: Doc) : TrailingFragmentDoc =
    { Comment = trailingTriviaAfterFragment frag
      Doc = doc <+> trailingCommentAfterFragment frag }

let private appendInlineComments (doc: Doc) (comments: Comment list) : Doc =
    match comments with
    | [] -> doc
    | _ -> doc <+> text " " <+> join (text " ") (comments |> List.map renderComment)

let private attachOwnLineComments (comments: Comment list) (doc: Doc) : Doc =
    match comments with
    | [] -> doc
    | _ -> join line (comments |> List.map renderComment) <+> line <+> doc

let private attachInlineLeadingComments (comments: Comment list) (doc: Doc) : Doc =
    match comments with
    | [] -> doc
    | _ -> join (text " ") (comments |> List.map renderComment) <++> doc

let private renderItemsWithLeadingComments<'T when 'T :> TSqlFragment>
    (render: 'T -> Doc)
    (leadingComments: 'T option -> 'T -> Comment list)
    (attachComments: Comment list -> Doc -> Doc)
    (items: 'T list)
    : Doc list =
    items
    |> List.mapFold
        (fun prev item ->
            let doc = render item |> attachComments (leadingComments prev item)
            doc, Some item)
        None
    |> fst

// ─── Expression formatting ───

let rec private exprDoc (cfg: FormattingStyle) (expr: TSqlFragment) : Doc =
    match expr with
    | :? ColumnReferenceExpression as col -> columnRefDoc cfg col
    | :? IntegerLiteral as lit -> text lit.Value
    | :? StringLiteral as lit -> text ("'" + lit.Value.Replace("'", "''") + "'")
    | :? NumericLiteral as lit -> text lit.Value
    | :? RealLiteral as lit -> text lit.Value
    | :? NullLiteral -> keyword cfg "NULL"
    | :? MoneyLiteral as lit -> text lit.Value
    | :? BinaryLiteral as lit -> text lit.Value
    | :? MaxLiteral -> keyword cfg "MAX"
    | :? DefaultLiteral -> keyword cfg "DEFAULT"
    | :? IdentifierLiteral as lit -> text lit.Value
    | :? GlobalVariableExpression as gv -> text (applyCase cfg.casing.globalVariables gv.Name)
    | :? VariableReference as v -> text v.Name
    | :? ParenthesisExpression as p -> parenExprDoc cfg p
    | :? ScalarSubquery as sq -> scalarSubqueryDoc cfg sq
    | :? SearchedCaseExpression as c -> searchedCaseDoc cfg c
    | :? SimpleCaseExpression as c -> simpleCaseDoc cfg c
    | :? CastCall as c -> castCallDoc cfg c
    | :? TryCastCall as c -> tryCastCallDoc cfg c
    | :? ConvertCall as c -> convertCallDoc cfg c
    | :? FunctionCall as f -> functionCallDoc cfg f
    | :? CoalesceExpression as c -> coalesceDoc cfg c
    | :? IIfCall as iif -> iifCallDoc cfg iif
    | :? NullIfExpression as n -> nullIfDoc cfg n
    | :? BooleanParenthesisExpression as bp -> boolParenDoc cfg bp
    | :? BooleanComparisonExpression as bc -> boolCompDoc cfg bc
    | :? BooleanBinaryExpression as bb -> boolExprDoc cfg bb
    | :? BooleanNotExpression as bn -> keyword cfg "NOT" <++> boolExprDoc cfg bn.Expression
    | :? BooleanIsNullExpression as bisn ->
        let e = exprDoc cfg bisn.Expression

        if bisn.IsNot then
            e <++> keyword cfg "IS" <++> keyword cfg "NOT" <++> keyword cfg "NULL"
        else
            e <++> keyword cfg "IS" <++> keyword cfg "NULL"
    | :? InPredicate as inp -> inPredicateDoc cfg inp
    | :? LikePredicate as lk -> likePredicateDoc cfg lk
    | :? ExistsPredicate as ep ->
        keyword cfg "EXISTS"
        <++> blockParenthesizedQueryDoc
            cfg
            true
            ep.Subquery.QueryExpression
            (queryExprDoc cfg ep.Subquery.QueryExpression)
    | :? BooleanTernaryExpression as be -> betweenDoc cfg be
    | :? BinaryExpression as binex -> binaryExprDoc cfg binex
    | :? UnaryExpression as unex ->
        match unex.UnaryExpressionType with
        | UnaryExpressionType.Negative -> text "-" <+> exprDoc cfg unex.Expression
        | UnaryExpressionType.Positive -> text "+" <+> exprDoc cfg unex.Expression
        | _ -> tokenStreamDoc cfg expr
    | :? MultiPartIdentifier as mpi -> multiPartIdDoc cfg mpi
    | :? SelectStarExpression as star -> selectStarDoc cfg star
    | :? SelectScalarExpression as sse -> selectScalarDoc cfg sse
    | :? SelectSetVariable as ssv -> selectSetVarDoc cfg ssv
    | :? WhenClause -> tokenStreamDoc cfg expr
    | :? TopRowFilter as top -> topDoc cfg top
    | :? OverClause as oc -> overClauseDoc cfg oc
    | :? WindowFrameClause -> tokenStreamDoc cfg expr
    | :? IdentifierOrValueExpression as iov ->
        if iov.Identifier <> null then
            identDoc iov.Identifier
        else
            text ("'" + iov.Value.Replace("'", "''") + "'")
    | _ -> tokenStreamDoc cfg expr

and private boolExprDoc (cfg: FormattingStyle) (expr: BooleanExpression) : Doc =
    match expr with
    | :? BooleanBinaryExpression as bb -> boolBinaryExprDoc cfg false bb
    | _ -> exprDoc cfg expr

and private boolBinaryExprDoc (cfg: FormattingStyle) (afterHead: bool) (bb: BooleanBinaryExpression) : Doc =
    let policy =
        if afterHead then
            andOrSequencePolicy cfg |> withFirstItemIndent (indentWidth cfg)
        else
            andOrSequencePolicy cfg

    sequenceDoc policy (booleanSequenceItems cfg bb)

and private headedConditionDoc (cfg: FormattingStyle) (head: Doc) (expr: BooleanExpression) : Doc =
    let body =
        match expr with
        | :? BooleanBinaryExpression as bb -> boolBinaryExprDoc cfg true bb
        | _ -> boolExprDoc cfg expr

    head <++> body

and private columnRefDoc (_cfg: FormattingStyle) (col: ColumnReferenceExpression) : Doc =
    if col.MultiPartIdentifier <> null then
        multiPartIdDoc _cfg col.MultiPartIdentifier
    else
        text (fragmentText col)

and private multiPartIdDoc (_cfg: FormattingStyle) (mpi: MultiPartIdentifier) : Doc =
    let parts = mpi.Identifiers |> Seq.map (fun i -> identDoc i) |> Seq.toList
    join (text ".") parts

and private identDoc (ident: Identifier) : Doc =
    match ident.QuoteType with
    | QuoteType.SquareBracket -> text ("[" + ident.Value + "]")
    | QuoteType.DoubleQuote -> text ("\"" + ident.Value + "\"")
    | _ -> text ident.Value

and private identOrValueDoc (iov: IdentifierOrValueExpression) : Doc =
    if iov.Identifier <> null then
        identDoc iov.Identifier
    else
        text ("'" + iov.Value.Replace("'", "''") + "'")

and private selectStarDoc (_cfg: FormattingStyle) (star: SelectStarExpression) : Doc =
    if star.Qualifier <> null then
        multiPartIdDoc _cfg star.Qualifier <+> text ".*"
    else
        text "*"

and private selectScalarDoc (cfg: FormattingStyle) (sse: SelectScalarExpression) : Doc =
    if fragmentHasComments sse then
        tokenStreamDoc cfg sse
    else
        let e = exprDoc cfg sse.Expression

        match sse.ColumnName with
        | null -> e
        | alias ->
            let aliasDoc = identOrValueDoc alias
            // Check if AS keyword was present in original source by scanning tokens
            let hasAs =
                let stream = sse.ScriptTokenStream

                if stream <> null then
                    seq { sse.Expression.LastTokenIndex + 1 .. alias.FirstTokenIndex - 1 }
                    |> Seq.exists (fun i -> stream.[i].TokenType = TSqlTokenType.As)
                else
                    true // default to having AS

            if hasAs then
                e <++> keyword cfg "AS" <++> aliasDoc
            else
                e <++> aliasDoc

and private selectSetVarDoc (cfg: FormattingStyle) (ssv: SelectSetVariable) : Doc =
    text ssv.Variable.Name <++> text "=" <++> exprDoc cfg ssv.Expression

and private topDoc (cfg: FormattingStyle) (top: TopRowFilter) : Doc =
    // TOP always adds parens, so unwrap ParenthesisExpression to avoid double parens
    let inner =
        match top.Expression with
        | :? ParenthesisExpression as pe -> pe.Expression
        | e -> e

    let e = exprDoc cfg inner
    let d = keyword cfg "TOP" <++> text "(" <+> e <+> text ")"

    if top.Percent then
        d <++> keyword cfg "PERCENT"
    elif top.WithTies then
        d <++> keyword cfg "WITH" <++> keyword cfg "TIES"
    else
        d

// ─── CASE expressions ───

and private collapseIfShort
    (enabled: bool)
    (maxLength: int)
    (frag: TSqlFragment)
    (flatDoc: Doc)
    (expandedDoc: Doc)
    : Doc =
    if canCollapseFragment enabled maxLength frag then
        flatDoc
    else
        expandedDoc

and private caseBodyLines (elseDoc: Doc option) (whenDocs: Doc list) =
    [ yield! whenDocs
      match elseDoc with
      | Some doc -> yield doc
      | None -> () ]

and private expandedCaseFromParts
    (cfg: FormattingStyle)
    (caseHead: Doc)
    (whenDocs: Doc list)
    (elseDoc: Doc option)
    : Doc =
    let bodyItems = caseBodyLines elseDoc whenDocs

    let whenPolicy =
        let indent = indentWidth cfg

        { placeFirstItemOnNewLine = cfg.caseExpressions.placeFirstWhenOnNewLine = PlaceOnNewLine.Always
          firstItemIndent = None
          subsequentItemsIndent =
            match cfg.caseExpressions.whenAlignment with
            | WhenAlignment.IndentedFromCase -> Some indent
            | _ -> None }

    headedSequenceDoc whenPolicy caseHead bodyItems <+> line <+> keyword cfg "END"

and private searchedCaseDoc (cfg: FormattingStyle) (c: SearchedCaseExpression) : Doc =
    let whenDocs =
        [ for wc in c.WhenClauses do
              yield
                  keyword cfg "WHEN"
                  <++> boolExprDoc cfg wc.WhenExpression
                  <++> keyword cfg "THEN"
                  <++> exprDoc cfg wc.ThenExpression ]

    let elseDoc =
        if c.ElseExpression <> null then
            Some(keyword cfg "ELSE" <++> exprDoc cfg c.ElseExpression)
        else
            None

    let flatParts =
        [ yield keyword cfg "CASE"
          yield! whenDocs
          match elseDoc with
          | Some doc -> yield doc
          | None -> ()
          yield keyword cfg "END" ]

    let flatDoc = hcat flatParts
    let expandedDoc = expandedCaseFromParts cfg (keyword cfg "CASE") whenDocs elseDoc

    collapseIfShort
        cfg.caseExpressions.collapseShortCaseExpressions
        cfg.caseExpressions.collapseCaseExpressionsShorterThan
        c
        flatDoc
        expandedDoc

and private simpleCaseDoc (cfg: FormattingStyle) (c: SimpleCaseExpression) : Doc =
    let inputExpr = exprDoc cfg c.InputExpression

    let whenDocs =
        [ for wc in c.WhenClauses do
              yield
                  keyword cfg "WHEN"
                  <++> exprDoc cfg wc.WhenExpression
                  <++> keyword cfg "THEN"
                  <++> exprDoc cfg wc.ThenExpression ]

    let elseDoc =
        if c.ElseExpression <> null then
            Some(keyword cfg "ELSE" <++> exprDoc cfg c.ElseExpression)
        else
            None

    let flatParts =
        [ yield keyword cfg "CASE" <++> inputExpr
          yield! whenDocs
          match elseDoc with
          | Some doc -> yield doc
          | None -> ()
          yield keyword cfg "END" ]

    let flatDoc = hcat flatParts

    let expandedDoc =
        expandedCaseFromParts cfg (keyword cfg "CASE" <++> inputExpr) whenDocs elseDoc

    collapseIfShort
        cfg.caseExpressions.collapseShortCaseExpressions
        cfg.caseExpressions.collapseCaseExpressionsShorterThan
        c
        flatDoc
        expandedDoc

// ─── CAST / CONVERT ───

and private castCallDoc (cfg: FormattingStyle) (c: CastCall) : Doc =
    let dataTypeDoc = dataTypeRefDoc cfg c.DataType

    functionName cfg "CAST" <+> text "(" <+> exprDoc cfg c.Parameter
    <++> keyword cfg "AS"
    <++> dataTypeDoc
    <+> text ")"

and private tryCastCallDoc (cfg: FormattingStyle) (c: TryCastCall) : Doc =
    let dataTypeDoc = dataTypeRefDoc cfg c.DataType

    functionName cfg "TRY_CAST" <+> text "(" <+> exprDoc cfg c.Parameter
    <++> keyword cfg "AS"
    <++> dataTypeDoc
    <+> text ")"

and private convertCallDoc (cfg: FormattingStyle) (c: ConvertCall) : Doc =
    let dataTypeDoc = dataTypeRefDoc cfg c.DataType

    let args =
        if c.Style <> null then
            dataTypeDoc <+> text "," <++> exprDoc cfg c.Parameter <+> text ","
            <++> exprDoc cfg c.Style
        else
            dataTypeDoc <+> text "," <++> exprDoc cfg c.Parameter

    functionName cfg "CONVERT" <+> text "(" <+> args <+> text ")"

and private dataTypeRefDoc (cfg: FormattingStyle) (dtr: DataTypeReference) : Doc =
    match dtr with
    | :? SqlDataTypeReference as sdt ->
        let name = sdt.SqlDataTypeOption.ToString().ToUpperInvariant()
        let typeName = caseDataType cfg.casing name

        if sdt.Parameters.Count > 0 then
            let parms = sdt.Parameters |> Seq.map (fun p -> exprDoc cfg p) |> Seq.toList
            text typeName <+> text "(" <+> join (text ", ") parms <+> text ")"
        else
            text typeName
    | :? UserDataTypeReference as udt -> multiPartIdDoc cfg udt.Name
    | _ -> tokenStreamDoc cfg dtr

// ─── Function calls ───

and private functionCallDoc (cfg: FormattingStyle) (f: FunctionCall) : Doc =
    let name =
        if f.FunctionName <> null then
            let n = f.FunctionName.Value
            if isBuiltInFunction n then caseFunction cfg.casing n else n
        else
            ""

    let callName =
        if f.CallTarget <> null then
            tokenStreamDoc cfg f.CallTarget <+> text "." <+> text name
        else
            text name

    let argsDoc =
        if f.Parameters <> null && f.Parameters.Count > 0 then
            let argDocs = f.Parameters |> Seq.map (fun a -> exprDoc cfg a) |> Seq.toList
            let uniqueStyle = f.UniqueRowFilter

            let prefix =
                if uniqueStyle = UniqueRowFilter.Distinct then
                    keyword cfg "DISTINCT" <++> empty
                elif uniqueStyle = UniqueRowFilter.All then
                    keyword cfg "ALL" <++> empty
                else
                    empty

            let flatArgs = text "(" <+> prefix <+> commaSep argDocs <+> text ")"

            let expandedArgs =
                text "("
                <+> nest (indentWidth cfg) (line <+> prefix <+> join (text "," <+> line) argDocs)
                <+> line
                <+> text ")"

            TSqlFormatter.Doc.Doc.Union(flatArgs, expandedArgs)
        else
            text "()"

    let overDoc =
        if f.OverClause <> null then
            text " " <+> overClauseDoc cfg f.OverClause
        else
            empty

    callName <+> argsDoc <+> overDoc

and private overClauseDoc (cfg: FormattingStyle) (oc: OverClause) : Doc =
    let parts =
        [ if oc.Partitions <> null && oc.Partitions.Count > 0 then
              let partDocs = oc.Partitions |> Seq.map (fun p -> exprDoc cfg p) |> Seq.toList
              yield keyword cfg "PARTITION" <++> keyword cfg "BY" <++> commaSep partDocs
          if
              oc.OrderByClause <> null
              && oc.OrderByClause.OrderByElements <> null
              && oc.OrderByClause.OrderByElements.Count > 0
          then
              let orderDocs =
                  oc.OrderByClause.OrderByElements
                  |> Seq.map (fun o -> orderByElemDoc cfg o)
                  |> Seq.toList

              yield keyword cfg "ORDER" <++> keyword cfg "BY" <++> commaSep orderDocs
          if oc.WindowFrameClause <> null then
              // ScriptDOM's WindowFrameClause.LastTokenIndex may not include
              // the trailing ROW keyword in "CURRENT ROW". Extend the range
              // to pick it up from the token stream.
              let wf = oc.WindowFrameClause
              let stream = wf.ScriptTokenStream

              let lastIdx =
                  if stream <> null then
                      let mutable idx = wf.LastTokenIndex + 1
                      // skip whitespace
                      while idx < stream.Count && stream.[idx].TokenType = TSqlTokenType.WhiteSpace do
                          idx <- idx + 1

                      if
                          idx < stream.Count
                          && stream.[idx].TokenType = TSqlTokenType.Identifier
                          && stream.[idx].Text.Equals("ROW", System.StringComparison.OrdinalIgnoreCase)
                      then
                          idx
                      else
                          wf.LastTokenIndex
                  else
                      wf.LastTokenIndex

              let tokens =
                  [ for i in wf.FirstTokenIndex .. lastIdx do
                        yield stream.[i] ]

              let sb = System.Text.StringBuilder()
              let mutable prevWs = false

              for tok in tokens do
                  match tok.TokenType with
                  | TSqlTokenType.WhiteSpace ->
                      if not prevWs then
                          sb.Append(' ') |> ignore

                      prevWs <- true
                  | TSqlTokenType.EndOfFile -> ()
                  | _ ->
                      sb.Append(caseToken cfg.casing tok.Text) |> ignore
                      prevWs <- false

              yield text (sb.ToString().Trim()) ]

    keyword cfg "OVER" <++> text "(" <+> join (text " ") parts <+> text ")"

and private coalesceDoc (cfg: FormattingStyle) (c: CoalesceExpression) : Doc =
    let argDocs = c.Expressions |> Seq.map (fun e -> exprDoc cfg e) |> Seq.toList
    let flatArgs = text "(" <+> commaSep argDocs <+> text ")"

    let expandedArgs =
        text "("
        <+> nest (indentWidth cfg) (line <+> join (text "," <+> line) argDocs)
        <+> line
        <+> text ")"

    functionName cfg "COALESCE"
    <+> TSqlFormatter.Doc.Doc.Union(flatArgs, expandedArgs)

and private iifCallDoc (cfg: FormattingStyle) (iif: IIfCall) : Doc =
    functionName cfg "IIF"
    <+> text "("
    <+> boolExprDoc cfg iif.Predicate
    <+> text ","
    <++> exprDoc cfg iif.ThenExpression
    <+> text ","
    <++> exprDoc cfg iif.ElseExpression
    <+> text ")"

and private nullIfDoc (cfg: FormattingStyle) (n: NullIfExpression) : Doc =
    functionName cfg "NULLIF"
    <+> text "("
    <+> exprDoc cfg n.FirstExpression
    <+> text ","
    <++> exprDoc cfg n.SecondExpression
    <+> text ")"

// ─── Boolean expressions ───

and private boolCompDoc (cfg: FormattingStyle) (bc: BooleanComparisonExpression) : Doc =
    if fragmentHasComments bc then
        tokenStreamDoc cfg bc
    else
        let op =
            match bc.ComparisonType with
            | BooleanComparisonType.Equals -> "="
            | BooleanComparisonType.NotEqualToBrackets -> "<>"
            | BooleanComparisonType.NotEqualToExclamation -> "!="
            | BooleanComparisonType.GreaterThan -> ">"
            | BooleanComparisonType.GreaterThanOrEqualTo -> ">="
            | BooleanComparisonType.LessThan -> "<"
            | BooleanComparisonType.LessThanOrEqualTo -> "<="
            | BooleanComparisonType.NotGreaterThan -> "!>"
            | BooleanComparisonType.NotLessThan -> "!<"
            | _ -> "="

        let lhs = exprDoc cfg bc.FirstExpression
        let rhs = exprDoc cfg bc.SecondExpression

        if cfg.operators.comparison.addSpacesAround then
            lhs <++> text op <++> rhs
        else
            lhs <+> text op <+> rhs

and private boolBinaryDoc (cfg: FormattingStyle) (bb: BooleanBinaryExpression) : Doc =
    let opText =
        match bb.BinaryExpressionType with
        | BooleanBinaryExpressionType.And -> "AND"
        | BooleanBinaryExpressionType.Or -> "OR"
        | _ -> "AND"

    let lhs = exprDoc cfg bb.FirstExpression
    let rhs = exprDoc cfg bb.SecondExpression
    lhs <+> line <+> nest (indentWidth cfg) (keyword cfg opText <++> rhs)

and private boolParenDoc (cfg: FormattingStyle) (bp: BooleanParenthesisExpression) : Doc =
    let inner = boolExprDoc cfg bp.Expression
    let flatDoc = text "(" <+> flatten inner <+> text ")"

    let expandedDoc =
        text "(" <+> nest (indentWidth cfg) (line <+> inner) <+> line <+> text ")"

    TSqlFormatter.Doc.Doc.Union(flatDoc, expandedDoc)

and private queryParensDoc
    (cfg: FormattingStyle)
    (allowCollapse: bool)
    (queryExpr: QueryExpression)
    (expandedDoc: Doc)
    (inner: Doc)
    : Doc =
    let flatDoc = text "(" <+> flatten inner <+> text ")"

    if
        allowCollapse
        && canCollapseFragment cfg.dml.collapseShortSubqueries cfg.dml.collapseSubqueriesShorterThan queryExpr
    then
        flatDoc
    else
        expandedDoc

and private blockParenthesizedQueryDoc
    (cfg: FormattingStyle)
    (allowCollapse: bool)
    (queryExpr: QueryExpression)
    (inner: Doc)
    : Doc =
    let expandedDoc =
        text "(" <+> nest (indentWidth cfg) (line <+> inner) <+> line <+> text ")"

    queryParensDoc cfg allowCollapse queryExpr expandedDoc inner

and private inlineParenthesizedQueryDoc
    (cfg: FormattingStyle)
    (allowCollapse: bool)
    (queryExpr: QueryExpression)
    (inner: Doc)
    : Doc =
    let expandedDoc = text "(" <+> inner <+> text ")"
    queryParensDoc cfg allowCollapse queryExpr expandedDoc inner

and private inPredicateDoc (cfg: FormattingStyle) (inp: InPredicate) : Doc =
    let lhs = exprDoc cfg inp.Expression

    let notPart =
        if inp.NotDefined then
            keyword cfg "NOT" <++> empty
        else
            empty

    let inKw = keyword cfg "IN"

    if inp.Subquery <> null then
        lhs <++> notPart <+> inKw
        <++> blockParenthesizedQueryDoc
            cfg
            true
            inp.Subquery.QueryExpression
            (queryExprDoc cfg inp.Subquery.QueryExpression)
    else
        let valDocs = inp.Values |> Seq.map (fun v -> exprDoc cfg v) |> Seq.toList
        // Use Union to allow the renderer to collapse onto one line when it fits.
        // The flat version has no spaces inside parens: IN (val1, val2)
        // The expanded version has each value on a new indented line.
        let flatContent =
            flatten (
                lhs <++> notPart <+> inKw <++> text "("
                <+> join (text ", ") valDocs
                <+> text ")"
            )

        let expandedContent =
            let valSep = text "," <+> line
            let valsDoc = join valSep valDocs

            lhs <++> notPart <+> inKw <++> text "("
            <+> nest (indentWidth cfg) (line <+> valsDoc)
            <+> line
            <+> text ")"

        TSqlFormatter.Doc.Doc.Union(flatContent, expandedContent)

and private likePredicateDoc (cfg: FormattingStyle) (lk: LikePredicate) : Doc =
    let lhs = exprDoc cfg lk.FirstExpression
    let rhs = exprDoc cfg lk.SecondExpression

    let notPart =
        if lk.NotDefined then
            keyword cfg "NOT" <++> empty
        else
            empty

    let escape =
        if lk.EscapeExpression <> null then
            text " " <+> keyword cfg "ESCAPE" <++> exprDoc cfg lk.EscapeExpression
        else
            empty

    lhs <++> notPart <+> keyword cfg "LIKE" <++> rhs <+> escape

and private betweenDoc (cfg: FormattingStyle) (be: BooleanTernaryExpression) : Doc =
    let e = exprDoc cfg be.FirstExpression
    let lo = exprDoc cfg be.SecondExpression
    let hi = exprDoc cfg be.ThirdExpression

    let notPart =
        if be.TernaryExpressionType = BooleanTernaryExpressionType.NotBetween then
            keyword cfg "NOT" <++> empty
        else
            empty

    e <++> notPart <+> keyword cfg "BETWEEN" <++> lo <++> keyword cfg "AND" <++> hi

and private binaryExprDoc (cfg: FormattingStyle) (be: BinaryExpression) : Doc =
    let op =
        match be.BinaryExpressionType with
        | BinaryExpressionType.Add -> "+"
        | BinaryExpressionType.Subtract -> "-"
        | BinaryExpressionType.Multiply -> "*"
        | BinaryExpressionType.Divide -> "/"
        | BinaryExpressionType.Modulo -> "%"
        | BinaryExpressionType.BitwiseAnd -> "&"
        | BinaryExpressionType.BitwiseOr -> "|"
        | BinaryExpressionType.BitwiseXor -> "^"
        | _ -> "+"

    let lhs = exprDoc cfg be.FirstExpression
    let rhs = exprDoc cfg be.SecondExpression

    if cfg.operators.arithmetic.addSpacesAround then
        lhs <++> text op <++> rhs
    else
        lhs <+> text op <+> rhs

// ─── Parenthesized expressions ───

and private parenExprDoc (cfg: FormattingStyle) (p: ParenthesisExpression) : Doc =
    text "(" <+> exprDoc cfg p.Expression <+> text ")"

and private scalarSubqueryDoc (cfg: FormattingStyle) (sq: ScalarSubquery) : Doc =
    blockParenthesizedQueryDoc cfg true sq.QueryExpression (queryExprDoc cfg sq.QueryExpression)

// ─── Query expressions ───

and private queryExprDoc (cfg: FormattingStyle) (qe: QueryExpression) : Doc = querySpecOrExprDoc cfg qe None

and private querySpecOrExprDoc
    (cfg: FormattingStyle)
    (qe: QueryExpression)
    (intoTarget: TrailingFragmentDoc option)
    : Doc =
    match qe with
    | :? QuerySpecification as qs -> querySpecDoc cfg qs intoTarget
    | :? BinaryQueryExpression as bqe -> binaryQueryDoc cfg bqe
    | :? QueryParenthesisExpression as qpe ->
        blockParenthesizedQueryDoc cfg false qpe.QueryExpression (queryExprDoc cfg qpe.QueryExpression)
    | _ -> tokenStreamDoc cfg qe

and private querySpecDoc
    (cfg: FormattingStyle)
    (qs: QuerySpecification)
    (intoTarget: TrailingFragmentDoc option)
    : Doc =
    // SELECT clause
    let selectKw =
        let s = keyword cfg "SELECT"

        if qs.UniqueRowFilter = UniqueRowFilter.Distinct then
            s <++> keyword cfg "DISTINCT"
        elif qs.UniqueRowFilter = UniqueRowFilter.All then
            s <++> keyword cfg "ALL"
        else
            s

    let selectKwWithTop =
        if qs.TopRowFilter <> null then
            selectKw <++> topDoc cfg qs.TopRowFilter
        else
            selectKw

    let selectElements = qs.SelectElements |> Seq.toList

    let selectItems =
        selectElements
        |> List.mapi (fun i e ->
            let itemDoc =
                if fragmentHasComments e then
                    exprDoc cfg e
                else
                    exprDoc cfg e |> withTrailingComment e

            let leadingComments =
                match i with
                | 0 -> []
                | _ ->
                    let leadingComments = leadingInterComments selectElements.[i - 1] selectElements.[i]

                    let _, afterCommaComments, _ =
                        splitCommaInterComments selectElements.[i - 1] selectElements.[i]

                    leadingComments
                    |> List.filter (fun comment -> not (List.contains comment afterCommaComments))

            // SQL Prompt-compatible rule for comma-separated lists:
            // comments before the comma belong to the item on the left, comments after
            // the comma stay with that comma on the previous line, and only own-line
            // comments in the gap lead the next item. Any preserved after-comma
            // comment forces expanded list layout.
            let trailingCommentsAfterComma =
                match i with
                | _ when i = List.length selectElements - 1 -> []
                | _ -> afterCommaComments selectElements.[i] selectElements.[i + 1]

            let forcesExpandedLayout = not (List.isEmpty trailingCommentsAfterComma)

            { Doc = attachInlineLeadingComments leadingComments itemDoc
              ForcesExpandedLayout = forcesExpandedLayout
              AfterCommaComments = trailingCommentsAfterComma })

    let canKeepIntoWithSelect, selectClause =
        match selectItems, intoTarget with
        | [ single ], Some intoTarget ->
            true, (selectKwWithTop <++> single.Doc <++> keyword cfg "INTO" <++> intoTarget.Doc)
        | _ -> false, formatCommaList cfg selectKwWithTop selectItems

    let intoClause =
        match intoTarget with
        | Some intoTarget when not canKeepIntoWithSelect -> keyword cfg "INTO" <++> intoTarget.Doc
        | None -> empty
        | _ -> empty

    let parts =
        [ yield selectClause

          // INTO clause (SELECT ... INTO #temp ...)
          if intoTarget.IsSome && not canKeepIntoWithSelect then
              yield intoClause

          // FROM clause
          if
              qs.FromClause <> null
              && qs.FromClause.TableReferences <> null
              && qs.FromClause.TableReferences.Count > 0
          then
              yield keyword cfg "FROM" <++> tableRefDoc cfg qs.FromClause.TableReferences.[0]

              for i = 1 to qs.FromClause.TableReferences.Count - 1 do
                  yield text "," <++> tableRefDoc cfg qs.FromClause.TableReferences.[i]

          // WHERE clause
          if qs.WhereClause <> null && qs.WhereClause.SearchCondition <> null then
              yield headedConditionDoc cfg (keyword cfg "WHERE") qs.WhereClause.SearchCondition

          // GROUP BY clause
          if
              qs.GroupByClause <> null
              && qs.GroupByClause.GroupingSpecifications <> null
              && qs.GroupByClause.GroupingSpecifications.Count > 0
          then
              let groupItems =
                  qs.GroupByClause.GroupingSpecifications
                  |> Seq.map (fun g ->
                      match g with
                      | :? ExpressionGroupingSpecification as egs -> exprDoc cfg egs.Expression
                      | _ -> tokenStreamDoc cfg g)
                  |> Seq.toList

              yield formatList cfg (keyword cfg "GROUP" <++> keyword cfg "BY") groupItems

          // HAVING clause
          if qs.HavingClause <> null && qs.HavingClause.SearchCondition <> null then
              yield headedConditionDoc cfg (keyword cfg "HAVING") qs.HavingClause.SearchCondition

          // ORDER BY clause
          if
              qs.OrderByClause <> null
              && qs.OrderByClause.OrderByElements <> null
              && qs.OrderByClause.OrderByElements.Count > 0
          then
              let orderItems =
                  qs.OrderByClause.OrderByElements
                  |> Seq.map (fun o -> orderByElemDoc cfg o)
                  |> Seq.toList

              yield formatList cfg (keyword cfg "ORDER" <++> keyword cfg "BY") orderItems ]

    join line parts

and private booleanSequenceItems (cfg: FormattingStyle) (bb: BooleanBinaryExpression) : Doc list =
    let opText =
        match bb.BinaryExpressionType with
        | BooleanBinaryExpressionType.And -> "AND"
        | BooleanBinaryExpressionType.Or -> "OR"
        | _ -> "AND"

    let leftParts =
        match bb.FirstExpression with
        | :? BooleanBinaryExpression as lbb when lbb.BinaryExpressionType = bb.BinaryExpressionType ->
            booleanSequenceItems cfg lbb
        | _ -> [ exprDoc cfg bb.FirstExpression ]

    let beforeOpComments, afterOpComments = splitBooleanInterComments bb

    let leftPartsWithComments =
        match List.rev leftParts with
        | last :: restRev -> List.rev (appendInlineComments last beforeOpComments :: restRev)
        | [] -> []

    let rightExpr = exprDoc cfg bb.SecondExpression

    let rightPart =
        match afterOpComments with
        | [] -> keyword cfg opText <++> rightExpr
        | comments ->
            keyword cfg opText
            <++> join (text " ") (comments |> List.map renderComment)
            <++> rightExpr

    leftPartsWithComments @ [ rightPart ]

and private orderByElemDoc (cfg: FormattingStyle) (o: ExpressionWithSortOrder) : Doc =
    let e = exprDoc cfg o.Expression

    match o.SortOrder with
    | SortOrder.Ascending -> e <++> keyword cfg "ASC"
    | SortOrder.Descending -> e <++> keyword cfg "DESC"
    | _ -> e

// ─── Table references ───

and private tableRefDoc (cfg: FormattingStyle) (tr: TableReference) : Doc =
    match tr with
    | :? QualifiedJoin as qj -> qualifiedJoinDoc cfg qj
    | :? UnqualifiedJoin as uj -> unqualifiedJoinDoc cfg uj
    | :? NamedTableReference as ntr -> namedTableDoc cfg ntr
    | :? QueryDerivedTable as qdt -> queryDerivedTableDoc cfg qdt
    | :? SchemaObjectFunctionTableReference as softr -> schemaObjectFuncTableDoc cfg softr
    | _ -> tokenStreamDoc cfg tr

and private namedTableDoc (cfg: FormattingStyle) (ntr: NamedTableReference) : Doc =
    let nameDoc = schemaObjectNameDoc cfg ntr.SchemaObject

    let hints =
        if ntr.TableHints <> null && ntr.TableHints.Count > 0 then
            let hintToText (h: TableHint) =
                match h.HintKind with
                | TableHintKind.NoLock -> caseKeyword cfg.casing "NOLOCK"
                | TableHintKind.HoldLock -> caseKeyword cfg.casing "HOLDLOCK"
                | TableHintKind.UpdLock -> caseKeyword cfg.casing "UPDLOCK"
                | TableHintKind.Rowlock -> caseKeyword cfg.casing "ROWLOCK"
                | TableHintKind.PagLock -> caseKeyword cfg.casing "PAGLOCK"
                | TableHintKind.TabLock -> caseKeyword cfg.casing "TABLOCK"
                | TableHintKind.TabLockX -> caseKeyword cfg.casing "TABLOCKX"
                | TableHintKind.XLock -> caseKeyword cfg.casing "XLOCK"
                | TableHintKind.ReadUncommitted -> caseKeyword cfg.casing "READUNCOMMITTED"
                | TableHintKind.ReadCommitted -> caseKeyword cfg.casing "READCOMMITTED"
                | TableHintKind.RepeatableRead -> caseKeyword cfg.casing "REPEATABLEREAD"
                | TableHintKind.Serializable -> caseKeyword cfg.casing "SERIALIZABLE"
                | TableHintKind.ReadPast -> caseKeyword cfg.casing "READPAST"
                | _ -> fragmentText h

            let hintTexts = ntr.TableHints |> Seq.map hintToText |> Seq.toList
            text " (" <+> join (text ", ") (hintTexts |> List.map text) <+> text ")"
        else
            empty

    let alias =
        if ntr.Alias <> null then
            text " " <+> identDoc ntr.Alias
        else
            empty

    nameDoc <+> hints <+> alias

and private schemaObjectNameDoc (_cfg: FormattingStyle) (son: SchemaObjectName) : Doc =
    let parts = son.Identifiers |> Seq.map identDoc |> Seq.toList
    join (text ".") parts

and private qualifiedJoinDoc (cfg: FormattingStyle) (qj: QualifiedJoin) : Doc =
    let firstTable = tableRefDoc cfg qj.FirstTableReference

    let joinType =
        match qj.QualifiedJoinType with
        | QualifiedJoinType.Inner ->
            // The AST does not distinguish bare JOIN from INNER JOIN —
            // both parse as QualifiedJoinType.Inner. We must inspect
            // the token stream to detect whether INNER was explicit.
            let hasExplicitInner =
                match qj.ScriptTokenStream with
                | null -> true
                | stream ->
                    seq { qj.FirstTableReference.LastTokenIndex + 1 .. qj.SecondTableReference.FirstTokenIndex - 1 }
                    |> Seq.exists (fun i -> stream.[i].TokenType = TSqlTokenType.Inner)

            if hasExplicitInner then
                keyword cfg "INNER" <++> keyword cfg "JOIN"
            else
                keyword cfg "JOIN"
        | QualifiedJoinType.LeftOuter -> keyword cfg "LEFT" <++> keyword cfg "JOIN"
        | QualifiedJoinType.RightOuter -> keyword cfg "RIGHT" <++> keyword cfg "JOIN"
        | QualifiedJoinType.FullOuter -> keyword cfg "FULL" <++> keyword cfg "JOIN"
        | _ -> keyword cfg "JOIN"

    let secondTable = tableRefDoc cfg qj.SecondTableReference
    let onCondition = headedConditionDoc cfg (keyword cfg "ON") qj.SearchCondition

    firstTable <+> line <+> joinType <++> secondTable
    <+> nest (indentWidth cfg) (line <+> onCondition)

and private unqualifiedJoinDoc (cfg: FormattingStyle) (uj: UnqualifiedJoin) : Doc =
    let firstTable = tableRefDoc cfg uj.FirstTableReference

    let joinType =
        match uj.UnqualifiedJoinType with
        | UnqualifiedJoinType.CrossJoin -> keyword cfg "CROSS" <++> keyword cfg "JOIN"
        | UnqualifiedJoinType.CrossApply -> keyword cfg "CROSS" <++> keyword cfg "APPLY"
        | UnqualifiedJoinType.OuterApply -> keyword cfg "OUTER" <++> keyword cfg "APPLY"
        | _ -> keyword cfg "CROSS" <++> keyword cfg "JOIN"

    let secondTable = tableRefDoc cfg uj.SecondTableReference
    firstTable <+> line <+> joinType <++> secondTable

and private queryDerivedTableDoc (cfg: FormattingStyle) (qdt: QueryDerivedTable) : Doc =
    let inner = queryExprDoc cfg qdt.QueryExpression

    let parenDoc =
        text "(" <+> nest (indentWidth cfg) (line <+> inner) <+> line <+> text ")"

    let aliasDoc =
        if qdt.Alias <> null then
            text " " <+> identDoc qdt.Alias
        else
            empty

    parenDoc <+> aliasDoc

and private schemaObjectFuncTableDoc (cfg: FormattingStyle) (softr: SchemaObjectFunctionTableReference) : Doc =
    let nameDoc = schemaObjectNameDoc cfg softr.SchemaObject

    let args =
        if softr.Parameters <> null && softr.Parameters.Count > 0 then
            let argDocs = softr.Parameters |> Seq.map (fun a -> exprDoc cfg a) |> Seq.toList
            let flatArgs = join (text ", ") argDocs

            if
                canCollapseFragments
                    cfg.parentheses.collapseShortParenthesisContents
                    cfg.parentheses.collapseParenthesesShorterThan
                    (softr.Parameters |> Seq.cast<TSqlFragment>)
            then
                text "(" <+> flatArgs <+> text ")"
            else
                let argBody = commaSep argDocs

                text "("
                <+> nest (2 * indentWidth cfg) argBody
                <+> nest (indentWidth cfg) (line <+> text ")")
        else
            text "()"

    let aliasDoc =
        if softr.Alias <> null then
            text " " <+> identDoc softr.Alias
        else
            empty

    nameDoc <+> args <+> aliasDoc

// ─── Binary Query (UNION, EXCEPT, INTERSECT) ───

and private binaryQueryDoc (cfg: FormattingStyle) (bqe: BinaryQueryExpression) : Doc =
    let lhs = queryExprDoc cfg bqe.FirstQueryExpression
    let rhs = queryExprDoc cfg bqe.SecondQueryExpression

    let op =
        match bqe.BinaryQueryExpressionType with
        | BinaryQueryExpressionType.Union ->
            if bqe.All then
                keyword cfg "UNION" <++> keyword cfg "ALL"
            else
                keyword cfg "UNION"
        | BinaryQueryExpressionType.Except -> keyword cfg "EXCEPT"
        | BinaryQueryExpressionType.Intersect -> keyword cfg "INTERSECT"
        | _ -> keyword cfg "UNION"
    // Blank lines around set operators are a formatter extension, not part of SQL Prompt schema.
    let separator =
        if cfg.formatterExtensions.setOperations.blankLinesAroundOperators then
            line <+> line
        else
            line

    let result = lhs <+> separator <+> op <+> separator <+> rhs
    // ORDER BY on BinaryQueryExpression
    if
        bqe.OrderByClause <> null
        && bqe.OrderByClause.OrderByElements <> null
        && bqe.OrderByClause.OrderByElements.Count > 0
    then
        let orderItems =
            bqe.OrderByClause.OrderByElements
            |> Seq.map (fun o -> orderByElemDoc cfg o)
            |> Seq.toList

        result
        <+> line
        <+> formatList cfg (keyword cfg "ORDER" <++> keyword cfg "BY") orderItems
    else
        result

// ─── SELECT statement (top-level, with ORDER BY, FOR, etc.) ───

and private selectStatementDoc (cfg: FormattingStyle) (ss: SelectStatement) : Doc =
    let intoTarget =
        if ss.Into <> null then
            Some(trailingFragmentDoc (ss.Into :> TSqlFragment) (schemaObjectNameDoc cfg ss.Into))
        else
            None

    let qe = querySpecOrExprDoc cfg ss.QueryExpression intoTarget

    // CTEs
    let cteDoc =
        if
            ss.WithCtesAndXmlNamespaces <> null
            && ss.WithCtesAndXmlNamespaces.CommonTableExpressions <> null
            && ss.WithCtesAndXmlNamespaces.CommonTableExpressions.Count > 0
        then
            let cteParts =
                ss.WithCtesAndXmlNamespaces.CommonTableExpressions
                |> Seq.map (fun cte -> cteExprDoc cfg cte)
                |> Seq.toList

            Some(keyword cfg "WITH" <++> join (text "," <+> line) cteParts)
        else
            None

    let parts =
        [ match cteDoc with
          | Some d -> yield d
          | None -> ()
          yield qe ]

    join line parts

and private cteExprDoc (cfg: FormattingStyle) (cte: CommonTableExpression) : Doc =
    let nameDoc = identDoc cte.ExpressionName

    let colsDoc =
        if cte.Columns <> null && cte.Columns.Count > 0 then
            let cols = cte.Columns |> Seq.map identDoc |> Seq.toList
            text " (" <+> commaSep cols <+> text ")"
        else
            empty

    let asDoc = keyword cfg "AS"
    let body = queryExprDoc cfg cte.QueryExpression

    if cfg.cte.placeAsOnNewLine then
        nameDoc <+> colsDoc <+> line <+> asDoc <++> text "("
        <+> nest (indentWidth cfg) (line <+> body)
        <+> line
        <+> text ")"
    else
        nameDoc <+> colsDoc <++> asDoc <++> text "("
        <+> nest (indentWidth cfg) (line <+> body)
        <+> line
        <+> text ")"

// ─── DDL: ALTER/CREATE FUNCTION/PROCEDURE ───

and private ddlParamListDoc
    (cfg: FormattingStyle)
    (parameters: System.Collections.Generic.IList<ProcedureParameter>)
    (trailingCommentMap: Map<int, string>)
    (wrapInParens: bool)
    : Doc =
    if parameters = null || parameters.Count = 0 then
        if wrapInParens then text "()" else empty
    else
        let paramCount = parameters.Count

        let paramDocs =
            parameters
            |> Seq.mapi (fun i p ->
                let nameDoc = text p.VariableName.Value
                let typeDoc = dataTypeRefDoc cfg p.DataType

                let defaultDoc =
                    if p.Value <> null then
                        text " " <+> text "=" <++> exprDoc cfg p.Value
                    else
                        empty

                let outputDoc =
                    if p.Modifier = ParameterModifier.Output then
                        text " " <+> keyword cfg "OUTPUT"
                    elif p.Modifier = ParameterModifier.ReadOnly then
                        text " " <+> keyword cfg "READONLY"
                    else
                        empty
                // Place comma before trailing comment (not after)
                let commaDoc = if i < paramCount - 1 then text "," else empty

                let commentDoc =
                    match trailingCommentMap |> Map.tryFind i with
                    | Some c -> text " " <+> text c
                    | None -> empty

                nameDoc <++> typeDoc <+> defaultDoc <+> outputDoc <+> commaDoc <+> commentDoc)
            |> Seq.toList

        let paramsBody = join line paramDocs

        if wrapInParens then
            let isSingleLine = parameters.Count = 1 && trailingCommentMap.IsEmpty

            if isSingleLine then
                text "(" <+> flatten paramsBody <+> text ")"
            else
                text " (" <+> nest (indentWidth cfg) (line <+> paramsBody) <+> line <+> text ")"
        else
            nest (indentWidth cfg) (line <+> paramsBody)

and private statementListDoc
    (cfg: FormattingStyle)
    (statementContext: StatementContext)
    (separator: Doc)
    (fallback: Doc)
    (statementList: StatementList)
    : Doc =
    match statementList with
    | null -> fallback
    | stmtList ->
        stmtList.Statements
        |> Seq.cast<TSqlStatement>
        |> Seq.toList
        |> renderItemsWithLeadingComments
            (statementDoc cfg statementContext)
            (fun prev stmt ->
                match prev with
                | None -> []
                | Some prevStmt -> leadingInterComments prevStmt stmt)
            attachOwnLineComments
        |> join separator

and private routineWithAsDoc (cfg: FormattingStyle) (header: Doc) (paramsDoc: Doc) (bodyDoc: Doc) : Doc =
    header <+> paramsDoc <+> line <+> keyword cfg "AS" <+> line <+> bodyDoc

and private functionHeaderDoc
    (cfg: FormattingStyle)
    (name: SchemaObjectName)
    (parameters: System.Collections.Generic.IList<ProcedureParameter>)
    : Doc =
    let header = keyword cfg "FUNCTION" <++> schemaObjectNameDoc cfg name

    let commentMap = getParamTrailingComments parameters
    let paramsDoc = ddlParamListDoc cfg parameters commentMap true
    header <+> paramsDoc

and private returnTypeDoc (cfg: FormattingStyle) (stmt: TSqlStatement) (returnType: FunctionReturnType) : Doc =
    let returnsComment, returnsKeywordDoc = returnsKeyword cfg stmt

    let returnsWith (doc: Doc) =
        if returnsComment |> Option.isSome then
            returnsKeywordDoc <+> line <+> doc
        else
            returnsKeywordDoc <++> doc

    match returnType with
    | :? TableValuedFunctionReturnType
    | :? SelectFunctionReturnType -> returnsWith (keyword cfg "TABLE")
    | :? ScalarFunctionReturnType as srt -> returnsWith (dataTypeRefDoc cfg srt.DataType)
    | _ -> returnsWith (tokenStreamDoc cfg returnType)

and private inlineTvfBodyStartsWithParen (stmt: TSqlStatement) : bool =
    let stream = stmt.ScriptTokenStream

    if stream = null then
        false
    else
        match returnKeywordTokenIndex stmt with
        | None -> false
        | Some returnIdx ->
            let bodyStartIdx =
                seq { returnIdx + 1 .. stmt.LastTokenIndex }
                |> Seq.skipWhile (fun i -> stream.[i].TokenType = TSqlTokenType.WhiteSpace)
                |> Seq.tryHead

            match bodyStartIdx with
            | None -> false
            | Some idx -> stream.[idx].TokenType = TSqlTokenType.LeftParenthesis

and private inlineSelectFunctionBodyDoc
    (cfg: FormattingStyle)
    (wrapInParens: bool)
    (selectStatement: SelectStatement)
    : Doc =
    let selectDoc = selectStatementDoc cfg selectStatement

    if wrapInParens then
        let wrappedDoc =
            text "(" <+> nest (indentWidth cfg) (line <+> selectDoc) <+> line <+> text ")"

        wrappedDoc
    else
        selectDoc

and private functionBodyDoc (cfg: FormattingStyle) (stmt: FunctionStatementBody) : Doc =
    match stmt.ReturnType with
    | :? SelectFunctionReturnType as sfrt ->
        let stmt = stmt :> TSqlStatement
        let returnComment, returnDoc = returnKeyword cfg stmt
        let wrapInParens = inlineTvfBodyStartsWithParen stmt

        let selectBodyDoc =
            inlineSelectFunctionBodyDoc cfg wrapInParens sfrt.SelectStatement

        match returnComment with
        | Some _ -> returnDoc <+> line <+> selectBodyDoc
        | None -> returnDoc <++> selectBodyDoc
    | _ -> statementListDoc cfg standaloneStatementContext line (tokenStreamDoc cfg stmt) stmt.StatementList

and private procedureParamsHaveParens
    (stmt: TSqlStatement)
    (parameters: System.Collections.Generic.IList<ProcedureParameter>)
    : bool =
    if stmt = null || parameters = null || parameters.Count = 0 then
        false
    else
        let stream = stmt.ScriptTokenStream

        if stream = null then
            false
        else
            let firstParamIdx = parameters.[0].FirstTokenIndex

            let rec scanBack idx =
                if idx < stmt.FirstTokenIndex then
                    false
                else
                    match stream.[idx].TokenType with
                    | TSqlTokenType.WhiteSpace -> scanBack (idx - 1)
                    | TSqlTokenType.SingleLineComment
                    | TSqlTokenType.MultilineComment -> scanBack (idx - 1)
                    | TSqlTokenType.LeftParenthesis -> true
                    | _ -> false

            scanBack (firstParamIdx - 1)

and private getParamTrailingComments
    (parameters: System.Collections.Generic.IList<ProcedureParameter>)
    : Map<int, string> =
    if parameters = null || parameters.Count = 0 then
        Map.empty
    else
        let scanForComment (p: ProcedureParameter) =
            let stream = p.ScriptTokenStream

            if stream = null then
                None
            else
                let rec scan idx =
                    if idx >= stream.Count then
                        None
                    else
                        let tok = stream.[idx]

                        match tok.TokenType with
                        | TSqlTokenType.WhiteSpace ->
                            if tok.Text.Contains('\n') || tok.Text.Contains('\r') then
                                None
                            else
                                scan (idx + 1)
                        | TSqlTokenType.Comma -> scan (idx + 1)
                        | TSqlTokenType.SingleLineComment -> Some(tok.Text.TrimEnd())
                        | TSqlTokenType.MultilineComment -> Some tok.Text
                        | _ -> None

                scan (p.LastTokenIndex + 1)

        parameters
        |> Seq.mapi (fun i p -> i, scanForComment p)
        |> Seq.choose (fun (i, comment) -> comment |> Option.map (fun c -> i, c))
        |> Map.ofSeq

and private alterFunctionDoc (cfg: FormattingStyle) (af: AlterFunctionStatement) : Doc =
    let headerDoc = keyword cfg "ALTER" <++> functionHeaderDoc cfg af.Name af.Parameters
    let returnsDoc = returnTypeDoc cfg af af.ReturnType
    let bodyDoc = functionBodyDoc cfg af

    headerDoc
    <+> line
    <+> returnsDoc
    <+> line
    <+> keyword cfg "AS"
    <+> line
    <+> bodyDoc

and private createFunctionDoc (cfg: FormattingStyle) (cf: CreateFunctionStatement) : Doc =
    let headerDoc =
        keyword cfg "CREATE" <++> functionHeaderDoc cfg cf.Name cf.Parameters

    let returnsDoc = returnTypeDoc cfg cf cf.ReturnType
    let bodyDoc = functionBodyDoc cfg cf

    headerDoc
    <+> line
    <+> returnsDoc
    <+> line
    <+> keyword cfg "AS"
    <+> line
    <+> bodyDoc

and private alterProcedureDoc (cfg: FormattingStyle) (ap: AlterProcedureStatement) : Doc =
    let header =
        keyword cfg "ALTER"
        <++> keyword cfg "PROCEDURE"
        <++> schemaObjectNameDoc cfg ap.ProcedureReference.Name

    let commentMap = getParamTrailingComments ap.Parameters
    let wrapParams = procedureParamsHaveParens (ap :> TSqlStatement) ap.Parameters
    let paramsDoc = ddlParamListDoc cfg ap.Parameters commentMap wrapParams

    let bodyDoc =
        statementListDoc cfg embeddedStatementContext (line <+> line) empty ap.StatementList

    routineWithAsDoc cfg header paramsDoc bodyDoc

and private createProcedureDoc (cfg: FormattingStyle) (cp: CreateProcedureStatement) : Doc =
    let header =
        keyword cfg "CREATE"
        <++> keyword cfg "PROCEDURE"
        <++> schemaObjectNameDoc cfg cp.ProcedureReference.Name

    let commentMap = getParamTrailingComments cp.Parameters
    let wrapParams = procedureParamsHaveParens (cp :> TSqlStatement) cp.Parameters
    let paramsDoc = ddlParamListDoc cfg cp.Parameters commentMap wrapParams

    let bodyDoc =
        statementListDoc cfg embeddedStatementContext (line <+> line) empty cp.StatementList

    routineWithAsDoc cfg header paramsDoc bodyDoc

and private setOnOffDoc (cfg: FormattingStyle) (stmt: SetOnOffStatement) : Doc =
    let formatSetOption (option: SetOptions) =
        match option with
        | SetOptions.QuotedIdentifier -> keyword cfg "QUOTED_IDENTIFIER"
        | SetOptions.ConcatNullYieldsNull -> keyword cfg "CONCAT_NULL_YIELDS_NULL"
        | SetOptions.CursorCloseOnCommit -> keyword cfg "CURSOR_CLOSE_ON_COMMIT"
        | SetOptions.ArithAbort -> keyword cfg "ARITHABORT"
        | SetOptions.ArithIgnore -> keyword cfg "ARITHIGNORE"
        | SetOptions.FmtOnly -> keyword cfg "FMTONLY"
        | SetOptions.NoCount -> keyword cfg "NOCOUNT"
        | SetOptions.NoExec -> keyword cfg "NOEXEC"
        | SetOptions.NumericRoundAbort -> keyword cfg "NUMERIC_ROUNDABORT"
        | SetOptions.ParseOnly -> keyword cfg "PARSEONLY"
        | SetOptions.AnsiDefaults -> keyword cfg "ANSI_DEFAULTS"
        | SetOptions.AnsiNullDfltOff -> keyword cfg "ANSI_NULL_DFLT_OFF"
        | SetOptions.AnsiNullDfltOn -> keyword cfg "ANSI_NULL_DFLT_ON"
        | SetOptions.AnsiNulls -> keyword cfg "ANSI_NULLS"
        | SetOptions.AnsiPadding -> keyword cfg "ANSI_PADDING"
        | SetOptions.AnsiWarnings -> keyword cfg "ANSI_WARNINGS"
        | SetOptions.ForcePlan -> keyword cfg "FORCEPLAN"
        | SetOptions.ShowPlanAll -> keyword cfg "SHOWPLAN_ALL"
        | SetOptions.ShowPlanText -> keyword cfg "SHOWPLAN_TEXT"
        | SetOptions.ImplicitTransactions -> keyword cfg "IMPLICIT_TRANSACTIONS"
        | SetOptions.RemoteProcTransactions -> keyword cfg "REMOTE_PROC_TRANSACTIONS"
        | SetOptions.XactAbort -> keyword cfg "XACT_ABORT"
        | SetOptions.DisableDefCnstChk -> keyword cfg "DISABLE_DEF_CNST_CHK"
        | SetOptions.ShowPlanXml -> keyword cfg "SHOWPLAN_XML"
        | SetOptions.NoBrowsetable -> keyword cfg "NO_BROWSETABLE"
        | _ -> keyword cfg (option.ToString())

    let optionDocs =
        match stmt with
        | :? PredicateSetStatement as predicateStmt ->
            let knownOptions =
                [ SetOptions.QuotedIdentifier
                  SetOptions.ConcatNullYieldsNull
                  SetOptions.CursorCloseOnCommit
                  SetOptions.ArithAbort
                  SetOptions.ArithIgnore
                  SetOptions.FmtOnly
                  SetOptions.NoCount
                  SetOptions.NoExec
                  SetOptions.NumericRoundAbort
                  SetOptions.ParseOnly
                  SetOptions.AnsiDefaults
                  SetOptions.AnsiNullDfltOff
                  SetOptions.AnsiNullDfltOn
                  SetOptions.AnsiNulls
                  SetOptions.AnsiPadding
                  SetOptions.AnsiWarnings
                  SetOptions.ForcePlan
                  SetOptions.ShowPlanAll
                  SetOptions.ShowPlanText
                  SetOptions.ImplicitTransactions
                  SetOptions.RemoteProcTransactions
                  SetOptions.XactAbort
                  SetOptions.DisableDefCnstChk
                  SetOptions.ShowPlanXml
                  SetOptions.NoBrowsetable ]

            knownOptions
            |> List.filter (fun option -> predicateStmt.Options.HasFlag(option))
            |> List.map formatSetOption
        | _ -> []

    let optionsDoc = join (text "," <+> text " ") optionDocs
    let stateDoc = if stmt.IsOn then keyword cfg "ON" else keyword cfg "OFF"

    keyword cfg "SET" <++> optionsDoc <++> stateDoc

and private dropTableDoc (cfg: FormattingStyle) (stmt: DropTableStatement) : Doc =
    let baseParts =
        [ keyword cfg "DROP"
          keyword cfg "TABLE"
          if stmt.IsIfExists then
              keyword cfg "IF"
              keyword cfg "EXISTS" ]

    let baseDoc = join (text " ") baseParts

    let tableDocs = stmt.Objects |> Seq.map (schemaObjectNameDoc cfg) |> Seq.toList

    baseDoc <++> join (text "," <+> text " ") tableDocs

and private viewColumnsDoc (cfg: FormattingStyle) (columns: IList<Identifier>) : Doc =
    if columns = null || columns.Count = 0 then
        empty
    else
        let columnDocs = columns |> Seq.map identDoc |> Seq.toList
        text "(" <+> join (text "," <+> line) columnDocs <+> text ")"

and private viewOptionsDoc (cfg: FormattingStyle) (options: IList<ViewOption>) : Doc =
    if options = null || options.Count = 0 then
        empty
    else
        let optionDocs = options |> Seq.map (tokenStreamDoc cfg) |> Seq.toList
        line <+> formatList cfg (keyword cfg "WITH") optionDocs

and private viewStatementDoc (cfg: FormattingStyle) (verb: string) (vs: ViewStatementBody) : Doc =
    let header =
        keyword cfg verb
        <++> keyword cfg "VIEW"
        <++> schemaObjectNameDoc cfg vs.SchemaObjectName

    let columnsDoc = viewColumnsDoc cfg vs.Columns
    let optionsDoc = viewOptionsDoc cfg vs.ViewOptions
    let asDoc = line <+> keyword cfg "AS"
    let selectDoc = line <+> selectStatementDoc cfg vs.SelectStatement

    let checkOptionDoc =
        if vs.WithCheckOption then
            line <+> keyword cfg "WITH" <++> keyword cfg "CHECK" <++> keyword cfg "OPTION"
        else
            empty

    header <+> columnsDoc <+> optionsDoc <+> asDoc <+> selectDoc <+> checkOptionDoc

and private createTableElementDoc (cfg: FormattingStyle) (frag: TSqlFragment) : Doc =
    match frag with
    | :? ColumnDefinition as col ->
        let nameDoc = identDoc col.ColumnIdentifier
        let typeDoc = dataTypeRefDoc cfg col.DataType

        let tail =
            let stream = col.ScriptTokenStream

            if stream = null then
                ""
            else
                let startIdx =
                    if col.DataType <> null then
                        col.DataType.LastTokenIndex + 1
                    else
                        col.ColumnIdentifier.LastTokenIndex + 1

                let result, _ =
                    seq { startIdx .. col.LastTokenIndex }
                    |> Seq.map (fun i -> stream.[i])
                    |> Seq.fold
                        (fun (sb: System.Text.StringBuilder, prevWs) tok ->
                            match tok.TokenType with
                            | TSqlTokenType.WhiteSpace ->
                                if not prevWs then
                                    sb.Append(' ') |> ignore

                                (sb, true)
                            | TSqlTokenType.EndOfFile -> (sb, prevWs)
                            | _ ->
                                sb.Append(tok.Text) |> ignore
                                (sb, false))
                        (System.Text.StringBuilder(), false)

                result.ToString().Trim()

        if String.IsNullOrWhiteSpace tail then
            nameDoc <++> typeDoc
        else
            nameDoc <++> typeDoc <++> text tail
    | :? UniqueConstraintDefinition as constraintDef ->
        let constraintNameDoc =
            if constraintDef.ConstraintIdentifier <> null then
                keyword cfg "CONSTRAINT" <++> identDoc constraintDef.ConstraintIdentifier
            else
                empty

        let kindDoc =
            if constraintDef.IsPrimaryKey then
                keyword cfg "PRIMARY" <++> keyword cfg "KEY"
            else
                keyword cfg "UNIQUE"

        let indexTypeDoc =
            if constraintDef.Clustered.HasValue && constraintDef.Clustered.Value then
                keyword cfg "CLUSTERED"
            elif constraintDef.Clustered.HasValue && not constraintDef.Clustered.Value then
                keyword cfg "NONCLUSTERED"
            elif constraintDef.IndexType <> null then
                tokenStreamDoc cfg constraintDef.IndexType
            else
                empty

        let prefixDoc =
            [ constraintNameDoc; kindDoc; indexTypeDoc ] |> List.filter ((<>) empty) |> hcat

        let columns = constraintDef.Columns |> Seq.map (tokenStreamDoc cfg) |> Seq.toList

        let columnsDoc =
            match columns with
            | [] -> text "()"
            | [ singleColumn ] -> text "(" <+> singleColumn <+> text ")"
            | _ ->
                text "("
                <+> nest (indentWidth cfg) (line <+> join (text "," <+> line) columns)
                <+> line
                <+> text ")"

        let optionsDoc =
            if constraintDef.IndexOptions <> null && constraintDef.IndexOptions.Count > 0 then
                let optionDocs =
                    constraintDef.IndexOptions |> Seq.map (tokenStreamDoc cfg) |> Seq.toList

                text " " <+> formatList cfg (keyword cfg "WITH") optionDocs
            else
                empty

        let onDoc =
            if constraintDef.OnFileGroupOrPartitionScheme <> null then
                text " " <+> keyword cfg "ON"
                <++> tokenStreamDoc cfg constraintDef.OnFileGroupOrPartitionScheme
            else
                empty

        prefixDoc <++> columnsDoc <+> optionsDoc <+> onDoc
    | _ -> tokenStreamDoc cfg frag

and private createTableDoc (cfg: FormattingStyle) (ct: CreateTableStatement) : Doc =
    let header =
        keyword cfg "CREATE"
        <++> keyword cfg "TABLE"
        <++> schemaObjectNameDoc cfg ct.SchemaObjectName

    let definition = ct.Definition

    let elements =
        [ if definition <> null then
              for col in definition.ColumnDefinitions do
                  yield createTableElementDoc cfg col

              for constraintDef in definition.TableConstraints do
                  yield createTableElementDoc cfg constraintDef ]

    let bodyDoc =
        match elements with
        | [] -> text "()"
        | _ ->
            text " ("
            <+> nest (indentWidth cfg) (line <+> join (text "," <+> line) elements)
            <+> line
            <+> text ")"

    let onDoc =
        if ct.OnFileGroupOrPartitionScheme <> null then
            line <+> keyword cfg "ON"
            <++> tokenStreamDoc cfg ct.OnFileGroupOrPartitionScheme
        else
            empty

    let selectDoc =
        if ct.SelectStatement <> null then
            line <+> selectStatementDoc cfg ct.SelectStatement
        else
            empty

    header <+> bodyDoc <+> onDoc <+> selectDoc

// ─── Control flow ───

and private beginEndDoc (cfg: FormattingStyle) (be: BeginEndBlockStatement) : Doc =
    let stmts =
        if be.StatementList <> null then
            let statements =
                be.StatementList.Statements |> Seq.cast<TSqlStatement> |> Seq.toList

            statements
            |> renderItemsWithLeadingComments
                (statementDoc cfg standaloneStatementContext)
                (fun prev stmt ->
                    match prev with
                    | Some prevStmt -> leadingInterComments prevStmt stmt
                    | None ->
                        ownLineCommentsInRange be.ScriptTokenStream (be.FirstTokenIndex + 1) (stmt.FirstTokenIndex - 1))
                attachOwnLineComments
        else
            []

    keyword cfg "BEGIN"
    <+> nest (indentWidth cfg) (line <+> join (line <+> line) stmts)
    <+> line
    <+> keyword cfg "END"

and private ifDoc (cfg: FormattingStyle) (ifs: IfStatement) : Doc =
    let ifCondition = headedConditionDoc cfg (keyword cfg "IF") ifs.Predicate

    let thenDoc =
        let doc = statementDoc cfg standaloneStatementContext ifs.ThenStatement

        match ifs.ThenStatement with
        | :? BeginEndBlockStatement -> line <+> doc
        | _ -> nest (indentWidth cfg) (line <+> doc)

    let elseDoc =
        if ifs.ElseStatement <> null then
            let elseStmtDoc = statementDoc cfg standaloneStatementContext ifs.ElseStatement

            match ifs.ElseStatement with
            | :? BeginEndBlockStatement -> line <+> keyword cfg "ELSE" <+> line <+> elseStmtDoc
            | _ -> line <+> keyword cfg "ELSE" <+> nest (indentWidth cfg) (line <+> elseStmtDoc)
        else
            empty

    ifCondition <+> thenDoc <+> elseDoc

and private whileDoc (cfg: FormattingStyle) (ws: WhileStatement) : Doc =
    let whileCondition = headedConditionDoc cfg (keyword cfg "WHILE") ws.Predicate
    let bodyDoc = statementDoc cfg standaloneStatementContext ws.Statement
    whileCondition <+> line <+> bodyDoc

and private tryCatchDoc (cfg: FormattingStyle) (tc: TryCatchStatement) : Doc =
    let tryStmts =
        if tc.TryStatements <> null then
            tc.TryStatements.Statements
            |> Seq.cast<TSqlStatement>
            |> Seq.map (fun s -> statementDoc cfg standaloneStatementContext s)
            |> Seq.toList
        else
            []

    let catchStmts =
        if tc.CatchStatements <> null then
            tc.CatchStatements.Statements
            |> Seq.cast<TSqlStatement>
            |> Seq.map (fun s -> statementDoc cfg standaloneStatementContext s)
            |> Seq.toList
        else
            []

    keyword cfg "BEGIN" <++> keyword cfg "TRY"
    <+> nest (indentWidth cfg) (line <+> join (line <+> line) tryStmts)
    <+> line
    <+> keyword cfg "END"
    <++> keyword cfg "TRY"
    <+> line
    <+> keyword cfg "BEGIN"
    <++> keyword cfg "CATCH"
    <+> nest (indentWidth cfg) (line <+> join (line <+> line) catchStmts)
    <+> line
    <+> keyword cfg "END"
    <++> keyword cfg "CATCH"

// ─── Other DML ───

and private insertDoc (cfg: FormattingStyle) (ins: InsertStatement) : Doc =
    let spec = ins.InsertSpecification

    let target =
        if spec.Target <> null then
            tableRefDoc cfg spec.Target
        else
            empty

    let header = keyword cfg "INSERT" <++> keyword cfg "INTO" <++> target

    let colsDoc =
        if spec.Columns <> null && spec.Columns.Count > 0 then
            let cols = spec.Columns |> Seq.map (fun c -> columnRefDoc cfg c) |> Seq.toList
            // expandedSplit: ( at end of line, content indented, ) on own line
            let flatDoc = text " (" <+> join (text ", ") cols <+> text ")"

            let expandedDoc =
                text " ("
                <+> nest (indentWidth cfg) (line <+> join (text "," <+> line) cols)
                <+> line
                <+> text ")"

            TSqlFormatter.Doc.Doc.Union(flatDoc, expandedDoc)
        else
            empty

    let sourceDoc =
        match spec.InsertSource with
        | :? ValuesInsertSource as vis ->
            let rowDoc (rv: RowValue) =
                let vals = rv.ColumnValues |> Seq.map (fun v -> exprDoc cfg v) |> Seq.toList
                // Each row: collapse if short, expand (compactIndented) if long
                let flatRow = text "(" <+> join (text ", ") vals <+> text ")"

                let expandedRow =
                    text "("
                    <+> nest (indentWidth cfg) (line <+> join (text "," <+> line) vals)
                    <+> line
                    <+> text ")"

                TSqlFormatter.Doc.Doc.Union(flatRow, expandedRow)

            let rows = vis.RowValues |> Seq.map rowDoc |> Seq.toList
            // Multiple rows: comma-separated with line breaks between
            let flatRows = keyword cfg "VALUES" <++> join (text ", ") rows |> flatten
            let expandedRows = keyword cfg "VALUES" <++> join (text ", ") rows
            TSqlFormatter.Doc.Doc.Union(flatRows, expandedRows)
        | :? SelectInsertSource as sis -> queryExprDoc cfg sis.Select
        | _ -> tokenStreamDoc cfg spec.InsertSource

    header <+> colsDoc <+> line <+> sourceDoc

and private setClauseDoc (cfg: FormattingStyle) (sc: SetClause) : Doc =
    match sc with
    | :? AssignmentSetClause as asc -> assignmentSetClauseDoc cfg asc
    | _ -> tokenStreamDoc cfg sc

and private updateDoc (cfg: FormattingStyle) (upd: UpdateStatement) : Doc =
    let spec = upd.UpdateSpecification

    let target =
        if spec.Target <> null then
            tableRefDoc cfg spec.Target
        else
            empty

    let setClauses = spec.SetClauses |> Seq.map (setClauseDoc cfg) |> Seq.toList

    let parts =
        [ yield keyword cfg "UPDATE" <++> target
          yield formatList cfg (keyword cfg "SET") setClauses

          if
              spec.FromClause <> null
              && spec.FromClause.TableReferences <> null
              && spec.FromClause.TableReferences.Count > 0
          then
              yield keyword cfg "FROM" <++> tableRefDoc cfg spec.FromClause.TableReferences.[0]

          if spec.WhereClause <> null && spec.WhereClause.SearchCondition <> null then
              yield headedConditionDoc cfg (keyword cfg "WHERE") spec.WhereClause.SearchCondition ]

    join line parts

and private deleteDoc (cfg: FormattingStyle) (del: DeleteStatement) : Doc =
    let spec = del.DeleteSpecification

    let target =
        if spec.Target <> null then
            tableRefDoc cfg spec.Target
        else
            empty

    let parts =
        [ yield keyword cfg "DELETE" <++> target

          if
              spec.FromClause <> null
              && spec.FromClause.TableReferences <> null
              && spec.FromClause.TableReferences.Count > 0
          then
              yield keyword cfg "FROM" <++> tableRefDoc cfg spec.FromClause.TableReferences.[0]

          if spec.WhereClause <> null && spec.WhereClause.SearchCondition <> null then
              yield headedConditionDoc cfg (keyword cfg "WHERE") spec.WhereClause.SearchCondition ]

    join line parts

and private assignmentSetClauseDoc (cfg: FormattingStyle) (asc: AssignmentSetClause) : Doc =
    let col = columnRefDoc cfg asc.Column
    let value = exprDoc cfg asc.NewValue

    let op =
        match asc.AssignmentKind with
        | AssignmentKind.Equals -> "="
        | AssignmentKind.AddEquals -> "+="
        | AssignmentKind.SubtractEquals -> "-="
        | AssignmentKind.MultiplyEquals -> "*="
        | AssignmentKind.DivideEquals -> "/="
        | AssignmentKind.ModEquals -> "%="
        | AssignmentKind.BitwiseAndEquals -> "&="
        | AssignmentKind.BitwiseOrEquals -> "|="
        | AssignmentKind.BitwiseXorEquals -> "^="
        | _ -> "="

    col <++> text op <++> value

and private mergeDoc (cfg: FormattingStyle) (merge: MergeStatement) : Doc =
    let spec = merge.MergeSpecification

    let target =
        if spec.Target <> null then
            tableRefDoc cfg spec.Target
        else
            empty

    let targetAlias =
        if spec.TableAlias <> null then
            text " " <+> keyword cfg "AS" <++> identDoc spec.TableAlias
        else
            empty

    let source = tableRefDoc cfg spec.TableReference
    let onCondition = headedConditionDoc cfg (keyword cfg "ON") spec.SearchCondition

    let mergeHeader =
        keyword cfg "MERGE" <++> keyword cfg "INTO" <++> target
        <+> targetAlias
        <+> line
        <+> keyword cfg "USING"
        <++> source
        <+> line
        <+> nest (indentWidth cfg) onCondition

    let actionClauseDoc (ac: MergeActionClause) =
        let conditionKw =
            match ac.Condition with
            | MergeCondition.Matched -> keyword cfg "WHEN" <++> keyword cfg "MATCHED"
            | MergeCondition.NotMatched
            | MergeCondition.NotMatchedByTarget ->
                keyword cfg "WHEN"
                <++> keyword cfg "NOT"
                <++> keyword cfg "MATCHED"
                <++> keyword cfg "BY"
                <++> keyword cfg "TARGET"
            | MergeCondition.NotMatchedBySource ->
                keyword cfg "WHEN"
                <++> keyword cfg "NOT"
                <++> keyword cfg "MATCHED"
                <++> keyword cfg "BY"
                <++> keyword cfg "SOURCE"
            | _ -> keyword cfg "WHEN" <++> keyword cfg "MATCHED"

        let searchDoc =
            if ac.SearchCondition <> null then
                text " " <+> keyword cfg "AND" <++> exprDoc cfg ac.SearchCondition
            else
                empty

        let actionDoc =
            match ac.Action with
            | :? UpdateMergeAction as u ->
                let setClauses = u.SetClauses |> Seq.map (setClauseDoc cfg) |> Seq.toList

                keyword cfg "THEN"
                <+> nest
                        (indentWidth cfg)
                        (line <+> formatList cfg (keyword cfg "UPDATE" <++> keyword cfg "SET") setClauses)
            | :? InsertMergeAction as ins ->
                let cols =
                    if ins.Columns <> null && ins.Columns.Count > 0 then
                        let colDocs = ins.Columns |> Seq.map (fun c -> columnRefDoc cfg c) |> Seq.toList
                        let flatCols = text " (" <+> join (text ", ") colDocs <+> text ")"

                        let expandedCols =
                            text " ("
                            <+> nest (indentWidth cfg) (line <+> join (text "," <+> line) colDocs)
                            <+> line
                            <+> text ")"

                        TSqlFormatter.Doc.Doc.Union(flatCols, expandedCols)
                    else
                        empty

                let vals =
                    let vis = ins.Source

                    let rowDoc (rv: RowValue) =
                        let valDocs = rv.ColumnValues |> Seq.map (fun v -> exprDoc cfg v) |> Seq.toList
                        let flatRow = text "(" <+> join (text ", ") valDocs <+> text ")"

                        let expandedRow =
                            text "("
                            <+> nest (indentWidth cfg) (line <+> join (text "," <+> line) valDocs)
                            <+> line
                            <+> text ")"

                        TSqlFormatter.Doc.Doc.Union(flatRow, expandedRow)

                    let rows = vis.RowValues |> Seq.map rowDoc |> Seq.toList
                    keyword cfg "VALUES" <++> join (text ", ") rows

                keyword cfg "THEN"
                <+> nest (indentWidth cfg) (line <+> keyword cfg "INSERT" <+> cols <+> line <+> vals)
            | :? DeleteMergeAction -> keyword cfg "THEN" <+> nest (indentWidth cfg) (line <+> keyword cfg "DELETE")
            | _ -> tokenStreamDoc cfg ac.Action

        conditionKw <+> searchDoc <+> text " " <+> actionDoc

    let clauseDocs = spec.ActionClauses |> Seq.map actionClauseDoc |> Seq.toList
    join line (mergeHeader :: clauseDocs)

// ─── Variables ───

and private declareDoc (cfg: FormattingStyle) (decl: DeclareVariableStatement) : Doc =
    let varDocs =
        decl.Declarations
        |> Seq.map (fun d ->
            let nameDoc = text d.VariableName.Value
            let typeDoc = dataTypeRefDoc cfg d.DataType

            let valueDoc =
                if d.Value <> null then
                    text " " <+> text "=" <++> exprDoc cfg d.Value
                else
                    empty

            nameDoc <++> typeDoc <+> valueDoc)
        |> Seq.toList

    formatList cfg (keyword cfg "DECLARE") varDocs

and private setVarDoc (cfg: FormattingStyle) (sv: SetVariableStatement) : Doc =
    let varDoc = text sv.Variable.Name

    let op =
        match sv.AssignmentKind with
        | AssignmentKind.Equals -> "="
        | AssignmentKind.AddEquals -> "+="
        | AssignmentKind.SubtractEquals -> "-="
        | AssignmentKind.MultiplyEquals -> "*="
        | AssignmentKind.DivideEquals -> "/="
        | _ -> "="

    let valueDoc = exprDoc cfg sv.Expression
    keyword cfg "SET" <++> varDoc <++> text op <++> valueDoc

// ─── Statement dispatcher ───

and private statementDoc (cfg: FormattingStyle) (context: StatementContext) (stmt: TSqlStatement) : Doc =
    let semicolon (doc: Doc) =
        if context.HasTrailingSemicolon && hasTrailingSemicolon stmt then
            doc <+> text ";"
        else
            doc

    let semicolonAndTrailingComment (doc: Doc) = semicolon doc <+> trailingComment stmt

    match stmt with
    // Standalone statement forms that own their trailing semicolon here.
    | :? SelectStatement as ss ->
        let selectDoc = selectStatementDoc cfg ss

        let doc =
            if
                context.HasLeadingCteSemicolon
                && not cfg.formatterExtensions.cte.omitLeadingSemicolon
                && ss.WithCtesAndXmlNamespaces <> null
                && ss.WithCtesAndXmlNamespaces.CommonTableExpressions <> null
                && ss.WithCtesAndXmlNamespaces.CommonTableExpressions.Count > 0
            then
                text ";" <+> selectDoc
            else
                selectDoc

        semicolon doc
    | :? InsertStatement as ins -> insertDoc cfg ins |> semicolon
    | :? UpdateStatement as upd -> updateDoc cfg upd |> semicolon
    | :? DeleteStatement as del -> deleteDoc cfg del |> semicolon
    | :? MergeStatement as merge -> mergeDoc cfg merge |> semicolon
    | :? CreateTableStatement as ct -> createTableDoc cfg ct |> semicolon
    | :? DropTableStatement as dt -> dropTableDoc cfg dt |> semicolon
    | :? CreateViewStatement as cv -> viewStatementDoc cfg "CREATE" cv |> semicolon
    | :? AlterViewStatement as av -> viewStatementDoc cfg "ALTER" av |> semicolon
    | :? CreateOrAlterViewStatement as coav -> viewStatementDoc cfg "CREATE OR ALTER" coav |> semicolon
    | :? AlterFunctionStatement as af -> alterFunctionDoc cfg af |> semicolon
    | :? CreateFunctionStatement as cf -> createFunctionDoc cfg cf |> semicolon
    | :? AlterProcedureStatement as ap -> alterProcedureDoc cfg ap |> semicolon
    | :? CreateProcedureStatement as cp -> createProcedureDoc cfg cp |> semicolon
    | :? BeginEndBlockStatement as be -> beginEndDoc cfg be |> semicolon
    | :? PredicateSetStatement as ps -> setOnOffDoc cfg ps |> semicolon
    | :? ReturnStatement as rs ->
        let doc =
            if rs.Expression <> null then
                keyword cfg "RETURN" <++> exprDoc cfg rs.Expression
            else
                keyword cfg "RETURN"

        semicolon doc
    | :? PrintStatement as ps -> keyword cfg "PRINT" <++> exprDoc cfg ps.Expression |> semicolon

    // Statement forms that need semicolon/comment ordering handled together.
    | :? DeclareVariableStatement as dv -> declareDoc cfg dv |> semicolonAndTrailingComment
    | :? SetVariableStatement as sv -> setVarDoc cfg sv |> semicolonAndTrailingComment

    // Container/control-flow statements do not own a trailing semicolon here.
    | :? IfStatement as ifs -> ifDoc cfg ifs
    | :? WhileStatement as ws -> whileDoc cfg ws
    | :? TryCatchStatement as tc -> tryCatchDoc cfg tc

    // Fallback: preserve unsupported statements exactly as parsed.
    | :? ExecuteStatement as es -> tokenStreamDoc cfg es
    | :? RaiseErrorStatement as re -> tokenStreamDoc cfg re
    | :? ThrowStatement as ts -> tokenStreamDoc cfg ts
    | _ -> tokenStreamDoc cfg stmt

// ─── Top-level format function ───

/// Collect leading comments from the token stream before the first statement.
let private leadingCommentsDoc (script: TSqlScript) : Doc =
    if script.Batches.Count = 0 then
        empty
    else
        let firstBatch = script.Batches.[0]

        if firstBatch.Statements.Count = 0 then
            empty
        else
            let firstStmt = firstBatch.Statements.[0]
            let stream = firstStmt.ScriptTokenStream

            if stream = null then
                empty
            else
                let comments =
                    [ for i in 0 .. firstStmt.FirstTokenIndex - 1 do
                          let tok = stream.[i]

                          if
                              tok.TokenType = TSqlTokenType.SingleLineComment
                              || tok.TokenType = TSqlTokenType.MultilineComment
                          then
                              yield tok.Text.TrimEnd() ]

                match comments with
                | [] -> empty
                | _ ->
                    let commentDocs = comments |> List.map text
                    join line commentDocs <+> line

/// Format a T-SQL string using the given configuration.
let format (config: FormattingStyle) (sql: string) : Result<string, string list> =
    let parser = TSql160Parser(initialQuotedIdentifiers = true)
    use reader = new StringReader(sql)
    let mutable errors = null: System.Collections.Generic.IList<ParseError>
    let fragment = parser.Parse(reader, &errors)

    if errors <> null && errors.Count > 0 then
        Error [ for e in errors -> e.Message ]
    else
        match fragment with
        | :? TSqlScript as script ->
            let leadingComments = leadingCommentsDoc script

            let batchDocs =
                script.Batches
                |> Seq.map (fun batch ->
                    let stmtDocs =
                        batch.Statements
                        |> Seq.cast<TSqlStatement>
                        |> Seq.map (fun stmt -> statementDoc config standaloneStatementContext stmt)
                        |> Seq.toList

                    join (line <+> line) stmtDocs)
                |> Seq.toList

            let result =
                leadingComments <+> join (line <+> keyword config "GO" <+> line) batchDocs

            Ok(render config.whitespace.wrapLinesLongerThan result)
        | _ -> Ok(render config.whitespace.wrapLinesLongerThan (tokenStreamDoc config fragment))

let formatBooleanExpressionDoc (cfg: FormattingStyle) (expr: BooleanExpression) : Doc = boolExprDoc cfg expr
