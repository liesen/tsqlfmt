/// Core T-SQL formatter: parses SQL via ScriptDOM, transforms AST to Doc, renders to string.
module TSqlFormatter.Formatter

open System.IO
open System.Collections.Generic
open Microsoft.SqlServer.TransactSql.ScriptDom
open TSqlFormatter.Doc
open TSqlFormatter.Config
open TSqlFormatter.Keywords

// ─── Helpers ───

let private keyword (cfg: FormattingStyle) (s: string) = text (caseKeyword cfg.casing s)
let private functionName (cfg: FormattingStyle) (s: string) = text (caseFunction cfg.casing s)
let private dataType (cfg: FormattingStyle) (s: string) = text (caseDataType cfg.casing s)
let private indentWidth (cfg: FormattingStyle) = cfg.whitespace.numberOfSpacesInTabs

/// Format a clause-level comma list: first item on the same line as the keyword,
/// continuation items indented one level on new lines.
/// E.g. "SELECT col1,\n    col2,\n    col3"
let private clauseCommaList (cfg: FormattingStyle) (keyword: Doc) (items: Doc list) : Doc =
    match items with
    | [] -> keyword
    | [single] -> keyword <++> single
    | first :: rest ->
        let restDoc = join (text "," <+> line) rest
        keyword <++> first <+> text "," <+> nest (indentWidth cfg) (line <+> restDoc)

/// Like clauseCommaList, but collapses onto one line when it fits.
/// Used for ORDER BY, GROUP BY where short lists should stay inline.
let private clauseCommaGroup (cfg: FormattingStyle) (keyword: Doc) (items: Doc list) : Doc =
    match items with
    | [] -> keyword
    | [single] -> keyword <++> single
    | _ ->
        let flatDoc = keyword <++> join (text ", ") items |> flatten
        let expandedDoc = clauseCommaList cfg keyword items
        TSqlFormatter.Doc.Doc.Union(flatDoc, expandedDoc)

/// Get the raw SQL text of a fragment from its token stream.
let private fragmentText (frag: TSqlFragment) : string =
    if frag = null then ""
    else
        let stream = frag.ScriptTokenStream
        if stream = null then ""
        else
            let sb = System.Text.StringBuilder()
            for i = frag.FirstTokenIndex to frag.LastTokenIndex do
                sb.Append(stream.[i].Text) |> ignore
            sb.ToString().Trim()

/// Collect comments between two token indices from the token stream.
let private collectComments (stream: IList<TSqlParserToken>) (fromIdx: int) (toIdx: int) : string list =
    if stream = null then []
    else
        [ for i in fromIdx .. toIdx do
            let tok = stream.[i]
            if tok.TokenType = TSqlTokenType.SingleLineComment ||
               tok.TokenType = TSqlTokenType.MultilineComment then
                yield tok.Text.Trim() ]

/// Emit a fragment's tokens as raw text (fallback for unhandled node types).
let private tokenStreamDoc (cfg: FormattingStyle) (frag: TSqlFragment) : Doc =
    if frag = null then empty
    else
        let stream = frag.ScriptTokenStream
        if stream = null then text (fragmentText frag)
        else
            let result, _ =
                seq { frag.FirstTokenIndex .. frag.LastTokenIndex }
                |> Seq.map (fun i -> stream.[i])
                |> Seq.fold (fun (sb: System.Text.StringBuilder, prevWs) tok ->
                    match tok.TokenType with
                    | TSqlTokenType.WhiteSpace ->
                        if not prevWs then sb.Append(' ') |> ignore
                        (sb, true)
                    | TSqlTokenType.EndOfFile ->
                        (sb, prevWs)
                    | TSqlTokenType.SingleLineComment ->
                        sb.Append(tok.Text.TrimEnd()) |> ignore
                        (sb, false)
                    | TSqlTokenType.MultilineComment ->
                        sb.Append(tok.Text) |> ignore
                        (sb, false)
                    | _ ->
                        sb.Append(caseToken cfg.casing tok.Text) |> ignore
                        (sb, false)
                ) (System.Text.StringBuilder(), false)
            text (result.ToString().Trim())

/// Get comment tokens between the end of prevFrag and start of nextFrag.
let private interComments (prevFrag: TSqlFragment) (nextFrag: TSqlFragment) : string list =
    if prevFrag = null || nextFrag = null then []
    else
        let stream = prevFrag.ScriptTokenStream
        if stream = null || nextFrag.ScriptTokenStream = null then []
        elif not (System.Object.ReferenceEquals(stream, nextFrag.ScriptTokenStream)) then []
        else collectComments stream (prevFrag.LastTokenIndex + 1) (nextFrag.FirstTokenIndex - 1)

/// Attach trailing comment on the same line.
let private trailingComment (frag: TSqlFragment) : Doc =
    if frag = null then empty
    else
        let stream = frag.ScriptTokenStream
        if stream = null then empty
        else
            let rec scan idx =
                if idx >= stream.Count then empty
                else
                    let tok = stream.[idx]
                    match tok.TokenType with
                    | TSqlTokenType.WhiteSpace ->
                        if tok.Text.Contains('\n') || tok.Text.Contains('\r') then empty
                        else scan (idx + 1)
                    | TSqlTokenType.SingleLineComment ->
                        text " " <+> text (tok.Text.TrimEnd())
                    | TSqlTokenType.MultilineComment ->
                        text " " <+> text tok.Text
                    | _ -> empty
            scan (frag.LastTokenIndex + 1)

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
    | :? GlobalVariableExpression as gv ->
        text (applyCase cfg.casing.globalVariables gv.Name)
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
    | :? BooleanParenthesisExpression as bp ->
        boolParenDoc cfg bp
    | :? BooleanComparisonExpression as bc -> boolCompDoc cfg bc
    | :? BooleanBinaryExpression as bb -> boolBinaryDoc cfg bb
    | :? BooleanNotExpression as bn ->
        keyword cfg "NOT" <++> boolExprDoc cfg bn.Expression
    | :? BooleanIsNullExpression as bisn ->
        let e = exprDoc cfg bisn.Expression
        if bisn.IsNot then e <++> keyword cfg "IS" <++> keyword cfg "NOT" <++> keyword cfg "NULL"
        else e <++> keyword cfg "IS" <++> keyword cfg "NULL"
    | :? InPredicate as inp -> inPredicateDoc cfg inp
    | :? LikePredicate as lk -> likePredicateDoc cfg lk
    | :? ExistsPredicate as ep ->
        keyword cfg "EXISTS" <++> text "(" <+> queryExprDoc cfg ep.Subquery.QueryExpression <+> text ")"
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
        if iov.Identifier <> null then identDoc iov.Identifier
        else text ("'" + iov.Value.Replace("'", "''") + "'")
    | _ -> tokenStreamDoc cfg expr

and private boolExprDoc (cfg: FormattingStyle) (expr: BooleanExpression) : Doc =
    exprDoc cfg expr

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
    if iov.Identifier <> null then identDoc iov.Identifier
    else text ("'" + iov.Value.Replace("'", "''") + "'")

and private selectStarDoc (_cfg: FormattingStyle) (star: SelectStarExpression) : Doc =
    if star.Qualifier <> null then
        multiPartIdDoc _cfg star.Qualifier <+> text ".*"
    else
        text "*"

and private selectScalarDoc (cfg: FormattingStyle) (sse: SelectScalarExpression) : Doc =
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
            else true // default to having AS
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
    if top.Percent then d <++> keyword cfg "PERCENT"
    elif top.WithTies then d <++> keyword cfg "WITH" <++> keyword cfg "TIES"
    else d

// ─── CASE expressions ───

and private searchedCaseDoc (cfg: FormattingStyle) (c: SearchedCaseExpression) : Doc =
    // Build the flat version to measure length
    let flatParts =
        [ yield keyword cfg "CASE"
          for wc in c.WhenClauses do
              yield keyword cfg "WHEN" <++> boolExprDoc cfg wc.WhenExpression <++> keyword cfg "THEN" <++> exprDoc cfg wc.ThenExpression
          if c.ElseExpression <> null then
              yield keyword cfg "ELSE" <++> exprDoc cfg c.ElseExpression
          yield keyword cfg "END" ]
    let flatDoc = hcat flatParts

    if cfg.caseExpressions.collapseShortCaseExpressions then
        let flatStr = render cfg.whitespace.wrapLinesLongerThan flatDoc
        if flatStr.Length <= cfg.caseExpressions.collapseCaseExpressionsShorterThan && not (flatStr.Contains('\n')) then
            flatDoc
        else
            expandedCaseDoc cfg c
    else
        expandedCaseDoc cfg c

and private expandedCaseDoc (cfg: FormattingStyle) (c: SearchedCaseExpression) : Doc =
    let whenDocs =
        [ for wc in c.WhenClauses do
            yield keyword cfg "WHEN" <++> boolExprDoc cfg wc.WhenExpression <++> keyword cfg "THEN" <++> exprDoc cfg wc.ThenExpression ]
    let body = nest (indentWidth cfg) (line <+> join line whenDocs)
    let elseDoc =
        if c.ElseExpression <> null then
            line <+> keyword cfg "ELSE" <++> exprDoc cfg c.ElseExpression
        else empty
    keyword cfg "CASE" <+> body <+> elseDoc <+> line <+> keyword cfg "END"

and private simpleCaseDoc (cfg: FormattingStyle) (c: SimpleCaseExpression) : Doc =
    let inputExpr = exprDoc cfg c.InputExpression
    let flatParts =
        [ yield keyword cfg "CASE" <++> inputExpr
          for wc in c.WhenClauses do
              yield keyword cfg "WHEN" <++> exprDoc cfg wc.WhenExpression <++> keyword cfg "THEN" <++> exprDoc cfg wc.ThenExpression
          if c.ElseExpression <> null then
              yield keyword cfg "ELSE" <++> exprDoc cfg c.ElseExpression
          yield keyword cfg "END" ]
    let flatDoc = hcat flatParts

    if cfg.caseExpressions.collapseShortCaseExpressions then
        let flatStr = render cfg.whitespace.wrapLinesLongerThan flatDoc
        if flatStr.Length <= cfg.caseExpressions.collapseCaseExpressionsShorterThan && not (flatStr.Contains('\n')) then
            flatDoc
        else
            let whenDocs =
                [ for wc in c.WhenClauses do
                    yield keyword cfg "WHEN" <++> exprDoc cfg wc.WhenExpression <++> keyword cfg "THEN" <++> exprDoc cfg wc.ThenExpression ]
            let body = nest (indentWidth cfg) (line <+> join line whenDocs)
            let elseDoc =
                if c.ElseExpression <> null then
                    line <+> keyword cfg "ELSE" <++> exprDoc cfg c.ElseExpression
                else empty
            keyword cfg "CASE" <++> inputExpr <+> body <+> elseDoc <+> line <+> keyword cfg "END"
    else
        let whenDocs =
            [ for wc in c.WhenClauses do
                yield keyword cfg "WHEN" <++> exprDoc cfg wc.WhenExpression <++> keyword cfg "THEN" <++> exprDoc cfg wc.ThenExpression ]
        let body = nest (indentWidth cfg) (line <+> join line whenDocs)
        let elseDoc =
            if c.ElseExpression <> null then
                line <+> keyword cfg "ELSE" <++> exprDoc cfg c.ElseExpression
            else empty
        keyword cfg "CASE" <++> inputExpr <+> body <+> elseDoc <+> line <+> keyword cfg "END"

// ─── CAST / CONVERT ───

and private castCallDoc (cfg: FormattingStyle) (c: CastCall) : Doc =
    let dataTypeDoc = dataTypeRefDoc cfg c.DataType
    functionName cfg "CAST" <+> text "(" <+> exprDoc cfg c.Parameter <++> keyword cfg "AS" <++> dataTypeDoc <+> text ")"

and private tryCastCallDoc (cfg: FormattingStyle) (c: TryCastCall) : Doc =
    let dataTypeDoc = dataTypeRefDoc cfg c.DataType
    functionName cfg "TRY_CAST" <+> text "(" <+> exprDoc cfg c.Parameter <++> keyword cfg "AS" <++> dataTypeDoc <+> text ")"

and private convertCallDoc (cfg: FormattingStyle) (c: ConvertCall) : Doc =
    let dataTypeDoc = dataTypeRefDoc cfg c.DataType
    let args =
        if c.Style <> null then
            dataTypeDoc <+> text "," <++> exprDoc cfg c.Parameter <+> text "," <++> exprDoc cfg c.Style
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
    | :? UserDataTypeReference as udt ->
        multiPartIdDoc cfg udt.Name
    | _ -> tokenStreamDoc cfg dtr

// ─── Function calls ───

and private functionCallDoc (cfg: FormattingStyle) (f: FunctionCall) : Doc =
    let name =
        if f.FunctionName <> null then
            let n = f.FunctionName.Value
            if isBuiltInFunction n then caseFunction cfg.casing n
            else n
        else ""
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
                if uniqueStyle = UniqueRowFilter.Distinct then keyword cfg "DISTINCT" <++> empty
                elif uniqueStyle = UniqueRowFilter.All then keyword cfg "ALL" <++> empty
                else empty
            let flatArgs = text "(" <+> prefix <+> commaSep argDocs <+> text ")"
            let expandedArgs =
                text "(" <+> nest (indentWidth cfg) (line <+> prefix <+> join (text "," <+> line) argDocs) <+> line <+> text ")"
            TSqlFormatter.Doc.Doc.Union(flatArgs, expandedArgs)
        else
            text "()"
    let overDoc =
        if f.OverClause <> null then
            text " " <+> overClauseDoc cfg f.OverClause
        else empty
    callName <+> argsDoc <+> overDoc

and private overClauseDoc (cfg: FormattingStyle) (oc: OverClause) : Doc =
    let parts =
        [ if oc.Partitions <> null && oc.Partitions.Count > 0 then
              let partDocs = oc.Partitions |> Seq.map (fun p -> exprDoc cfg p) |> Seq.toList
              yield keyword cfg "PARTITION" <++> keyword cfg "BY" <++> commaSep partDocs
          if oc.OrderByClause <> null && oc.OrderByClause.OrderByElements <> null && oc.OrderByClause.OrderByElements.Count > 0 then
              let orderDocs = oc.OrderByClause.OrderByElements |> Seq.map (fun o -> orderByElemDoc cfg o) |> Seq.toList
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
                      if idx < stream.Count && stream.[idx].TokenType = TSqlTokenType.Identifier
                         && stream.[idx].Text.Equals("ROW", System.StringComparison.OrdinalIgnoreCase) then
                          idx
                      else
                          wf.LastTokenIndex
                  else wf.LastTokenIndex
              let tokens =
                  [ for i in wf.FirstTokenIndex .. lastIdx do
                      yield stream.[i] ]
              let sb = System.Text.StringBuilder()
              let mutable prevWs = false
              for tok in tokens do
                  match tok.TokenType with
                  | TSqlTokenType.WhiteSpace ->
                      if not prevWs then sb.Append(' ') |> ignore
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
        text "(" <+> nest (indentWidth cfg) (line <+> join (text "," <+> line) argDocs) <+> line <+> text ")"
    functionName cfg "COALESCE" <+> TSqlFormatter.Doc.Doc.Union(flatArgs, expandedArgs)

and private iifCallDoc (cfg: FormattingStyle) (iif: IIfCall) : Doc =
    functionName cfg "IIF" <+> text "(" <+> boolExprDoc cfg iif.Predicate <+> text "," <++> exprDoc cfg iif.ThenExpression <+> text "," <++> exprDoc cfg iif.ElseExpression <+> text ")"

and private nullIfDoc (cfg: FormattingStyle) (n: NullIfExpression) : Doc =
    functionName cfg "NULLIF" <+> text "(" <+> exprDoc cfg n.FirstExpression <+> text "," <++> exprDoc cfg n.SecondExpression <+> text ")"

// ─── Boolean expressions ───

and private boolCompDoc (cfg: FormattingStyle) (bc: BooleanComparisonExpression) : Doc =
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
    let expandedDoc = text "(" <+> nest (indentWidth cfg) (line <+> inner) <+> line <+> text ")"
    TSqlFormatter.Doc.Doc.Union(flatDoc, expandedDoc)

and private inPredicateDoc (cfg: FormattingStyle) (inp: InPredicate) : Doc =
    let lhs = exprDoc cfg inp.Expression
    let notPart = if inp.NotDefined then keyword cfg "NOT" <++> empty else empty
    let inKw = keyword cfg "IN"
    if inp.Subquery <> null then
        let subDoc = queryExprDoc cfg inp.Subquery.QueryExpression
        // Check for collapse
        let flatSub = render cfg.whitespace.wrapLinesLongerThan subDoc
        if cfg.dml.collapseShortSubqueries && flatSub.Length < cfg.dml.collapseSubqueriesShorterThan && not (flatSub.Contains('\n')) then
            lhs <++> notPart <+> inKw <++> text "(" <+> subDoc <+> text ")"
        else
            // expandedSplit: content at nest+indent, closing paren at nest level
            // (whereConditionDoc wraps the first expression in nest (indentWidth cfg), providing the outer indent)
            lhs <++> notPart <+> inKw <++> text "(" <+> nest (indentWidth cfg) (line <+> subDoc) <+> line <+> text ")"
    else
        let valDocs = inp.Values |> Seq.map (fun v -> exprDoc cfg v) |> Seq.toList
        // Use Union to allow the renderer to collapse onto one line when it fits.
        // The flat version has no spaces inside parens: IN (val1, val2)
        // The expanded version has each value on a new indented line.
        let flatContent = flatten (lhs <++> notPart <+> inKw <++> text "(" <+> join (text ", ") valDocs <+> text ")")
        let expandedContent =
            let valSep = text "," <+> line
            let valsDoc = join valSep valDocs
            lhs <++> notPart <+> inKw <++> text "(" <+> nest (indentWidth cfg) (line <+> valsDoc) <+> line <+> text ")"
        TSqlFormatter.Doc.Doc.Union(flatContent, expandedContent)

and private likePredicateDoc (cfg: FormattingStyle) (lk: LikePredicate) : Doc =
    let lhs = exprDoc cfg lk.FirstExpression
    let rhs = exprDoc cfg lk.SecondExpression
    let notPart = if lk.NotDefined then keyword cfg "NOT" <++> empty else empty
    let escape =
        if lk.EscapeExpression <> null then
            text " " <+> keyword cfg "ESCAPE" <++> exprDoc cfg lk.EscapeExpression
        else empty
    lhs <++> notPart <+> keyword cfg "LIKE" <++> rhs <+> escape

and private betweenDoc (cfg: FormattingStyle) (be: BooleanTernaryExpression) : Doc =
    let e = exprDoc cfg be.FirstExpression
    let lo = exprDoc cfg be.SecondExpression
    let hi = exprDoc cfg be.ThirdExpression
    let notPart = if be.TernaryExpressionType = BooleanTernaryExpressionType.NotBetween then keyword cfg "NOT" <++> empty else empty
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
    let inner = queryExprDoc cfg sq.QueryExpression
    let flatStr = render cfg.whitespace.wrapLinesLongerThan inner
    if cfg.dml.collapseShortSubqueries && flatStr.Length < cfg.dml.collapseSubqueriesShorterThan && not (flatStr.Contains('\n')) then
        text "(" <+> inner <+> text ")"
    else
        text "(" <+> nest (indentWidth cfg) (line <+> inner) <+> line <+> text ")"

// ─── Query expressions ───

and private queryExprDoc (cfg: FormattingStyle) (qe: QueryExpression) : Doc =
    querySpecOrExprDoc cfg qe None

and private querySpecOrExprDoc (cfg: FormattingStyle) (qe: QueryExpression) (intoTarget: Doc option) : Doc =
    match qe with
    | :? QuerySpecification as qs -> querySpecDoc cfg qs intoTarget
    | :? BinaryQueryExpression as bqe -> binaryQueryDoc cfg bqe
    | :? QueryParenthesisExpression as qpe ->
        text "(" <+> nest (indentWidth cfg) (line <+> queryExprDoc cfg qpe.QueryExpression) <+> line <+> text ")"
    | _ -> tokenStreamDoc cfg qe

and private querySpecDoc (cfg: FormattingStyle) (qs: QuerySpecification) (intoTarget: Doc option) : Doc =
    // SELECT clause
    let selectKw =
        let s = keyword cfg "SELECT"
        if qs.UniqueRowFilter = UniqueRowFilter.Distinct then
            s <++> keyword cfg "DISTINCT"
        elif qs.UniqueRowFilter = UniqueRowFilter.All then
            s <++> keyword cfg "ALL"
        else s
    let selectKwWithTop =
        if qs.TopRowFilter <> null then
            selectKw <++> topDoc cfg qs.TopRowFilter
        else selectKw

    let selectItems = qs.SelectElements |> Seq.map (fun e -> exprDoc cfg e) |> Seq.toList
    let selectClause = clauseCommaList cfg selectKwWithTop selectItems

    let parts =
        [ yield selectClause

          // INTO clause (SELECT ... INTO #temp ...)
          match intoTarget with
          | Some t -> yield keyword cfg "INTO" <++> t
          | None -> ()

          // FROM clause
          if qs.FromClause <> null && qs.FromClause.TableReferences <> null && qs.FromClause.TableReferences.Count > 0 then
              yield keyword cfg "FROM" <++> tableRefDoc cfg qs.FromClause.TableReferences.[0]
              for i = 1 to qs.FromClause.TableReferences.Count - 1 do
                  yield text "," <++> tableRefDoc cfg qs.FromClause.TableReferences.[i]

          // WHERE clause
          if qs.WhereClause <> null && qs.WhereClause.SearchCondition <> null then
              yield keyword cfg "WHERE" <++> whereConditionDoc cfg qs.WhereClause.SearchCondition

          // GROUP BY clause
          if qs.GroupByClause <> null && qs.GroupByClause.GroupingSpecifications <> null && qs.GroupByClause.GroupingSpecifications.Count > 0 then
              let groupItems =
                  qs.GroupByClause.GroupingSpecifications
                  |> Seq.map (fun g ->
                      match g with
                      | :? ExpressionGroupingSpecification as egs -> exprDoc cfg egs.Expression
                      | _ -> tokenStreamDoc cfg g)
                  |> Seq.toList
              yield clauseCommaGroup cfg (keyword cfg "GROUP" <++> keyword cfg "BY") groupItems

          // HAVING clause
          if qs.HavingClause <> null && qs.HavingClause.SearchCondition <> null then
              yield keyword cfg "HAVING" <++> whereConditionDoc cfg qs.HavingClause.SearchCondition

          // ORDER BY clause
          if qs.OrderByClause <> null && qs.OrderByClause.OrderByElements <> null && qs.OrderByClause.OrderByElements.Count > 0 then
              let orderItems = qs.OrderByClause.OrderByElements |> Seq.map (fun o -> orderByElemDoc cfg o) |> Seq.toList
              yield clauseCommaGroup cfg (keyword cfg "ORDER" <++> keyword cfg "BY") orderItems ]

    join line parts

and private whereConditionDoc (cfg: FormattingStyle) (expr: BooleanExpression) : Doc =
    // Flatten AND/OR chains with proper indentation
    match expr with
    | :? BooleanBinaryExpression as bb ->
        let parts = flattenBoolChain cfg bb
        match parts with
        | [] -> empty
        | [single] -> single
        | first :: rest ->
            // Wrap first in nest (indentWidth cfg) so that any line breaks inside it
            // (e.g. IN subquery content) get an extra indent level.
            // This makes expandedSplit IN subqueries indent correctly at both
            // top-level WHERE and nested ON/AND contexts.
            let restDoc = rest |> List.map id |> join line
            nest (indentWidth cfg) first <+> nest (indentWidth cfg) (line <+> restDoc)
    | _ -> nest (indentWidth cfg) (exprDoc cfg expr)

and private flattenBoolChain (cfg: FormattingStyle) (bb: BooleanBinaryExpression) : Doc list =
    let opText =
        match bb.BinaryExpressionType with
        | BooleanBinaryExpressionType.And -> "AND"
        | BooleanBinaryExpressionType.Or -> "OR"
        | _ -> "AND"

    let leftParts =
        match bb.FirstExpression with
        | :? BooleanBinaryExpression as lbb when lbb.BinaryExpressionType = bb.BinaryExpressionType ->
            flattenBoolChain cfg lbb
        | _ -> [exprDoc cfg bb.FirstExpression]

    let rightPart = keyword cfg opText <++> exprDoc cfg bb.SecondExpression
    leftParts @ [rightPart]

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
        else empty
    let alias =
        if ntr.Alias <> null then
            text " " <+> identDoc ntr.Alias
        else empty
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
            if hasExplicitInner then keyword cfg "INNER" <++> keyword cfg "JOIN"
            else keyword cfg "JOIN"
        | QualifiedJoinType.LeftOuter -> keyword cfg "LEFT" <++> keyword cfg "JOIN"
        | QualifiedJoinType.RightOuter -> keyword cfg "RIGHT" <++> keyword cfg "JOIN"
        | QualifiedJoinType.FullOuter -> keyword cfg "FULL" <++> keyword cfg "JOIN"
        | _ -> keyword cfg "JOIN"
    let secondTable = tableRefDoc cfg qj.SecondTableReference
    let onCondition = whereConditionDoc cfg qj.SearchCondition
    firstTable <+> line <+> joinType <++> secondTable <+> nest (indentWidth cfg) (line <+> keyword cfg "ON" <++> onCondition)

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
    let parenDoc = text "(" <+> nest (indentWidth cfg) (line <+> inner) <+> line <+> text ")"
    let aliasDoc =
        if qdt.Alias <> null then text " " <+> identDoc qdt.Alias
        else empty
    parenDoc <+> aliasDoc

and private schemaObjectFuncTableDoc (cfg: FormattingStyle) (softr: SchemaObjectFunctionTableReference) : Doc =
    let nameDoc = schemaObjectNameDoc cfg softr.SchemaObject
    let args =
        if softr.Parameters <> null && softr.Parameters.Count > 0 then
            let argDocs = softr.Parameters |> Seq.map (fun a -> exprDoc cfg a) |> Seq.toList
            // expandedSplit: first arg inline with (, rest indented 2*indent, closing paren at indent
            let flatArgs = join (text ", ") argDocs
            let flatStr = render cfg.whitespace.wrapLinesLongerThan flatArgs
            // Collapse if parenthesis content is shorter than the collapse threshold
            if cfg.parentheses.collapseShortParenthesisContents && flatStr.Length < cfg.parentheses.collapseParenthesesShorterThan && not (flatStr.Contains('\n')) then
                text "(" <+> flatArgs <+> text ")"
            else
                let argBody = commaSep argDocs
                text "(" <+> nest (2 * indentWidth cfg) argBody <+> nest (indentWidth cfg) (line <+> text ")")
        else text "()"
    let aliasDoc =
        if softr.Alias <> null then text " " <+> identDoc softr.Alias
        else empty
    nameDoc <+> args <+> aliasDoc

// ─── Binary Query (UNION, EXCEPT, INTERSECT) ───

and private binaryQueryDoc (cfg: FormattingStyle) (bqe: BinaryQueryExpression) : Doc =
    let lhs = queryExprDoc cfg bqe.FirstQueryExpression
    let rhs = queryExprDoc cfg bqe.SecondQueryExpression
    let op =
        match bqe.BinaryQueryExpressionType with
        | BinaryQueryExpressionType.Union ->
            if bqe.All then keyword cfg "UNION" <++> keyword cfg "ALL"
            else keyword cfg "UNION"
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
    if bqe.OrderByClause <> null && bqe.OrderByClause.OrderByElements <> null && bqe.OrderByClause.OrderByElements.Count > 0 then
        let orderItems = bqe.OrderByClause.OrderByElements |> Seq.map (fun o -> orderByElemDoc cfg o) |> Seq.toList
        result <+> line <+> clauseCommaGroup cfg (keyword cfg "ORDER" <++> keyword cfg "BY") orderItems
    else result

// ─── SELECT statement (top-level, with ORDER BY, FOR, etc.) ───

and private selectStatementDoc (cfg: FormattingStyle) (ss: SelectStatement) : Doc =
    let intoTarget =
        if ss.Into <> null then Some (schemaObjectNameDoc cfg ss.Into)
        else None
    let qe = querySpecOrExprDoc cfg ss.QueryExpression intoTarget

    // CTEs
    let cteDoc =
        if ss.WithCtesAndXmlNamespaces <> null && ss.WithCtesAndXmlNamespaces.CommonTableExpressions <> null && ss.WithCtesAndXmlNamespaces.CommonTableExpressions.Count > 0 then
            let cteParts =
                ss.WithCtesAndXmlNamespaces.CommonTableExpressions
                |> Seq.map (fun cte -> cteExprDoc cfg cte)
                |> Seq.toList
            let withKw =
                if cfg.formatterExtensions.cte.omitLeadingSemicolon then
                    keyword cfg "WITH"
                else
                    text ";" <+> keyword cfg "WITH"
            Some (withKw <++> join (text "," <+> line) cteParts)
        else None

    let parts =
        [ match cteDoc with Some d -> yield d | None -> ()
          yield qe ]

    join line parts

and private cteExprDoc (cfg: FormattingStyle) (cte: CommonTableExpression) : Doc =
    let nameDoc = identDoc cte.ExpressionName
    let colsDoc =
        if cte.Columns <> null && cte.Columns.Count > 0 then
            let cols = cte.Columns |> Seq.map identDoc |> Seq.toList
            text " (" <+> commaSep cols <+> text ")"
        else empty
    let asDoc = keyword cfg "AS"
    let body = queryExprDoc cfg cte.QueryExpression
    if cfg.cte.placeAsOnNewLine then
        nameDoc <+> colsDoc <+> line <+> asDoc <++> text "(" <+> nest (indentWidth cfg) (line <+> body) <+> line <+> text ")"
    else
        nameDoc <+> colsDoc <++> asDoc <++> text "(" <+> nest (indentWidth cfg) (line <+> body) <+> line <+> text ")"

// ─── DDL: ALTER/CREATE FUNCTION/PROCEDURE ───

and private ddlParamListDoc (cfg: FormattingStyle) (parameters: System.Collections.Generic.IList<ProcedureParameter>) (trailingCommentMap: Map<int, string>) (wrapInParens: bool) : Doc =
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
                    else empty
                let outputDoc =
                    if p.Modifier = ParameterModifier.Output then text " " <+> keyword cfg "OUTPUT"
                    elif p.Modifier = ParameterModifier.ReadOnly then text " " <+> keyword cfg "READONLY"
                    else empty
                // Place comma before trailing comment (not after)
                let commaDoc =
                    if i < paramCount - 1 then text ","
                    else empty
                let commentDoc =
                    match trailingCommentMap |> Map.tryFind i with
                    | Some c -> text " " <+> text c
                    | None -> empty
                nameDoc <++> typeDoc <+> defaultDoc <+> outputDoc <+> commaDoc <+> commentDoc)
            |> Seq.toList
        let paramsBody = join line paramDocs
        if wrapInParens then
            text " (" <+> nest (indentWidth cfg) (line <+> paramsBody) <+> line <+> text ")"
        else
            nest (indentWidth cfg) (line <+> paramsBody)

and private procedureParamsHaveParens (stmt: TSqlStatement) (parameters: System.Collections.Generic.IList<ProcedureParameter>) : bool =
    if stmt = null || parameters = null || parameters.Count = 0 then false
    else
        let stream = stmt.ScriptTokenStream
        if stream = null then false
        else
            let firstParamIdx = parameters.[0].FirstTokenIndex
            let rec scanBack idx =
                if idx < stmt.FirstTokenIndex then false
                else
                    match stream.[idx].TokenType with
                    | TSqlTokenType.WhiteSpace -> scanBack (idx - 1)
                    | TSqlTokenType.SingleLineComment
                    | TSqlTokenType.MultilineComment -> scanBack (idx - 1)
                    | TSqlTokenType.LeftParenthesis -> true
                    | _ -> false
            scanBack (firstParamIdx - 1)

and private getParamTrailingComments (parameters: System.Collections.Generic.IList<ProcedureParameter>) : Map<int, string> =
    if parameters = null || parameters.Count = 0 then Map.empty
    else
        let scanForComment (p: ProcedureParameter) =
            let stream = p.ScriptTokenStream
            if stream = null then None
            else
                let rec scan idx =
                    if idx >= stream.Count then None
                    else
                        let tok = stream.[idx]
                        match tok.TokenType with
                        | TSqlTokenType.WhiteSpace ->
                            if tok.Text.Contains('\n') || tok.Text.Contains('\r') then None
                            else scan (idx + 1)
                        | TSqlTokenType.Comma -> scan (idx + 1)
                        | TSqlTokenType.SingleLineComment -> Some (tok.Text.TrimEnd())
                        | TSqlTokenType.MultilineComment -> Some tok.Text
                        | _ -> None
                scan (p.LastTokenIndex + 1)

        parameters
        |> Seq.mapi (fun i p -> i, scanForComment p)
        |> Seq.choose (fun (i, comment) -> comment |> Option.map (fun c -> i, c))
        |> Map.ofSeq

and private alterFunctionDoc (cfg: FormattingStyle) (af: AlterFunctionStatement) : Doc =
    let header = keyword cfg "ALTER" <++> keyword cfg "FUNCTION" <++> schemaObjectNameDoc cfg af.Name
    let commentMap = getParamTrailingComments af.Parameters
    let paramsDoc = ddlParamListDoc cfg af.Parameters commentMap true
    let bodyDoc =
        match af.ReturnType with
        | :? SelectFunctionReturnType as sfrt ->
            // Inline TVF: RETURNS TABLE AS RETURN SELECT ...
            let returnsDoc = keyword cfg "RETURNS" <++> keyword cfg "TABLE"
            let asDoc = keyword cfg "AS"
            let selectDoc = selectStatementDoc cfg sfrt.SelectStatement
            returnsDoc <+> line <+> asDoc <+> line <+> keyword cfg "RETURN" <++> selectDoc
        | :? ScalarFunctionReturnType as srt ->
            let returnsDoc = keyword cfg "RETURNS" <++> dataTypeRefDoc cfg srt.DataType
            let asDoc = keyword cfg "AS"
            let stmtDoc =
                match af.StatementList with
                | null -> empty
                | stmtList ->
                    let stmts = stmtList.Statements |> Seq.map (fun s -> statementDoc cfg s) |> Seq.toList
                    join line stmts
            returnsDoc <+> line <+> asDoc <+> line <+> stmtDoc
        | :? TableValuedFunctionReturnType ->
            let returnsDoc = keyword cfg "RETURNS" <++> keyword cfg "TABLE"
            let asDoc = keyword cfg "AS"
            let stmtDoc =
                match af.StatementList with
                | null -> tokenStreamDoc cfg af
                | stmtList ->
                    let stmts = stmtList.Statements |> Seq.map (fun s -> statementDoc cfg s) |> Seq.toList
                    join line stmts
            returnsDoc <+> line <+> asDoc <+> line <+> stmtDoc
        | _ ->
            let returnsDoc = keyword cfg "RETURNS" <++> tokenStreamDoc cfg af.ReturnType
            let asDoc = keyword cfg "AS"
            let stmtDoc =
                match af.StatementList with
                | null -> tokenStreamDoc cfg af
                | stmtList ->
                    let stmts = stmtList.Statements |> Seq.map (fun s -> statementDoc cfg s) |> Seq.toList
                    join line stmts
            returnsDoc <+> line <+> asDoc <+> line <+> stmtDoc

    header <+> paramsDoc <+> line <+> bodyDoc

and private createFunctionDoc (cfg: FormattingStyle) (cf: CreateFunctionStatement) : Doc =
    let header = keyword cfg "CREATE" <++> keyword cfg "FUNCTION" <++> schemaObjectNameDoc cfg cf.Name
    let commentMap = getParamTrailingComments cf.Parameters
    let paramsDoc = ddlParamListDoc cfg cf.Parameters commentMap true
    let returnsDoc = keyword cfg "RETURNS" <++> (
        match cf.ReturnType with
        | :? TableValuedFunctionReturnType -> keyword cfg "TABLE"
        | :? ScalarFunctionReturnType as srt -> dataTypeRefDoc cfg srt.DataType
        | _ -> tokenStreamDoc cfg cf.ReturnType
    )
    let asDoc = keyword cfg "AS"
    let bodyDoc =
        match cf.StatementList with
        | null -> tokenStreamDoc cfg cf
        | stmtList ->
            let stmts = stmtList.Statements |> Seq.map (fun s -> statementDoc cfg s) |> Seq.toList
            join line stmts

    header <+> paramsDoc <+> line <+> returnsDoc <+> line <+> asDoc <+> line <+> bodyDoc

and private alterProcedureDoc (cfg: FormattingStyle) (ap: AlterProcedureStatement) : Doc =
    let header = keyword cfg "ALTER" <++> keyword cfg "PROCEDURE" <++> schemaObjectNameDoc cfg ap.ProcedureReference.Name
    let commentMap = getParamTrailingComments ap.Parameters
    let wrapParams = procedureParamsHaveParens (ap :> TSqlStatement) ap.Parameters
    let paramsDoc = ddlParamListDoc cfg ap.Parameters commentMap wrapParams
    let asDoc = keyword cfg "AS"
    let bodyDoc =
        match ap.StatementList with
        | null -> empty
        | stmtList ->
            let stmts = stmtList.Statements |> Seq.map (fun s -> statementDoc cfg s) |> Seq.toList
            join (line <+> line) stmts
    header <+> paramsDoc <+> line <+> asDoc <+> line <+> bodyDoc

and private createProcedureDoc (cfg: FormattingStyle) (cp: CreateProcedureStatement) : Doc =
    let header = keyword cfg "CREATE" <++> keyword cfg "PROCEDURE" <++> schemaObjectNameDoc cfg cp.ProcedureReference.Name
    let commentMap = getParamTrailingComments cp.Parameters
    let wrapParams = procedureParamsHaveParens (cp :> TSqlStatement) cp.Parameters
    let paramsDoc = ddlParamListDoc cfg cp.Parameters commentMap wrapParams
    let asDoc = keyword cfg "AS"
    let bodyDoc =
        match cp.StatementList with
        | null -> empty
        | stmtList ->
            let stmts = stmtList.Statements |> Seq.map (fun s -> statementDoc cfg s) |> Seq.toList
            join (line <+> line) stmts
    header <+> paramsDoc <+> line <+> asDoc <+> line <+> bodyDoc

// ─── Control flow ───

and private beginEndDoc (cfg: FormattingStyle) (be: BeginEndBlockStatement) : Doc =
    let stmts =
        if be.StatementList <> null then
            be.StatementList.Statements |> Seq.map (fun s -> statementDoc cfg s) |> Seq.toList
        else []
    keyword cfg "BEGIN" <+> nest (indentWidth cfg) (line <+> join (line <+> line) stmts) <+> line <+> keyword cfg "END"

and private ifDoc (cfg: FormattingStyle) (ifs: IfStatement) : Doc =
    let cond = exprDoc cfg ifs.Predicate
    let thenDoc = statementDoc cfg ifs.ThenStatement
    let elseDoc =
        if ifs.ElseStatement <> null then
            line <+> keyword cfg "ELSE" <+> line <+> statementDoc cfg ifs.ElseStatement
        else empty
    keyword cfg "IF" <++> cond <+> line <+> thenDoc <+> elseDoc

and private whileDoc (cfg: FormattingStyle) (ws: WhileStatement) : Doc =
    let cond = exprDoc cfg ws.Predicate
    let bodyDoc = statementDoc cfg ws.Statement
    keyword cfg "WHILE" <++> cond <+> line <+> bodyDoc

and private tryCatchDoc (cfg: FormattingStyle) (tc: TryCatchStatement) : Doc =
    let tryStmts =
        if tc.TryStatements <> null then
            tc.TryStatements.Statements |> Seq.map (fun s -> statementDoc cfg s) |> Seq.toList
        else []
    let catchStmts =
        if tc.CatchStatements <> null then
            tc.CatchStatements.Statements |> Seq.map (fun s -> statementDoc cfg s) |> Seq.toList
        else []
    keyword cfg "BEGIN" <++> keyword cfg "TRY" <+>
    nest (indentWidth cfg) (line <+> join (line <+> line) tryStmts) <+> line <+>
    keyword cfg "END" <++> keyword cfg "TRY" <+> line <+>
    keyword cfg "BEGIN" <++> keyword cfg "CATCH" <+>
    nest (indentWidth cfg) (line <+> join (line <+> line) catchStmts) <+> line <+>
    keyword cfg "END" <++> keyword cfg "CATCH"

// ─── Other DML ───

and private insertDoc (cfg: FormattingStyle) (ins: InsertStatement) : Doc =
    let spec = ins.InsertSpecification
    let target =
        if spec.Target <> null then tableRefDoc cfg spec.Target
        else empty
    let header = keyword cfg "INSERT" <++> keyword cfg "INTO" <++> target

    let colsDoc =
        if spec.Columns <> null && spec.Columns.Count > 0 then
            let cols = spec.Columns |> Seq.map (fun c -> columnRefDoc cfg c) |> Seq.toList
            // expandedSplit: ( at end of line, content indented, ) on own line
            let flatDoc = text " (" <+> join (text ", ") cols <+> text ")"
            let expandedDoc =
                text " (" <+> nest (indentWidth cfg) (line <+> join (text "," <+> line) cols) <+> line <+> text ")"
            TSqlFormatter.Doc.Doc.Union(flatDoc, expandedDoc)
        else empty

    let sourceDoc =
        match spec.InsertSource with
        | :? ValuesInsertSource as vis ->
            let rowDoc (rv: RowValue) =
                let vals = rv.ColumnValues |> Seq.map (fun v -> exprDoc cfg v) |> Seq.toList
                // Each row: collapse if short, expand (compactIndented) if long
                let flatRow = text "(" <+> join (text ", ") vals <+> text ")"
                let expandedRow =
                    text "(" <+> nest (indentWidth cfg) (line <+> join (text "," <+> line) vals) <+> line <+> text ")"
                TSqlFormatter.Doc.Doc.Union(flatRow, expandedRow)
            let rows = vis.RowValues |> Seq.map rowDoc |> Seq.toList
            // Multiple rows: comma-separated with line breaks between
            let flatRows = keyword cfg "VALUES" <++> join (text ", ") rows |> flatten
            let expandedRows = keyword cfg "VALUES" <++> join (text ", ") rows
            TSqlFormatter.Doc.Doc.Union(flatRows, expandedRows)
        | :? SelectInsertSource as sis ->
            queryExprDoc cfg sis.Select
        | _ -> tokenStreamDoc cfg spec.InsertSource

    header <+> colsDoc <+> line <+> sourceDoc

and private updateDoc (cfg: FormattingStyle) (upd: UpdateStatement) : Doc =
    let spec = upd.UpdateSpecification
    let target =
        if spec.Target <> null then tableRefDoc cfg spec.Target
        else empty

    let setClauses =
        spec.SetClauses
        |> Seq.map (fun sc ->
            match sc with
            | :? AssignmentSetClause as asc ->
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
            | _ -> tokenStreamDoc cfg sc)
        |> Seq.toList

    let parts =
        [ yield keyword cfg "UPDATE" <++> target
          yield clauseCommaList cfg (keyword cfg "SET") setClauses

          if spec.FromClause <> null && spec.FromClause.TableReferences <> null && spec.FromClause.TableReferences.Count > 0 then
              yield keyword cfg "FROM" <++> tableRefDoc cfg spec.FromClause.TableReferences.[0]

          if spec.WhereClause <> null && spec.WhereClause.SearchCondition <> null then
              yield keyword cfg "WHERE" <++> whereConditionDoc cfg spec.WhereClause.SearchCondition ]

    join line parts

and private deleteDoc (cfg: FormattingStyle) (del: DeleteStatement) : Doc =
    let spec = del.DeleteSpecification
    let target =
        if spec.Target <> null then tableRefDoc cfg spec.Target
        else empty

    let parts =
        [ yield keyword cfg "DELETE" <++> target

          if spec.FromClause <> null && spec.FromClause.TableReferences <> null && spec.FromClause.TableReferences.Count > 0 then
              yield keyword cfg "FROM" <++> tableRefDoc cfg spec.FromClause.TableReferences.[0]

          if spec.WhereClause <> null && spec.WhereClause.SearchCondition <> null then
              yield keyword cfg "WHERE" <++> whereConditionDoc cfg spec.WhereClause.SearchCondition ]

    join line parts

and private mergeDoc (cfg: FormattingStyle) (merge: MergeStatement) : Doc =
    let spec = merge.MergeSpecification
    let target =
        if spec.Target <> null then tableRefDoc cfg spec.Target
        else empty
    let targetAlias =
        if spec.TableAlias <> null then
            text " " <+> keyword cfg "AS" <++> identDoc spec.TableAlias
        else empty
    let source = tableRefDoc cfg spec.TableReference
    let onCondition = whereConditionDoc cfg spec.SearchCondition

    let mergeHeader =
        keyword cfg "MERGE" <++> keyword cfg "INTO" <++> target <+> targetAlias <+> line <+>
        keyword cfg "USING" <++> source <+> line <+>
        nest (indentWidth cfg) (keyword cfg "ON" <++> onCondition)

    let setClauseDoc (sc: SetClause) =
        match sc with
        | :? AssignmentSetClause as asc ->
            let col = columnRefDoc cfg asc.Column
            let value = exprDoc cfg asc.NewValue
            let op =
                match asc.AssignmentKind with
                | AssignmentKind.Equals -> "="
                | AssignmentKind.AddEquals -> "+="
                | AssignmentKind.SubtractEquals -> "-="
                | AssignmentKind.MultiplyEquals -> "*="
                | AssignmentKind.DivideEquals -> "/="
                | _ -> "="
            col <++> text op <++> value
        | _ -> tokenStreamDoc cfg sc

    let actionClauseDoc (ac: MergeActionClause) =
        let conditionKw =
            match ac.Condition with
            | MergeCondition.Matched -> keyword cfg "WHEN" <++> keyword cfg "MATCHED"
            | MergeCondition.NotMatched | MergeCondition.NotMatchedByTarget ->
                keyword cfg "WHEN" <++> keyword cfg "NOT" <++> keyword cfg "MATCHED" <++> keyword cfg "BY" <++> keyword cfg "TARGET"
            | MergeCondition.NotMatchedBySource ->
                keyword cfg "WHEN" <++> keyword cfg "NOT" <++> keyword cfg "MATCHED" <++> keyword cfg "BY" <++> keyword cfg "SOURCE"
            | _ -> keyword cfg "WHEN" <++> keyword cfg "MATCHED"
        let searchDoc =
            if ac.SearchCondition <> null then
                text " " <+> keyword cfg "AND" <++> exprDoc cfg ac.SearchCondition
            else empty
        let actionDoc =
            match ac.Action with
            | :? UpdateMergeAction as u ->
                let setClauses = u.SetClauses |> Seq.map setClauseDoc |> Seq.toList
                keyword cfg "THEN" <+>
                nest (indentWidth cfg) (line <+> clauseCommaList cfg (keyword cfg "UPDATE" <++> keyword cfg "SET") setClauses)
            | :? InsertMergeAction as ins ->
                let cols =
                    if ins.Columns <> null && ins.Columns.Count > 0 then
                        let colDocs = ins.Columns |> Seq.map (fun c -> columnRefDoc cfg c) |> Seq.toList
                        let flatCols = text " (" <+> join (text ", ") colDocs <+> text ")"
                        let expandedCols =
                            text " (" <+> nest (indentWidth cfg) (line <+> join (text "," <+> line) colDocs) <+> line <+> text ")"
                        TSqlFormatter.Doc.Doc.Union(flatCols, expandedCols)
                    else empty
                let vals =
                    let vis = ins.Source
                    let rowDoc (rv: RowValue) =
                        let valDocs = rv.ColumnValues |> Seq.map (fun v -> exprDoc cfg v) |> Seq.toList
                        let flatRow = text "(" <+> join (text ", ") valDocs <+> text ")"
                        let expandedRow =
                            text "(" <+> nest (indentWidth cfg) (line <+> join (text "," <+> line) valDocs) <+> line <+> text ")"
                        TSqlFormatter.Doc.Doc.Union(flatRow, expandedRow)
                    let rows = vis.RowValues |> Seq.map rowDoc |> Seq.toList
                    keyword cfg "VALUES" <++> join (text ", ") rows
                keyword cfg "THEN" <+>
                nest (indentWidth cfg) (line <+> keyword cfg "INSERT" <+> cols <+> line <+> vals)
            | :? DeleteMergeAction ->
                keyword cfg "THEN" <+>
                nest (indentWidth cfg) (line <+> keyword cfg "DELETE")
            | _ -> tokenStreamDoc cfg ac.Action
        conditionKw <+> searchDoc <+> text " " <+> actionDoc
    
    let clauseDocs = spec.ActionClauses |> Seq.map actionClauseDoc |> Seq.toList
    join line (mergeHeader :: clauseDocs)

// ─── Variables ───

and private declareDoc (cfg: FormattingStyle) (decl: DeclareVariableStatement) : Doc =
    let varDocs =
        decl.Declarations |> Seq.map (fun d ->
            let nameDoc = text d.VariableName.Value
            let typeDoc = dataTypeRefDoc cfg d.DataType
            let valueDoc =
                if d.Value <> null then
                    text " " <+> text "=" <++> exprDoc cfg d.Value
                else empty
            nameDoc <++> typeDoc <+> valueDoc
        ) |> Seq.toList
    clauseCommaList cfg (keyword cfg "DECLARE") varDocs

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

and private statementDoc (cfg: FormattingStyle) (stmt: TSqlStatement) : Doc =
    match stmt with
    | :? SelectStatement as ss -> selectStatementDoc cfg ss
    | :? InsertStatement as ins -> insertDoc cfg ins
    | :? UpdateStatement as upd -> updateDoc cfg upd
    | :? DeleteStatement as del -> deleteDoc cfg del
    | :? MergeStatement as merge -> mergeDoc cfg merge
    | :? AlterFunctionStatement as af -> alterFunctionDoc cfg af
    | :? CreateFunctionStatement as cf -> createFunctionDoc cfg cf
    | :? AlterProcedureStatement as ap -> alterProcedureDoc cfg ap
    | :? CreateProcedureStatement as cp -> createProcedureDoc cfg cp
    | :? BeginEndBlockStatement as be -> beginEndDoc cfg be
    | :? IfStatement as ifs -> ifDoc cfg ifs
    | :? WhileStatement as ws -> whileDoc cfg ws
    | :? TryCatchStatement as tc -> tryCatchDoc cfg tc
    | :? DeclareVariableStatement as dv -> declareDoc cfg dv
    | :? SetVariableStatement as sv -> setVarDoc cfg sv
    | :? ReturnStatement as rs ->
        if rs.Expression <> null then
            keyword cfg "RETURN" <++> exprDoc cfg rs.Expression
        else
            // Check if this is a RETURN followed by a SELECT (inline TVF pattern)
            // ScriptDOM may parse "RETURN SELECT ..." with Expression = null
            // In that case, fall back to token stream
            tokenStreamDoc cfg rs
    | :? PrintStatement as ps ->
        keyword cfg "PRINT" <++> exprDoc cfg ps.Expression
    | :? ExecuteStatement as es -> tokenStreamDoc cfg es
    | :? RaiseErrorStatement as re -> tokenStreamDoc cfg re
    | :? ThrowStatement as ts -> tokenStreamDoc cfg ts
    | _ -> tokenStreamDoc cfg stmt

// ─── Top-level: handle RETURN SELECT for inline TVFs ───

/// Special handling for AlterFunctionStatement with inline TVF (RETURN SELECT)
and private handleInlineTvf (cfg: FormattingStyle) (af: AlterFunctionStatement) : Doc =
    let header = keyword cfg "ALTER" <++> keyword cfg "FUNCTION" <++> schemaObjectNameDoc cfg af.Name
    let commentMap = getParamTrailingComments af.Parameters
    let paramsDoc = ddlParamListDoc cfg af.Parameters commentMap true
    let returnsDoc = keyword cfg "RETURNS" <++> keyword cfg "TABLE"
    let asDoc = keyword cfg "AS"

    // For inline TVFs, we need to find the RETURN SELECT in the token stream
    let stream = af.ScriptTokenStream
    let returnIdxOpt =
        if stream <> null then
            seq { af.FirstTokenIndex .. af.LastTokenIndex }
            |> Seq.tryFind (fun i -> stream.[i].TokenType = TSqlTokenType.Return)
        else None

    match returnIdxOpt with
    | Some returnIdx ->
        // Skip whitespace after RETURN to find the SELECT start
        let selectStart =
            seq { returnIdx + 1 .. af.LastTokenIndex }
            |> Seq.skipWhile (fun i -> stream.[i].TokenType = TSqlTokenType.WhiteSpace)
            |> Seq.tryHead
            |> Option.defaultValue (returnIdx + 1)

        // Build RETURN + SELECT doc from the remaining tokens
        let selectSql =
            seq { selectStart .. af.LastTokenIndex }
            |> Seq.map (fun i -> stream.[i].Text)
            |> String.concat ""
            |> fun s -> s.Trim()

        let parser = TSql160Parser(true)
        use reader = new StringReader(selectSql)
        let fragment, errors = parser.Parse(reader)
        let selectDoc =
            if errors.Count = 0 then
                match fragment with
                | :? TSqlScript as script when script.Batches.Count > 0 && script.Batches.[0].Statements.Count > 0 ->
                    statementDoc cfg script.Batches.[0].Statements.[0]
                | _ -> text selectSql
            else text selectSql

        header <+> paramsDoc <+> line <+> returnsDoc <+> line <+> asDoc <+> line <+> keyword cfg "RETURN" <++> selectDoc
    | None ->
        header <+> paramsDoc <+> line <+> returnsDoc <+> line <+> asDoc

// ─── Top-level format function ───

/// Collect leading comments from the token stream before the first statement.
let private leadingCommentsDoc (script: TSqlScript) : Doc =
    if script.Batches.Count = 0 then empty
    else
        let firstBatch = script.Batches.[0]
        if firstBatch.Statements.Count = 0 then empty
        else
            let firstStmt = firstBatch.Statements.[0]
            let stream = firstStmt.ScriptTokenStream
            if stream = null then empty
            else
                let comments =
                    [ for i in 0 .. firstStmt.FirstTokenIndex - 1 do
                        let tok = stream.[i]
                        if tok.TokenType = TSqlTokenType.SingleLineComment ||
                           tok.TokenType = TSqlTokenType.MultilineComment then
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
    let mutable errors = null : System.Collections.Generic.IList<ParseError>
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
                        |> Seq.map (fun stmt -> statementDoc config stmt)
                        |> Seq.toList
                    join (line <+> line) stmtDocs
                )
                |> Seq.toList
            let result = leadingComments <+> join (line <+> keyword config "GO" <+> line) batchDocs
            Ok (render config.whitespace.wrapLinesLongerThan result)
        | _ ->
            Ok (render config.whitespace.wrapLinesLongerThan (tokenStreamDoc config fragment))
