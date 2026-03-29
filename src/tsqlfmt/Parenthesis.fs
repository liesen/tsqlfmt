module TSqlFormatter.Parenthesis

open TSqlFormatter.Doc
open TSqlFormatter.Style

type ParenthesisCombinator = Doc -> Doc

let private optNest (indent: int option) (doc: Doc) : Doc =
    match indent with
    | Some spaces -> nest spaces doc
    | None -> doc

let private expandedSplit (contentsIndent: int option) (contentsDoc: Doc) =
    let laidOutContents = optNest contentsIndent (softline <+> contentsDoc)

    text "(" <+> laidOutContents <+> softline <+> text ")"

let private compactIndented (contentsIndent: int option) (contentsDoc: Doc) =
    let laidOutContents = optNest contentsIndent (softline <+> contentsDoc)

    text "(" <+> laidOutContents <+> softline <+> text ")"

let private parenthesisDoc (contentsIndent: int option) (style: ParenthesisStyle) : ParenthesisCombinator =
    match style with
    | ParenthesisStyle.ExpandedSplit -> expandedSplit contentsIndent
    | ParenthesisStyle.CompactIndented -> compactIndented contentsIndent
    | unsupported -> failwithf "Unsupported parenthesis style: %A" unsupported

let private optWhen condition value = if condition then Some value else None

let private cteColumnAlignmentIndent (cfg: Style) =
    match cfg.cte.columnAlignment with
    | Alignment.Indented -> Some cfg.whitespace.numberOfSpacesInTabs
    | _ -> None

/// Use for generic parenthesized expressions driven by `cfg.parentheses`.
/// Example: `(a + b)`, `EXISTS (SELECT 1 ...)`, `(SELECT ... UNION SELECT ...)`.
let expressionParensDoc (cfg: Style) : ParenthesisCombinator =
    parenthesisDoc
        (optWhen cfg.parentheses.indentParenthesesContents cfg.whitespace.numberOfSpacesInTabs)
        cfg.parentheses.parenthesisStyle

/// Use for DDL definition-style lists driven by `cfg.ddl`.
/// Example: `CREATE TABLE t (a int, b int)` or `CONSTRAINT pk PRIMARY KEY (id)`.
let ddlParensDoc (cfg: Style) : ParenthesisCombinator =
    parenthesisDoc
        (optWhen cfg.ddl.indentParenthesesContents cfg.whitespace.numberOfSpacesInTabs)
        cfg.ddl.parenthesisStyle

/// Use for the parenthesized CTE body after `AS`, driven by `cfg.cte`.
/// Example: `WITH cte AS (SELECT ... FROM ...)`.
let cteBodyParensDoc (cfg: Style) : ParenthesisCombinator =
    parenthesisDoc (optWhen cfg.cte.indentContents cfg.whitespace.numberOfSpacesInTabs) cfg.cte.parenthesisStyle

/// Use for the optional CTE column list driven by `cfg.cte.placeColumnsOnNewLine` and `cfg.cte.columnAlignment`.
/// Example: `WITH cte (col1, col2) AS ...`.
let cteColumnListParensDoc (cfg: Style) columnsDoc : Doc =
    let columnIndent = cteColumnAlignmentIndent cfg

    if cfg.cte.placeColumnsOnNewLine then
        text "(" <+> optNest columnIndent (line <+> columnsDoc) <+> line <+> text ")"
    else
        text "(" <+> columnsDoc <+> text ")"

/// Use for insert target column lists driven by `cfg.insertStatements.columns`.
/// Example: `INSERT INTO t (a, b, c)`.
let insertColumnListParensDoc (cfg: Style) : ParenthesisCombinator =
    parenthesisDoc
        (optWhen cfg.insertStatements.columns.indentContents cfg.whitespace.numberOfSpacesInTabs)
        cfg.insertStatements.columns.parenthesisStyle

/// Use for insert row value tuples driven by `cfg.insertStatements.values`.
/// Example: `VALUES (1, 'x', GETDATE())`.
let insertValuesListParensDoc (cfg: Style) : ParenthesisCombinator =
    parenthesisDoc
        (optWhen cfg.insertStatements.values.indentContents cfg.whitespace.numberOfSpacesInTabs)
        cfg.insertStatements.values.parenthesisStyle

/// Use for function-call argument lists driven by `cfg.functionCalls`.
/// Example: `COUNT(*)`, `dbo.fn(a, b)`, `COALESCE(x, y, z)`.
let functionCallParensDoc (cfg: Style) (argDocs: Doc list) : Doc =
    let openDoc =
        if cfg.functionCalls.addSpacesAroundParentheses then
            text " ("
        else
            text "("

    match argDocs with
    | [] when cfg.functionCalls.addSpaceBetweenEmptyParentheses -> openDoc <++> text ")"
    | [] -> openDoc <+> text ")"
    | _ ->
        let openDoc, closeDoc =
            if cfg.functionCalls.addSpacesAroundArgumentList then
                openDoc <+> text " ", text " )"
            else
                openDoc, text ")"

        if cfg.functionCalls.placeArgumentsOnNewLines = PlaceOnNewLine.Never then
            openDoc <+> join (text ", ") argDocs <+> closeDoc
        else
            let argListSpacing =
                if cfg.functionCalls.addSpacesAroundArgumentList then
                    line
                else
                    softline

            let doc =
                openDoc
                <+> nest cfg.whitespace.numberOfSpacesInTabs (argListSpacing <+> join (text "," <+> line) argDocs)
                <+> argListSpacing
                <+> closeDoc

            match cfg.functionCalls.placeArgumentsOnNewLines with
            | PlaceOnNewLine.Always -> doc
            | _ -> group doc
