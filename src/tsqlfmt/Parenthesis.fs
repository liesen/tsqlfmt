module TSqlFormatter.Parenthesis

open TSqlFormatter.Doc
open TSqlFormatter.Style

type ParenthesisCombinator = Doc -> Doc -> Doc

let private indentWidth (cfg: Style) = cfg.whitespace.numberOfSpacesInTabs

let private expandedSplit (indentWidth: int) (indentContents: bool) : ParenthesisCombinator =
    fun anchorDoc contentsDoc ->
        let laidOutContents =
            if indentContents then
                nest indentWidth (softline <+> contentsDoc)
            else
                softline <+> contentsDoc

        anchorDoc <+> text "(" <+> laidOutContents <+> softline <+> text ")"

let private compactIndented (indentWidth: int) (indentContents: bool) : ParenthesisCombinator =
    fun anchorDoc contentsDoc ->
        let laidOutContents =
            if indentContents then
                nest indentWidth (softline <+> contentsDoc)
            else
                softline <+> contentsDoc

        anchorDoc <+> text "(" <+> laidOutContents <+> softline <+> text ")"

let private parenthesisDoc (indentWidth: int) (indentContents: bool) (style: ParenthesisStyle) : ParenthesisCombinator =
    match style with
    | ParenthesisStyle.ExpandedSplit -> expandedSplit indentWidth indentContents
    | ParenthesisStyle.CompactIndented -> compactIndented indentWidth indentContents
    | unsupported -> failwithf "Unsupported parenthesis style: %A" unsupported

let parenthesesDoc (cfg: Style) : ParenthesisCombinator =
    parenthesisDoc (indentWidth cfg) cfg.parentheses.indentParenthesesContents cfg.parentheses.parenthesisStyle

let ddlDoc (cfg: Style) : ParenthesisCombinator =
    parenthesisDoc (indentWidth cfg) cfg.ddl.indentParenthesesContents cfg.ddl.parenthesisStyle

let cteDoc (cfg: Style) : ParenthesisCombinator =
    parenthesisDoc (indentWidth cfg) cfg.cte.indentContents cfg.cte.parenthesisStyle

let insertColumnsDoc (cfg: Style) : ParenthesisCombinator =
    parenthesisDoc
        (indentWidth cfg)
        cfg.insertStatements.columns.indentContents
        cfg.insertStatements.columns.parenthesisStyle

let insertValuesDoc (cfg: Style) : ParenthesisCombinator =
    parenthesisDoc
        (indentWidth cfg)
        cfg.insertStatements.values.indentContents
        cfg.insertStatements.values.parenthesisStyle
