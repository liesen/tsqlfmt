module ParenthesisTests

open Xunit
open TSqlFormatter.Doc
open TSqlFormatter.Parenthesis
open TSqlFormatter.Style
open TestSupport

[<Fact>]
let ``expressionParensDoc respects generic indentation setting`` () =
    let testConfig =
        { config with
            parentheses =
                { config.parentheses with
                    parenthesisStyle = ParenthesisStyle.ExpandedSplit
                    indentParenthesesContents = true } }

    let doc = expressionParensDoc testConfig (text "a," <+> line <+> text "b")

    let expected =
        """
(
    a,
    b
)
"""

    assertRenderedDoc testConfig.whitespace.wrapLinesLongerThan expected doc

[<Fact>]
let ``ddlParensDoc uses ddl style bucket instead of generic parenthesis settings`` () =
    let testConfig =
        { config with
            parentheses =
                { config.parentheses with
                    indentParenthesesContents = false }
            ddl =
                { config.ddl with
                    parenthesisStyle = ParenthesisStyle.ExpandedSplit
                    indentParenthesesContents = true } }

    let doc = ddlParensDoc testConfig (text "a," <+> line <+> text "b")

    let expected =
        """
(
    a,
    b
)
"""

    assertRenderedDoc testConfig.whitespace.wrapLinesLongerThan expected doc
