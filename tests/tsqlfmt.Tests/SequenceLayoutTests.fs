module SequenceLayoutTests

open Xunit
open TSqlFormatter.Doc
open TSqlFormatter.Formatter

let private renderDoc (doc: Doc) =
    render 120 doc

[<Fact>]
let ``sequenceDoc indents the first item when configured`` () =
    let policy =
        { placeFirstItemOnNewLine = false
          firstItemIndent = Some 4
          subsequentItemsIndent = Some 4 }
    let items = [ text "a IN (" <+> line <+> text "SELECT 1" <+> line <+> text ")"; text "AND b = 2" ]
    let actual = renderDoc (sequenceDoc policy items)
    let expected =
        """
    a IN (
    SELECT 1
    )
    AND b = 2
"""

    Assert.Equal(expected.Trim(), actual)

[<Fact>]
let ``sequenceDoc leaves subsequent items flush when policy has no subsequent indent`` () =
    let policy =
        { placeFirstItemOnNewLine = false
          firstItemIndent = None
          subsequentItemsIndent = None }
    let items = [ text "a = 1"; text "AND b = 2"; text "AND c = 3" ]
    let actual = renderDoc (sequenceDoc policy items)
    let expected =
        """
a = 1
AND b = 2
AND c = 3
"""

    Assert.Equal(expected.Trim(), actual)

[<Fact>]
let ``headedSequenceDoc places the first item on a new line when policy requests it`` () =
    let policy =
        { placeFirstItemOnNewLine = true
          firstItemIndent = None
          subsequentItemsIndent = Some 4 }
    let items = [ text "a,"; text "b,"; text "c" ]
    let actual = renderDoc (headedSequenceDoc policy (text "SELECT") items)
    let expected =
        """
SELECT
    a,
    b,
    c
"""

    Assert.Equal(expected.Trim(), actual)

[<Fact>]
let ``headedSequenceDoc keeps the first item after the head when policy allows it`` () =
    let policy =
        { placeFirstItemOnNewLine = false
          firstItemIndent = None
          subsequentItemsIndent = Some 4 }
    let items = [ text "a = 1"; text "AND b = 2" ]
    let actual = renderDoc (headedSequenceDoc policy (text "WHERE") items)
    let expected =
        """
WHERE a = 1
    AND b = 2
"""

    Assert.Equal(expected.Trim(), actual)
