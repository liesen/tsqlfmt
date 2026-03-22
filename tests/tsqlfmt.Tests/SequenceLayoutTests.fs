module SequenceLayoutTests

open Xunit
open TSqlFormatter.Doc
open TSqlFormatter.Formatter
open TestSupport

[<Fact>]
let ``sequenceDoc indents the first item when configured`` () =
    let policy =
        { placeFirstItemOnNewLine = false
          firstItemIndent = Some 4
          subsequentItemsIndent = Some 4 }

    let items =
        [ text "a IN (" <+> line <+> text "SELECT 1" <+> line <+> text ")"
          text "AND b = 2" ]

    let expected =
        """
    a IN (
    SELECT 1
    )
    AND b = 2
"""

    assertRenderedDoc 120 expected (sequenceDoc policy items)

[<Fact>]
let ``sequenceDoc leaves subsequent items flush when policy has no subsequent indent`` () =
    let policy =
        { placeFirstItemOnNewLine = false
          firstItemIndent = None
          subsequentItemsIndent = None }

    let items = [ text "a = 1"; text "AND b = 2"; text "AND c = 3" ]

    let expected =
        """
a = 1
AND b = 2
AND c = 3
"""

    assertRenderedDoc 120 expected (sequenceDoc policy items)

[<Fact>]
let ``headedSequenceDoc places the first item on a new line when policy requests it`` () =
    let policy =
        { placeFirstItemOnNewLine = true
          firstItemIndent = None
          subsequentItemsIndent = Some 4 }

    let items = [ text "a,"; text "b,"; text "c" ]

    let expected =
        """
SELECT
    a,
    b,
    c
"""

    assertRenderedDoc 120 expected (headedSequenceDoc policy (text "SELECT") items)

[<Fact>]
let ``headedSequenceDoc keeps the first item after the head when policy allows it`` () =
    let policy =
        { placeFirstItemOnNewLine = false
          firstItemIndent = None
          subsequentItemsIndent = Some 4 }

    let items = [ text "a = 1"; text "AND b = 2" ]

    let expected =
        """
WHERE a = 1
    AND b = 2
"""

    assertRenderedDoc 120 expected (headedSequenceDoc policy (text "WHERE") items)

[<Fact>]
let ``render trims indentation on blank lines in nested docs`` () =
    let doc =
        text "BEGIN" <+> nest 4 (line <+> line <+> text "x = 1") <+> line <+> text "END"

    let expected =
        """
BEGIN

    x = 1
END
"""

    assertRenderedDoc 120 expected doc
