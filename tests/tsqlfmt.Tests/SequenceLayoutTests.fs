module SequenceLayoutTests

open Xunit
open TSqlFormatter.Doc
open TSqlFormatter.Formatter
open TSqlFormatter.Lists
open TSqlFormatter.Style
open TestSupport

[<Fact>]
let ``sequenceDoc indents the first item when configured`` () =
    let layout =
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

    assertRenderedDoc 120 expected (sequenceDoc layout items)

[<Fact>]
let ``sequenceDoc leaves subsequent items flush when layout has no subsequent indent`` () =
    let layout =
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

    assertRenderedDoc 120 expected (sequenceDoc layout items)

[<Fact>]
let ``anchoredSequenceDoc places the first item on a new line when layout requests it`` () =
    let layout =
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

    assertRenderedDoc 120 expected (anchoredSequenceDoc layout (text "SELECT") items)

[<Fact>]
let ``anchoredSequenceDoc keeps the first item after the head when layout allows it`` () =
    let layout =
        { placeFirstItemOnNewLine = false
          firstItemIndent = None
          subsequentItemsIndent = Some 4 }

    let items = [ text "a = 1"; text "AND b = 2" ]

    let expected =
        """
WHERE a = 1
    AND b = 2
"""

    assertRenderedDoc 120 expected (anchoredSequenceDoc layout (text "WHERE") items)

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

[<Fact>]
let ``decorateListItems respects leading comma style`` () =
    let testConfig =
        { config with
            lists =
                { config.lists with
                    placeCommasBeforeItems = true
                    addSpaceBeforeComma = false } }

    let items = [ text "a"; text "b"; text "c" ]

    let expected =
        """
a
,b
,c
"""

    assertRenderedDoc 120 expected (join line (decorateListItems testConfig items))

[<Fact>]
let ``decorateListItems respects space before comma`` () =
    let testConfig =
        { config with
            lists =
                { config.lists with
                    placeCommasBeforeItems = false
                    addSpaceBeforeComma = true } }

    let items = [ text "a"; text "b"; text "c" ]

    let expected =
        """
a ,
b ,
c
"""

    assertRenderedDoc 120 expected (join line (decorateListItems testConfig items))

[<Fact>]
let ``decorateDdlListItems keeps trailing commas without extra spacing`` () =
    let items = [ text "a"; text "b"; text "c" ]

    let expected =
        """
a,
b,
c
"""

    assertRenderedDoc 120 expected (join line (decorateDdlListItems items))

[<Fact>]
let ``anchoredSequenceDoc equals anchor <++> commaListDoc when placeFirstItemOnNewLine is false`` () =
    // When placeFirstItemOnNewLine = false, the anchored helper just concatenates
    // the anchor with the sequence via <++>.  This means the caller can use either
    // anchoredCommaSeparatedListDoc or manually write anchor <++> commaListDoc
    // and get identical output.  Anchoring only matters when placeFirstItemOnNewLine = true.
    let items = [ text "a"; text "b"; text "c" ]
    let anchorDoc = text "ORDER BY"

    let anchored = anchoredCommaSeparatedListDoc config anchorDoc items
    let manual = anchorDoc <++> commaListDoc config items

    let width = 120
    let anchoredRendered = render width anchored
    let manualRendered = render width manual
    Assert.Equal(anchoredRendered, manualRendered)

[<Fact>]
let ``anchoredSequenceDoc breaks after keyword when placeFirstItemOnNewLine is true`` () =
    // placeFirstItemOnNewLine controls the break between a keyword and the first
    // list item (SQL Prompt applies it to SELECT, PARTITION BY, ORDER BY, etc.).
    // Only anchoredSequenceDoc can honour this — it sees the keyword and inserts a
    // line break before the first item.
    let items = [ text "a"; text "b"; text "c" ]
    let anchorDoc = text "SELECT"

    let newLineConfig =
        { config with
            lists =
                { config.lists with
                    placeFirstItemOnNewLine = PlaceOnNewLine.Always } }

    assertRenderedDoc
        120
        """
SELECT
    a,
    b,
    c
"""
        (anchoredCommaSeparatedListDoc newLineConfig anchorDoc items)

[<Fact>]
let ``commaListDoc ignores placeFirstItemOnNewLine because it has no keyword anchor`` () =
    // commaListDoc (sequenceDoc) formats the item list without knowledge of the
    // preceding keyword, so placeFirstItemOnNewLine has no effect — the first
    // item stays on the same line as whatever precedes the list.
    let items = [ text "a"; text "b"; text "c" ]
    let anchorDoc = text "SELECT"

    let newLineConfig =
        { config with
            lists =
                { config.lists with
                    placeFirstItemOnNewLine = PlaceOnNewLine.Always } }

    assertRenderedDoc
        120
        """
SELECT a,
    b,
    c
"""
        (anchorDoc <++> commaListDoc newLineConfig items)
