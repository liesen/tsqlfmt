module TableReferenceTests

open Xunit
open TSqlFormatter.Style
open TestSupport

[<Fact>]
let ``query derived table keeps the inner query parenthesized and aliases it`` () =
    let sql = "select dt.a from (select a from dbo.t where b = 1) dt"

    let expected =
        """
SELECT dt.a
FROM (
    SELECT a
    FROM dbo.t
    WHERE b = 1
) dt
"""

    assertFormatsTo expected sql

[<Fact>]
let ``schema object function table reference keeps arguments and alias`` () =
    let sql = "select f.a from dbo.fn(1, 2) f"

    let expected =
        """
SELECT f.a
FROM dbo.fn(1, 2) f
"""

    assertFormatsTo expected sql

[<Fact>]
let ``schema object function table reference lays out many arguments vertically`` () =
    let testConfig =
        { config with
            functionCalls =
                { config.functionCalls with
                    placeArgumentsOnNewLines = PlaceOnNewLine.Always }
            whitespace =
                { config.whitespace with
                    wrapLinesLongerThan = 25 } }

    let sql = "select f.a from dbo.fn(111, 222, 333, 444) f"

    let expected =
        """
SELECT f.a
FROM dbo.fn(
    111,
    222,
    333,
    444
) f
"""

    assertFormatsToWithConfig testConfig expected sql
