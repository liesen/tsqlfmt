module BooleanExpressionTests

open Xunit
open TSqlFormatter.Style
open TestSupport

[<Fact>]
let ``headed condition indents multiline first item without overindenting following AND`` () =
    let testConfig =
        { config with
            dml =
                { config.dml with
                    collapseShortSubqueries = false } }

    let sql =
        "select 1 from dbo.t where a in (select 1 from dbo.u where u.id = t.id) and b = 2"

    let expected =
        """
SELECT 1
FROM dbo.t
WHERE a IN (
        SELECT 1
        FROM dbo.u
        WHERE u.id = t.id
    )
    AND b = 2
"""

    assertFormatsToWithConfig testConfig expected sql

[<Fact>]
let ``headed IF condition formats EXISTS like other subquery predicates`` () =
    let sql = "if exists (select 1 from sys.tables where name = 'test24') select 1"

    let expected =
        """
IF EXISTS (
    SELECT 1
    FROM sys.tables
    WHERE name = 'test24'
)
    SELECT 1
"""

    assertFormatsTo expected sql

[<Fact>]
let ``standalone boolean expression keeps normal AND indentation`` () =
    let sql = "select 1 from tbl where x = 1 and y = 2 and z = 3"

    let expected =
        """
SELECT 1
FROM tbl
WHERE x = 1
    AND y = 2
    AND z = 3
"""

    assertFormatsTo expected sql

[<Fact>]
let ``IIF uses structured argument layout`` () =
    let testConfig =
        { config with
            lists =
                { config.lists with
                    placeFirstItemOnNewLine = PlaceOnNewLine.Always
                    indentListItems = true }
            functionCalls =
                { config.functionCalls with
                    placeArgumentsOnNewLines = PlaceOnNewLine.Always } }

    let sql = "select iif(a = 1 and b = 2, 'yes', 'no') as result"

    let expected =
        """
SELECT
    IIF(
        a = 1
            AND b = 2,
        'yes',
        'no'
    ) AS result
"""

    assertFormatsToWithConfig testConfig expected sql

[<Fact>]
let ``NOT over a parenthesized boolean expression keeps generic boolean formatting`` () =
    let sql = "select 1 where not (a = 1 and b = 2 or c = 3)"

    let expected =
        """
SELECT 1
WHERE NOT (a = 1 AND b = 2 OR c = 3)
"""

    assertFormatsTo expected sql

[<Fact>]
let ``plain boolean expression formats with normal AND indentation`` () =
    let expected =
        """
a = 1
    AND b = 2
    AND c = 3
"""

    assertBooleanExpressionDoc expected "a = 1 and b = 2 and c = 3"

[<Fact>]
let ``plain parenthesized boolean expression stays inline when it fits`` () =
    let expected = "NOT (a = 1 AND b = 2 OR c = 3)"
    assertBooleanExpressionDoc expected "not (a = 1 and b = 2 or c = 3)"

[<Fact>]
let ``IN value list uses operators.in placement settings`` () =
    let testConfig =
        { config with
            operators =
                { config.operators with
                    ``in`` =
                        { config.operators.``in`` with
                            placeOpeningParenthesisOnNewLine = true
                            alignment = InAlignment.Indented
                            addSpaceAroundInContents = false } } }

    let sql = "select 1 where a in (1, 2, 3)"

    let expected =
        """
SELECT 1
WHERE a IN (
    1,
    2,
    3
)
"""

    assertFormatsToWithConfig testConfig expected sql
