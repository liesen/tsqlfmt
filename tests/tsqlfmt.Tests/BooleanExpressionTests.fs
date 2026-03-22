module BooleanExpressionTests

open Xunit
open TSqlFormatter.Config
open TestSupport

[<Fact>]
let ``headed condition indents multiline first item without overindenting following AND`` () =
    let testConfig =
        { config with
            dml =
                { config.dml with
                    collapseShortSubqueries = false } }
    let sql = "select 1 from dbo.t where a in (select 1 from dbo.u where u.id = t.id) and b = 2"
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
