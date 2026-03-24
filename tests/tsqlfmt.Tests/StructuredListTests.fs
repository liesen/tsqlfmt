module StructuredListTests

open Xunit
open TSqlFormatter.Style
open TestSupport

[<Fact>]
let ``table hints use structured list formatting`` () =
    let testConfig =
        { config with
            lists =
                { config.lists with
                    placeFirstItemOnNewLine = PlaceOnNewLine.Always
                    indentListItems = true } }

    let sql = "SELECT * FROM dbo.t WITH (NOLOCK, READPAST, HOLDLOCK)"

    let expected =
        """SELECT
    *
FROM dbo.t (NOLOCK, READPAST, HOLDLOCK)"""

    assertFormatsToWithConfig testConfig expected sql

[<Fact>]
let ``function options use structured list formatting`` () =
    let testConfig =
        { config with
            lists =
                { config.lists with
                    placeFirstItemOnNewLine = PlaceOnNewLine.Always
                    indentListItems = true } }

    let sql =
        "CREATE FUNCTION dbo.f() RETURNS INT WITH EXECUTE AS CALLER, INLINE = ON AS BEGIN RETURN 1 END"

    let expected =
        """CREATE FUNCTION dbo.f()
WITH
    EXECUTE AS CALLER,
    INLINE = ON
RETURNS INT
AS
BEGIN
    RETURN 1
END"""

    assertFormatsToWithConfig testConfig expected sql

[<Fact>]
let ``order hint uses structured list formatting`` () =
    let testConfig =
        { config with
            lists =
                { config.lists with
                    placeFirstItemOnNewLine = PlaceOnNewLine.Always
                    indentListItems = true } }

    let sql =
        "CREATE FUNCTION dbo.f() RETURNS TABLE (c1 INT, c2 INT, c3 INT) ORDER (c1 DESC, c2, c3 ASC) AS EXTERNAL NAME MyAssembly.MyClass.MyMethod"

    let expected =
        """CREATE FUNCTION dbo.f()
RETURNS TABLE (
    c1 INT,
    c2 INT,
    c3 INT
)
ORDER (c1 DESC, c2, c3 ASC)
AS
EXTERNAL NAME MyAssembly.MyClass.MyMethod"""

    assertFormatsToWithConfig testConfig expected sql
