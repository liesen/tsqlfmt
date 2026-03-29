module ProcedureParameterTests

open Xunit
open TestSupport

// ─── CREATE PROCEDURE without parentheses ───

[<Fact>]
let ``proc single param no parens stays inline`` () =
    let sql = "CREATE PROCEDURE f @x INT AS SELECT 1"

    let expected =
        """CREATE PROCEDURE f @x INT
AS
SELECT 1"""

    assertFormatsTo expected sql

[<Fact>]
let ``proc two params no parens go multiline`` () =
    let sql = "CREATE PROCEDURE f @x INT, @y INT AS SELECT 1"

    let expected =
        """CREATE PROCEDURE f
    @x INT,
    @y INT
AS
SELECT 1"""

    assertFormatsTo expected sql

[<Fact>]
let ``proc many params no parens go multiline`` () =
    let sql =
        "CREATE PROCEDURE dbo.MyProc @FirstName VARCHAR(50), @LastName VARCHAR(50), @Age INT = 25, @Active BIT OUTPUT AS SELECT 1"

    let expected =
        """CREATE PROCEDURE dbo.MyProc
    @FirstName VARCHAR(50),
    @LastName VARCHAR(50),
    @Age INT = 25,
    @Active BIT OUTPUT
AS
SELECT 1"""

    assertFormatsTo expected sql

// ─── CREATE PROCEDURE with parentheses ───

[<Fact>]
let ``proc single param with parens collapses inline with space before paren`` () =
    let sql = "CREATE PROCEDURE f (@x INT) AS SELECT 1"

    let expected =
        """CREATE PROCEDURE f (@x INT)
AS
SELECT 1"""

    assertFormatsTo expected sql

[<Fact>]
let ``proc two params with parens collapses inline when short`` () =
    let sql = "CREATE PROCEDURE f (@x INT, @y INT) AS SELECT 1"

    let expected =
        """CREATE PROCEDURE f (@x INT, @y INT)
AS
SELECT 1"""

    assertFormatsTo expected sql

[<Fact>]
let ``proc many params with parens expands with paren on same line`` () =
    let sql =
        "CREATE PROCEDURE dbo.MyProc (@FirstName VARCHAR(50), @LastName VARCHAR(50), @Age INT = 25, @Active BIT OUTPUT) AS SELECT 1"

    let expected =
        """CREATE PROCEDURE dbo.MyProc (
    @FirstName VARCHAR(50),
    @LastName VARCHAR(50),
    @Age INT = 25,
    @Active BIT OUTPUT
)
AS
SELECT 1"""

    assertFormatsTo expected sql

// ─── CREATE FUNCTION ───

[<Fact>]
let ``func single param collapses inline with space before paren`` () =
    let sql = "CREATE FUNCTION dbo.f (@x INT) RETURNS INT AS BEGIN RETURN @x END"

    let expected =
        """CREATE FUNCTION dbo.f (@x INT)
RETURNS INT
AS
BEGIN
    RETURN @x
END"""

    assertFormatsTo expected sql

[<Fact>]
let ``func two params collapses inline when short`` () =
    let sql =
        "CREATE FUNCTION dbo.f (@x INT, @y INT) RETURNS INT AS BEGIN RETURN @x + @y END"

    let expected =
        """CREATE FUNCTION dbo.f (@x INT, @y INT)
RETURNS INT
AS
BEGIN
    RETURN @x + @y
END"""

    assertFormatsTo expected sql

[<Fact>]
let ``func many params expands with paren on same line`` () =
    let sql =
        "CREATE FUNCTION dbo.MyFunc (@FirstName VARCHAR(50), @LastName VARCHAR(50), @Age INT = 25, @Active BIT) RETURNS INT AS BEGIN RETURN 1 END"

    let expected =
        """CREATE FUNCTION dbo.MyFunc (
    @FirstName VARCHAR(50),
    @LastName VARCHAR(50),
    @Age INT = 25,
    @Active BIT
)
RETURNS INT
AS
BEGIN
    RETURN 1
END"""

    assertFormatsTo expected sql

// ─── ALTER PROCEDURE ───

[<Fact>]
let ``alter proc single param with parens collapses inline`` () =
    let sql = "ALTER PROCEDURE f (@x INT) AS SELECT 1"

    let expected =
        """ALTER PROCEDURE f (@x INT)
AS
SELECT 1"""

    assertFormatsTo expected sql

[<Fact>]
let ``alter proc many params with parens expands`` () =
    let sql =
        "ALTER PROCEDURE dbo.MyProc (@FirstName VARCHAR(50), @LastName VARCHAR(50), @Age INT = 25, @Active BIT OUTPUT) AS SELECT 1"

    let expected =
        """ALTER PROCEDURE dbo.MyProc (
    @FirstName VARCHAR(50),
    @LastName VARCHAR(50),
    @Age INT = 25,
    @Active BIT OUTPUT
)
AS
SELECT 1"""

    assertFormatsTo expected sql

// ─── ALTER FUNCTION ───

[<Fact>]
let ``alter func single param collapses inline`` () =
    let sql = "ALTER FUNCTION dbo.f (@x INT) RETURNS INT AS BEGIN RETURN @x END"

    let expected =
        """ALTER FUNCTION dbo.f (@x INT)
RETURNS INT
AS
BEGIN
    RETURN @x
END"""

    assertFormatsTo expected sql
