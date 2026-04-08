module DdlTests

open Xunit
open TestSupport
open TSqlFormatter.Style

let private narrowConfig =
    { config with
        whitespace =
            { config.whitespace with
                wrapLinesLongerThan = 60 } }

let private ddlTestConfig =
    { narrowConfig with
        ddl =
            { narrowConfig.ddl with
                parenthesisStyle = ParenthesisStyle.ExpandedSplit
                indentParenthesesContents = true } }

[<Fact>]
let ``constraint columns keep single column inline when if longer or multiple columns`` () =
    let testConfig =
        { ddlTestConfig with
            ddl =
                { ddlTestConfig.ddl with
                    placeConstraintColumnsOnNewLines = DdlConstraintColumnsOnNewLines.IfLongerOrMultipleColumns } }

    let sql = "CREATE TABLE dbo.t (CONSTRAINT PK_t PRIMARY KEY (id))"

    let expected =
        """CREATE TABLE dbo.t (
    CONSTRAINT PK_t PRIMARY KEY (
        id
    )
)"""

    assertFormatsToWithConfig testConfig expected sql

[<Fact>]
let ``constraint columns break when multiple and if longer or multiple columns`` () =
    let testConfig =
        { ddlTestConfig with
            ddl =
                { ddlTestConfig.ddl with
                    placeConstraintColumnsOnNewLines = DdlConstraintColumnsOnNewLines.IfLongerOrMultipleColumns } }

    let sql = "CREATE TABLE dbo.t (CONSTRAINT PK_t PRIMARY KEY (id, tenant_id))"

    let expected =
        """CREATE TABLE dbo.t (
    CONSTRAINT PK_t PRIMARY KEY (
        id,
        tenant_id
    )
)"""

    assertFormatsToWithConfig testConfig expected sql

[<Fact>]
let ``constraint columns stay inline when short and if longer than max line length`` () =
    let testConfig =
        { ddlTestConfig with
            ddl =
                { ddlTestConfig.ddl with
                    placeConstraintColumnsOnNewLines = DdlConstraintColumnsOnNewLines.IfLongerThanMaxLineLength } }

    let sql = "CREATE TABLE dbo.t (CONSTRAINT PK_t PRIMARY KEY (id, tenant_id))"

    let expected =
        """CREATE TABLE dbo.t (
    CONSTRAINT PK_t PRIMARY KEY (
        id, tenant_id
    )
)"""

    assertFormatsToWithConfig testConfig expected sql

[<Fact>]
let ``procedure params keep first parameter inline when style says never`` () =
    let testConfig =
        { config with
            ddl =
                { config.ddl with
                    placeFirstProcedureParameterOnNewLine = DdlFirstProcedureParameterOnNewLine.Never } }

    let sql = "CREATE PROCEDURE f @x INT, @y INT AS SELECT 1"

    let expected =
        """CREATE PROCEDURE f @x INT,
    @y INT
AS
SELECT 1"""

    assertFormatsToWithConfig testConfig expected sql

[<Fact>]
let ``procedure params break when style says always`` () =
    let testConfig =
        { config with
            ddl =
                { config.ddl with
                    placeFirstProcedureParameterOnNewLine = DdlFirstProcedureParameterOnNewLine.Always } }

    let sql = "CREATE PROCEDURE f @x INT AS SELECT 1"

    let expected =
        """CREATE PROCEDURE f
    @x INT
AS
SELECT 1"""

    assertFormatsToWithConfig testConfig expected sql
