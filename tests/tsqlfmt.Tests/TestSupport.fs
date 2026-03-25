module TestSupport

open System
open System.IO
open Xunit
open Microsoft.SqlServer.TransactSql.ScriptDom
open TSqlFormatter.Doc
open TSqlFormatter.Style
open TSqlFormatter.Formatter

let testDataDir =
    let assemblyDir =
        System.Reflection.Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName

    let mutable dir = assemblyDir

    while dir <> null
          && not (Directory.Exists(Path.Combine(dir, "tests", "tsqlfmt.Tests", "TestData"))) do
        dir <- Path.GetDirectoryName(dir)

    if dir <> null then
        Path.Combine(dir, "tests", "tsqlfmt.Tests", "TestData")
    else
        Path.GetFullPath(Path.Combine(Directory.GetCurrentDirectory(), "tests", "tsqlfmt.Tests", "TestData"))

let configPath =
    let assemblyDir =
        System.Reflection.Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName

    let mutable dir = assemblyDir

    while dir <> null && not (File.Exists(Path.Combine(dir, "default-style.json"))) do
        dir <- Path.GetDirectoryName(dir)

    if dir <> null then
        Path.Combine(dir, "default-style.json")
    else
        Path.GetFullPath(
            Path.Combine(Directory.GetCurrentDirectory(), "..", "..", "..", "..", "..", "default-style.json")
        )

let config = loadStyle configPath

let withOptionalCasing applyCasing config =
    if applyCasing then
        config
    else
        { config with casing = defaultCasing }

let assertFormatsToWithConfig (testConfig: Style) (expected: string) (sql: string) =
    let expectedSql = expected.ReplaceLineEndings("\n").Trim().TrimEnd()

    match format testConfig sql with
    | Error errors -> Assert.Fail(sprintf "Parse errors: %s" (String.Join("; ", errors)))
    | Ok formatted -> Assert.Equal(expectedSql, formatted.TrimEnd())

let assertFormatsTo (expected: string) (sql: string) =
    assertFormatsToWithConfig config expected sql

let parseBooleanExpression (sql: string) =
    let parser = TSql160Parser(initialQuotedIdentifiers = true)
    use reader = new StringReader("SELECT 1 WHERE " + sql)
    let mutable errors = null: System.Collections.Generic.IList<ParseError>
    let fragment = parser.Parse(reader, &errors)

    if errors <> null && errors.Count > 0 then
        Assert.Fail(sprintf "Parse errors: %s" (String.Join("; ", errors |> Seq.map (fun e -> e.Message))))

    match fragment with
    | :? TSqlScript as script when script.Batches.Count > 0 && script.Batches.[0].Statements.Count > 0 ->
        match script.Batches.[0].Statements.[0] with
        | :? SelectStatement as stmt ->
            match stmt.QueryExpression with
            | :? QuerySpecification as spec ->
                if isNull spec.WhereClause || isNull spec.WhereClause.SearchCondition then
                    Assert.Fail("Expected WHERE clause search condition")

                spec.WhereClause.SearchCondition
            | _ ->
                Assert.Fail("Expected SELECT statement with query specification")
                failwith "unreachable"
        | _ ->
            Assert.Fail("Expected SELECT statement with query specification")
            failwith "unreachable"
    | _ ->
        Assert.Fail("Expected T-SQL script")
        failwith "unreachable"

let parseSelectStatement (sql: string) =
    let parser = TSql160Parser(initialQuotedIdentifiers = true)
    use reader = new StringReader(sql)
    let mutable errors = null: System.Collections.Generic.IList<ParseError>
    let fragment = parser.Parse(reader, &errors)

    if errors <> null && errors.Count > 0 then
        Assert.Fail(sprintf "Parse errors: %s" (String.Join("; ", errors |> Seq.map (fun e -> e.Message))))

    match fragment with
    | :? TSqlScript as script when script.Batches.Count > 0 && script.Batches.[0].Statements.Count > 0 ->
        match script.Batches.[0].Statements.[0] with
        | :? SelectStatement as stmt -> stmt
        | _ ->
            Assert.Fail("Expected SELECT statement")
            failwith "unreachable"
    | _ ->
        Assert.Fail("Expected T-SQL script")
        failwith "unreachable"

let assertBooleanExpressionDoc (expected: string) (sql: string) =
    let expectedSql = expected.ReplaceLineEndings("\n").Trim().TrimEnd()
    let expr = parseBooleanExpression sql

    let actual =
        expr
        |> formatBooleanExpressionDoc config
        |> render config.whitespace.wrapLinesLongerThan

    Assert.Equal(expectedSql, actual.TrimEnd())

let assertRenderedDoc (maxWidth: int) (expected: string) (doc: Doc) =
    let expectedDoc = expected.ReplaceLineEndings("\n").Trim().TrimEnd()
    let actual = render maxWidth doc
    Assert.Equal(expectedDoc, actual.TrimEnd())

let testCases () : seq<obj[]> =
    Directory.GetFiles(testDataDir, "*.actual.sql")
    |> Array.map (fun path ->
        let name = Path.GetFileName(path).Replace(".actual.sql", "")
        [| name :> obj |])
    |> Array.filter (fun arr -> (arr.[0] :?> string) <> "test46")
    |> Array.sortBy (fun arr -> arr.[0] :?> string)
    |> Seq.ofArray

type FixtureData =
    static member testCases() = testCases ()
