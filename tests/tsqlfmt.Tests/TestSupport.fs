module TestSupport

open System
open System.IO
open Xunit
open TSqlFormatter.Config
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

let config = loadConfig configPath

let withOptionalCasing applyCasing config =
    if applyCasing then
        config
    else
        { config with casing = defaultCasing }

let assertFormatsToWithConfig (testConfig: FormattingStyle) (expected: string) (sql: string) =
    let expectedSql = expected.ReplaceLineEndings("\n").Trim().TrimEnd()

    match format testConfig sql with
    | Error errors -> Assert.Fail(sprintf "Parse errors: %s" (String.Join("; ", errors)))
    | Ok formatted -> Assert.Equal(expectedSql, formatted.TrimEnd())

let assertFormatsTo (expected: string) (sql: string) =
    assertFormatsToWithConfig config expected sql

let testCases () : seq<obj[]> =
    Directory.GetFiles(testDataDir, "*.actual.sql")
    |> Array.map (fun path ->
        let name = Path.GetFileName(path).Replace(".actual.sql", "")
        [| name :> obj |])
    |> Array.sortBy (fun arr -> arr.[0] :?> string)
    |> Seq.ofArray

type FixtureData =
    static member testCases() = testCases ()
