module RegressionTests

open System
open System.IO
open Xunit
open TSqlFormatter.Formatter
open TestSupport

[<Theory>]
[<MemberData("testCases", MemberType = typeof<FixtureData>)>]
let ``format test`` (testName: string) =
    let actualPath = Path.Combine(testDataDir, testName + ".actual.sql")
    let expectedPath = Path.Combine(testDataDir, testName + ".expected.sql")
    Assert.True(File.Exists(actualPath), sprintf "Input file not found: %s" actualPath)
    Assert.True(File.Exists(expectedPath), sprintf "Expected file not found: %s" expectedPath)
    let inputSql = File.ReadAllText(actualPath)
    let expectedSql = File.ReadAllText(expectedPath).ReplaceLineEndings("\n").TrimEnd()

    match format config inputSql with
    | Error errors ->
        Assert.Fail(sprintf "Parse errors: %s" (String.Join("; ", errors)))
    | Ok formatted ->
        let formatted = formatted.TrimEnd()

        if formatted <> expectedSql then
            let debugPath = Path.Combine(testDataDir, testName + ".debug.sql")
            File.WriteAllText(debugPath, formatted)
            let expectedLines = expectedSql.Split('\n')
            let formattedLines = formatted.Split('\n')
            let mutable diffLine = -1
            let maxLines = max expectedLines.Length formattedLines.Length

            for i in 0 .. maxLines - 1 do
                if diffLine = -1 then
                    let eLine = if i < expectedLines.Length then expectedLines.[i].TrimEnd('\r') else "<missing>"
                    let fLine = if i < formattedLines.Length then formattedLines.[i].TrimEnd('\r') else "<missing>"
                    if eLine <> fLine then
                        diffLine <- i

            let diffInfo =
                if diffLine >= 0 then
                    let eLine = if diffLine < expectedLines.Length then expectedLines.[diffLine].TrimEnd('\r') else "<missing>"
                    let fLine = if diffLine < formattedLines.Length then formattedLines.[diffLine].TrimEnd('\r') else "<missing>"
                    sprintf "\nFirst difference at line %d:\nExpected: [%s]\nActual:   [%s]" (diffLine + 1) eLine fLine
                else ""
            Assert.Fail(sprintf "Output does not match expected for %s. Debug output written to %s.%s" testName debugPath diffInfo)
