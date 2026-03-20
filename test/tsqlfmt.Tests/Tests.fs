module Tests

open System.IO
open Xunit
open TSqlFormatter.Config
open TSqlFormatter.Formatter

let private testDataDir =
    // Navigate from test/tsqlfmt.Tests/bin/Debug/net10.0 up to repo root/tests
    let assemblyDir = System.Reflection.Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let mutable dir = assemblyDir
    // Walk up until we find the tests/ directory
    while dir <> null && not (Directory.Exists(Path.Combine(dir, "tests"))) do
        dir <- Path.GetDirectoryName(dir)
    if dir <> null then Path.Combine(dir, "tests")
    else
        // Fallback: relative from working directory
        Path.GetFullPath(Path.Combine(Directory.GetCurrentDirectory(), "..", "..", "..", "..", "..", "tests"))

let private configPath =
    let assemblyDir = System.Reflection.Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let mutable dir = assemblyDir
    while dir <> null && not (File.Exists(Path.Combine(dir, "default-style.json"))) do
        dir <- Path.GetDirectoryName(dir)
    if dir <> null then Path.Combine(dir, "default-style.json")
    else
        Path.GetFullPath(Path.Combine(Directory.GetCurrentDirectory(), "..", "..", "..", "..", "..", "default-style.json"))

let private config = loadConfig configPath

let testCases () : seq<obj[]> =
    Directory.GetFiles(testDataDir, "*.actual.sql")
    |> Array.map (fun path ->
        let name = Path.GetFileName(path).Replace(".actual.sql", "")
        [| name :> obj |])
    |> Array.sortBy (fun arr -> arr.[0] :?> string)
    |> Seq.ofArray

[<Theory>]
[<MemberData(nameof testCases)>]
let ``format test`` (testName: string) =
    let actualPath = Path.Combine(testDataDir, testName + ".actual.sql")
    let expectedPath = Path.Combine(testDataDir, testName + ".expected.sql")
    Assert.True(File.Exists(actualPath), sprintf "Input file not found: %s" actualPath)
    Assert.True(File.Exists(expectedPath), sprintf "Expected file not found: %s" expectedPath)
    let inputSql = File.ReadAllText(actualPath)
    let expectedSql = File.ReadAllText(expectedPath).ReplaceLineEndings("\n").TrimEnd()
    match format config inputSql with
    | Error errors ->
        Assert.Fail(sprintf "Parse errors: %s" (System.String.Join("; ", errors)))
    | Ok formatted ->
        let formatted = formatted.TrimEnd()
        if formatted <> expectedSql then
            // Write actual output for debugging
            let debugPath = Path.Combine(testDataDir, testName + ".debug.sql")
            File.WriteAllText(debugPath, formatted)
            // Find the first differing line
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
