module Tests

open System
open System.IO
open Xunit
open TSqlFormatter.Config
open TSqlFormatter.Formatter
open TSqlFormatter.Program

let private testDataDir =
    let assemblyDir = System.Reflection.Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let mutable dir = assemblyDir
    while dir <> null && not (Directory.Exists(Path.Combine(dir, "tests", "tsqlfmt.Tests", "TestData"))) do
        dir <- Path.GetDirectoryName(dir)
    if dir <> null then Path.Combine(dir, "tests", "tsqlfmt.Tests", "TestData")
    else
        Path.GetFullPath(Path.Combine(Directory.GetCurrentDirectory(), "tests", "tsqlfmt.Tests", "TestData"))

let private configPath =
    let assemblyDir = System.Reflection.Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let mutable dir = assemblyDir
    while dir <> null && not (File.Exists(Path.Combine(dir, "default-style.json"))) do
        dir <- Path.GetDirectoryName(dir)
    if dir <> null then Path.Combine(dir, "default-style.json")
    else
        Path.GetFullPath(Path.Combine(Directory.GetCurrentDirectory(), "..", "..", "..", "..", "..", "default-style.json"))

let private config = loadConfig configPath

let private writeTempStyle (dir: string) (name: string) (keywordStyle: string) =
    let path = Path.Combine(dir, name)
    let json = sprintf "{\"casing\":{\"reservedKeywords\":\"%s\"}}" keywordStyle
    File.WriteAllText(path, json)
    path

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

[<Fact>]
let ``parseArgs accepts sqlprompt formatSql command`` () =
    let parsed = parseArgs [| "formatSql"; "--styleName"; "Default"; "input.sql" |]
    match parsed with
    | Error msg -> Assert.Fail msg
    | Ok args ->
        Assert.Equal("Default", args.styleName)
        Assert.Equal(Some "input.sql", args.inputFile)

[<Fact>]
let ``parseArgs rejects unsupported sqlprompt command`` () =
    let parsed = parseArgs [| "listAvailableStyles" |]
    match parsed with
    | Ok _ -> Assert.Fail("Expected unsupported command to fail")
    | Error msg -> Assert.Contains("Unsupported SQL Prompt command", msg)

[<Fact>]
let ``resolveConfigPath ignores missing styles directory for default style`` () =
    let missingDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    let args = { defaultCliArgs with stylesPath = Some missingDir; styleName = "Default"; styleNameSpecified = true }
    let resolved = resolveConfigPath (Directory.GetCurrentDirectory()) args
    match resolved with
    | Error msg -> Assert.Fail msg
    | Ok _ -> ()

[<Fact>]
let ``resolveConfigPath rejects missing named style without styles directory`` () =
    let missingDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    let args = { defaultCliArgs with stylesPath = Some missingDir; styleName = "MyStyle"; styleNameSpecified = true }
    let resolved = resolveConfigPath (Directory.GetCurrentDirectory()) args
    match resolved with
    | Ok _ -> Assert.Fail("Expected missing style to fail")
    | Error msg -> Assert.Contains("Style 'MyStyle' could not be found", msg)

[<Fact>]
let ``resolveConfigPath rejects missing named style`` () =
    let stylesDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(stylesDir) |> ignore
    try
        let args = { defaultCliArgs with stylesPath = Some stylesDir; styleName = "Missing"; styleNameSpecified = true }
        let resolved = resolveConfigPath (Directory.GetCurrentDirectory()) args
        match resolved with
        | Ok _ -> Assert.Fail("Expected missing style to fail")
        | Error msg -> Assert.Contains("Style 'Missing' could not be found", msg)
    finally
        Directory.Delete(stylesDir, true)

[<Fact>]
let ``resolveConfigPath uses named style from styles directory`` () =
    let stylesDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(stylesDir) |> ignore
    try
        let stylePath = writeTempStyle stylesDir "MyStyle.json" "lowercase"
        let args = { defaultCliArgs with stylesPath = Some stylesDir; styleName = "MyStyle" }
        let resolved = resolveConfigPath (Directory.GetCurrentDirectory()) args
        match resolved with
        | Error msg -> Assert.Fail msg
        | Ok None -> Assert.Fail("Expected a style path")
        | Ok (Some path) -> Assert.Equal(stylePath, path)
    finally
        Directory.Delete(stylesDir, true)

[<Fact>]
let ``resolveConfigPath falls back to formattingstyle json for default style`` () =
    let stylesDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(stylesDir) |> ignore
    try
        let stylePath = writeTempStyle stylesDir "formattingstyle.json" "uppercase"
        let args = { defaultCliArgs with stylesPath = Some stylesDir; styleName = "Default" }
        let resolved = resolveConfigPath (Directory.GetCurrentDirectory()) args
        match resolved with
        | Error msg -> Assert.Fail msg
        | Ok None -> Assert.Fail("Expected a style path")
        | Ok (Some path) -> Assert.Equal(stylePath, path)
    finally
        Directory.Delete(stylesDir, true)
