module Tests

open System
open System.IO
open Xunit
open TSqlFormatter.CliArgs
open TSqlFormatter.Style
open TSqlFormatter.Program
open TestSupport

let private defaultCliArgs =
    { stylesPath = None
      styleName = "Default"
      styleNameSpecified = false
      applyCasing = false }

let private writeTempStyle (dir: string) (name: string) (keywordStyle: string) =
    let path = Path.Combine(dir, name)
    let json = sprintf "{\"casing\":{\"reservedKeywords\":\"%s\"}}" keywordStyle
    File.WriteAllText(path, json)
    path

[<Fact>]
let ``parseArgs accepts sqlprompt formatSql command`` () =
    let parsed = parseArgs [| "formatSql"; "--styleName"; "Default" |]

    match parsed with
    | Error msg -> Assert.Fail msg
    | Ok args -> Assert.Equal("Default", args.styleName)

[<Fact>]
let ``parseArgs accepts auth token and ignores it`` () =
    let parsed =
        parseArgs [| "formatSql"; "--authToken"; "secret"; "--styleName"; "Default" |]

    match parsed with
    | Error msg -> Assert.Fail msg
    | Ok args -> Assert.Equal("Default", args.styleName)

[<Fact>]
let ``parseArgs rejects direct non-sqlprompt invocation`` () =
    let parsed = parseArgs [| "--styleName"; "Default" |]

    match parsed with
    | Ok _ -> Assert.Fail("Expected direct invocation to fail")
    | Error _ -> ()

[<Fact>]
let ``parseArgs rejects unsupported sqlprompt command`` () =
    let parsed = parseArgs [| "listAvailableStyles" |]

    match parsed with
    | Ok _ -> Assert.Fail("Expected unsupported command to fail")
    | Error msg -> Assert.Contains("Unsupported SQL Prompt command", msg)

[<Fact>]
let ``resolveConfigPath ignores missing styles directory for default style`` () =
    let missingDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))

    let args =
        { defaultCliArgs with
            stylesPath = Some missingDir
            styleName = "Default"
            styleNameSpecified = true }

    let resolved = resolveConfigPath (Directory.GetCurrentDirectory()) args

    match resolved with
    | Error msg -> Assert.Fail msg
    | Ok _ -> ()

[<Fact>]
let ``resolveConfigPath rejects missing named style without styles directory`` () =
    let missingDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))

    let args =
        { defaultCliArgs with
            stylesPath = Some missingDir
            styleName = "MyStyle"
            styleNameSpecified = true }

    let resolved = resolveConfigPath (Directory.GetCurrentDirectory()) args

    match resolved with
    | Ok _ -> Assert.Fail("Expected missing style to fail")
    | Error msg -> Assert.Contains("Style 'MyStyle' could not be found", msg)

[<Fact>]
let ``resolveConfigPath rejects missing named style`` () =
    let stylesDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(stylesDir) |> ignore

    try
        let args =
            { defaultCliArgs with
                stylesPath = Some stylesDir
                styleName = "Missing"
                styleNameSpecified = true }

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

        let args =
            { defaultCliArgs with
                stylesPath = Some stylesDir
                styleName = "MyStyle" }

        let resolved = resolveConfigPath (Directory.GetCurrentDirectory()) args

        match resolved with
        | Error msg -> Assert.Fail msg
        | Ok None -> Assert.Fail("Expected a style path")
        | Ok(Some path) -> Assert.Equal(stylePath, path)
    finally
        Directory.Delete(stylesDir, true)

[<Fact>]
let ``resolveConfigPath falls back to formattingstyle json for default style`` () =
    let stylesDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(stylesDir) |> ignore

    try
        let stylePath = writeTempStyle stylesDir "formattingstyle.json" "uppercase"

        let args =
            { defaultCliArgs with
                stylesPath = Some stylesDir
                styleName = "Default" }

        let resolved = resolveConfigPath (Directory.GetCurrentDirectory()) args

        match resolved with
        | Error msg -> Assert.Fail msg
        | Ok None -> Assert.Fail("Expected a style path")
        | Ok(Some path) -> Assert.Equal(stylePath, path)
    finally
        Directory.Delete(stylesDir, true)

[<Fact>]
let ``withOptionalCasing disables style casing when applyCasing is false`` () =
    let configWithCasing =
        { defaultStyle with
            casing =
                { defaultCasing with
                    reservedKeywords = CasingStyle.Uppercase
                    builtInFunctions = CasingStyle.Uppercase
                    builtInDataTypes = CasingStyle.Uppercase } }

    let result = withOptionalCasing false configWithCasing

    Assert.Equal(CasingStyle.LeaveAsIs, result.casing.reservedKeywords)
    Assert.Equal(CasingStyle.LeaveAsIs, result.casing.builtInFunctions)
    Assert.Equal(CasingStyle.LeaveAsIs, result.casing.builtInDataTypes)

[<Fact>]
let ``withOptionalCasing preserves style casing when applyCasing is true`` () =
    let configWithCasing =
        { defaultStyle with
            casing =
                { defaultCasing with
                    reservedKeywords = CasingStyle.Uppercase } }

    let result = withOptionalCasing true configWithCasing

    Assert.Equal(CasingStyle.Uppercase, result.casing.reservedKeywords)

[<Fact>]
let ``loadStyle parses ddl constraint columns if longer or multiple columns`` () =
    let stylesDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(stylesDir) |> ignore

    try
        let stylePath = Path.Combine(stylesDir, "style.json")

        File.WriteAllText(stylePath, """{"ddl":{"placeConstraintColumnsOnNewLines":"ifLongerOrMultipleColumns"}}""")

        let style = loadStyle stylePath

        Assert.Equal(
            DdlConstraintColumnsOnNewLines.IfLongerOrMultipleColumns,
            style.ddl.placeConstraintColumnsOnNewLines
        )
    finally
        Directory.Delete(stylesDir, true)

[<Fact>]
let ``loadStyle parses ddl first procedure parameter enum`` () =
    let stylesDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(stylesDir) |> ignore

    try
        let stylePath = Path.Combine(stylesDir, "style.json")
        File.WriteAllText(stylePath, """{"ddl":{"placeFirstProcedureParameterOnNewLine":"ifMultipleItems"}}""")

        let style = loadStyle stylePath

        Assert.Equal(
            DdlFirstProcedureParameterOnNewLine.IfMultipleItems,
            style.ddl.placeFirstProcedureParameterOnNewLine
        )
    finally
        Directory.Delete(stylesDir, true)

[<Fact>]
let ``loadStyle parses unsupported right aligned boolean alignment`` () =
    let stylesDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(stylesDir) |> ignore

    try
        let stylePath = Path.Combine(stylesDir, "style.json")
        File.WriteAllText(stylePath, """{"operators":{"andOr":{"alignment":"rightAligned"}}}""")

        let style = loadStyle stylePath

        Assert.Equal(Alignment.RightAligned, style.operators.andOr.alignment)
    finally
        Directory.Delete(stylesDir, true)

[<Fact>]
let ``validateStyle rejects unsupported right aligned boolean alignment`` () =
    let stylesDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(stylesDir) |> ignore

    try
        let stylePath = Path.Combine(stylesDir, "style.json")
        File.WriteAllText(stylePath, """{"operators":{"andOr":{"alignment":"rightAligned"}}}""")

        let style = loadStyle stylePath
        let ex = Assert.Throws<ArgumentException>(fun () -> validateStyle style |> ignore)

        Assert.Contains("operators.andOr.alignment", ex.Message)
        Assert.Contains("not supported", ex.Message)
    finally
        Directory.Delete(stylesDir, true)

[<Fact>]
let ``loadStyle parses unsupported right aligned case end alignment`` () =
    let stylesDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(stylesDir) |> ignore

    try
        let stylePath = Path.Combine(stylesDir, "style.json")
        File.WriteAllText(stylePath, """{"caseExpressions":{"endAlignment":"rightAlignedToWhen"}}""")

        let style = loadStyle stylePath

        Assert.Equal(EndAlignment.RightAlignedToWhen, style.caseExpressions.endAlignment)
    finally
        Directory.Delete(stylesDir, true)

[<Fact>]
let ``validateStyle rejects unsupported right aligned case end alignment`` () =
    let stylesDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(stylesDir) |> ignore

    try
        let stylePath = Path.Combine(stylesDir, "style.json")
        File.WriteAllText(stylePath, """{"caseExpressions":{"endAlignment":"rightAlignedToWhen"}}""")

        let style = loadStyle stylePath

        let ex = Assert.Throws<ArgumentException>(fun () -> validateStyle style |> ignore)

        Assert.Contains("caseExpressions.endAlignment", ex.Message)
        Assert.Contains("not supported", ex.Message)
    finally
        Directory.Delete(stylesDir, true)
