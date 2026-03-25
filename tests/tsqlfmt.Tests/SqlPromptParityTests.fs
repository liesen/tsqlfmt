module SqlPromptParityTests

open System
open System.Diagnostics
open System.IO
open Xunit
open TSqlFormatter.Style
open TSqlFormatter.Formatter
open TestSupport

let private tryGetSqlPromptExe () =
    match Environment.GetEnvironmentVariable("SQLPROMPT_EXE") with
    | null
    | "" -> None
    | value when String.IsNullOrWhiteSpace value -> None
    | value -> Some value

let private tryGetSqlPromptAuthToken () =
    match Environment.GetEnvironmentVariable("SQLPROMPT_AUTH_TOKEN") with
    | null
    | "" -> None
    | value when String.IsNullOrWhiteSpace value -> None
    | value -> Some value

let private requireSqlPromptConfig () =
    match tryGetSqlPromptExe (), tryGetSqlPromptAuthToken () with
    | Some exe, Some authToken -> Ok(exe, authToken)
    | _ -> Error "Set SQLPROMPT_EXE and SQLPROMPT_AUTH_TOKEN to run SQL Prompt parity tests."

let private runSqlPrompt (stylePath: string) (applyCasing: bool) (sql: string) =
    let exe, authToken =
        match requireSqlPromptConfig () with
        | Ok value -> value
        | Error message -> failwith message

    let args =
        [ "formatSql"
          "--authToken"
          authToken
          "--adsStylesPath"
          Path.GetDirectoryName(stylePath)
          "--styleName"
          Path.GetFileNameWithoutExtension(stylePath) ]
        @ (if applyCasing then [ "--applyCasing" ] else [])

    let psi = ProcessStartInfo(exe)
    args |> List.iter psi.ArgumentList.Add
    psi.RedirectStandardInput <- true
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false

    use proc = new Process()
    proc.StartInfo <- psi

    if not (proc.Start()) then
        failwith "Failed to start SQL Prompt command from SQLPROMPT_EXE"

    proc.StandardInput.Write(sql)
    proc.StandardInput.Close()

    let stdout = proc.StandardOutput.ReadToEnd()
    let stderr = proc.StandardError.ReadToEnd()
    proc.WaitForExit()

    if proc.ExitCode <> 0 then
        failwithf "SQL Prompt failed: %s" (stderr.Trim())

    stdout.ReplaceLineEndings("\n").TrimEnd()

let private writeStyleFile (dir: string) (name: string) (json: string) =
    let path = Path.Combine(dir, name + ".formattingstyle")
    File.WriteAllText(path, json)
    path

let sqlPromptParityCases () : seq<obj[]> =
    match requireSqlPromptConfig () with
    | Error _ -> Seq.empty
    | Ok _ ->
        testCases ()
        |> Seq.filter (fun (arr: obj[]) -> (arr.[0] :?> string) <> "test32")
        |> Seq.map (fun (arr: obj[]) -> [| arr.[0]; false :> obj |])

type SqlPromptParityData =
    static member sqlPromptParityCases() = sqlPromptParityCases ()

[<Theory>]
[<MemberData("sqlPromptParityCases", MemberType = typeof<SqlPromptParityData>, DisableDiscoveryEnumeration = true)>]
let ``existing fixture matches sqlprompt for default style`` (testName: string, applyCasing: bool) =
    let actualPath = Path.Combine(testDataDir, testName + ".actual.sql")
    let inputSql = File.ReadAllText(actualPath)

    let localStyleDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(localStyleDir) |> ignore

    try
        let styleJson = File.ReadAllText(configPath)
        let stylePath = writeStyleFile localStyleDir "Default" styleJson

        let ourConfig =
            loadStyle stylePath |> validateStyle |> withOptionalCasing applyCasing

        let ourOutput =
            match format ourConfig inputSql with
            | Ok formatted -> formatted.ReplaceLineEndings("\n").TrimEnd()
            | Error errors -> failwithf "tsqlfmt parse errors: %s" (String.Join("; ", errors))

        let sqlPromptOutput = runSqlPrompt stylePath applyCasing inputSql

        Assert.Equal(sqlPromptOutput, ourOutput)
    finally
        Directory.Delete(localStyleDir, true)
