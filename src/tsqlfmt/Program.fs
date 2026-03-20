/// CLI entry point for the T-SQL formatter.
module TSqlFormatter.Program

open System
open System.IO
open Argu
open TSqlFormatter.Config
open TSqlFormatter.Formatter

[<CliPrefix(CliPrefix.None)>]
type FormatSqlArgs =
    | [<CustomCommandLine("authToken", "t")>] AuthToken of token: string
    | [<CustomCommandLine("config")>] Config of path: string
    | [<CustomCommandLine("adsStylesPath")>] AdsStylesPath of path: string
    | [<CustomCommandLine("styleName", "n")>] StyleName of name: string
    | [<CustomCommandLine("applyCasing")>] ApplyCasing
    | [<CustomCommandLine("check")>] Check
    | [<CustomCommandLine("in-place")>] InPlace
    | [<MainCommand>] File of path: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | AuthToken _ ->
                "Accepted for SQL Prompt compatibility and ignored."
            | Config _ ->
                "Path to a JSON configuration file. If omitted, loads 'formattingstyle.json' from the current directory when present; otherwise uses built-in defaults."
            | AdsStylesPath _ ->
                "Path to a directory of SQL Prompt-style formatting files."
            | StyleName _ ->
                "Style name to load from available style files. Defaults to 'Default'."
            | ApplyCasing ->
                "Accepted for SQL Prompt compatibility."
            | Check ->
                "Check mode. Exits with code 1 if the input would change."
            | InPlace ->
                "Overwrite the input file with formatted output."
            | File _ ->
                "Input file path. If omitted or '-', reads from stdin."

[<CliPrefix(CliPrefix.None)>]
type CliArgs =
    | [<CliPrefix(CliPrefix.None)>]
      [<CustomCommandLine("formatSql")>]
      FormatSql of ParseResults<FormatSqlArgs>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | FormatSql _ -> "Format SQL."

type ResolvedArgs = {
    configPath: string option
    stylesPath: string option
    styleName: string
    styleNameSpecified: bool
    applyCasing: bool
    checkMode: bool
    inPlace: bool
    inputFile: string option
}

let private cliParser =
    ArgumentParser.Create<CliArgs>(programName = "tsqlfmt")

let private formatSqlParser =
    cliParser.GetSubCommandParser <@ FormatSql @>

let private isKnownButUnsupportedCommand (arg: string) =
    match arg with
    | "listAvailableStyles"
    | "createStyle"
    | "styleDeleted"
    | "styleEdited"
    | "activeStyleChanged"
    | "adsStarted" -> true
    | _ -> false

let private parseFormatSqlArgs (results: ParseResults<FormatSqlArgs>) =
    let styleName = results.GetResult(<@ StyleName @>, defaultValue = "Default")
    {
        configPath = results.TryGetResult <@ Config @>
        stylesPath = results.TryGetResult <@ AdsStylesPath @>
        styleName = styleName
        styleNameSpecified = results.Contains <@ StyleName @>
        applyCasing = results.Contains <@ ApplyCasing @>
        checkMode = results.Contains <@ Check @>
        inPlace = results.Contains <@ InPlace @>
        inputFile = results.TryGetResult <@ File @>
    }

let parseArgs (argv: string[]) : Result<ResolvedArgs, string> =
    try
        match argv with
        | [||] ->
            formatSqlParser.ParseCommandLine(inputs = [||], raiseOnUsage = true)
            |> parseFormatSqlArgs
            |> Ok
        | [| command |] when isKnownButUnsupportedCommand command ->
            Error (sprintf "Unsupported SQL Prompt command: %s" command)
        | [| command; _ |] when isKnownButUnsupportedCommand command ->
            Error (sprintf "Unsupported SQL Prompt command: %s" command)
        | _ when argv.Length > 0 && argv.[0] = "formatSql" ->
            cliParser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
            |> function
                | parsed when parsed.Contains <@ FormatSql @> -> parsed.GetResult <@ FormatSql @> |> parseFormatSqlArgs |> Ok
                | _ -> Error "Unsupported command"
        | _ when argv.Length > 0 && isKnownButUnsupportedCommand argv.[0] ->
            Error (sprintf "Unsupported SQL Prompt command: %s" argv.[0])
        | _ ->
            formatSqlParser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
            |> parseFormatSqlArgs
            |> Ok
    with
    | :? ArguParseException as ex -> Error ex.Message

let resolveConfigPath (currentDirectory: string) (args: ResolvedArgs) : Result<string option, string> =
    let tryFindNamedStyle (searchDirectories: string list) (styleName: string) =
        let matches =
            searchDirectories
            |> List.distinct
            |> List.filter Directory.Exists
            |> List.collect (fun directory ->
                Directory.GetFiles(directory)
                |> Array.choose (fun path ->
                    let fileName = Path.GetFileName(path)
                    let baseName = Path.GetFileNameWithoutExtension(path)
                    let rank =
                        if String.Equals(baseName, styleName, StringComparison.OrdinalIgnoreCase) then Some 0
                        elif String.Equals(styleName, "Default", StringComparison.OrdinalIgnoreCase) && String.Equals(fileName, "formattingstyle.json", StringComparison.OrdinalIgnoreCase) then Some 1
                        elif String.Equals(styleName, "Default", StringComparison.OrdinalIgnoreCase) && String.Equals(fileName, "default-style.json", StringComparison.OrdinalIgnoreCase) then Some 2
                        else None
                    rank |> Option.map (fun r -> r, path))
                |> Array.toList)

        match matches |> List.sortBy fst |> List.tryHead with
        | Some (_, path) -> Ok (Some path)
        | None when String.Equals(styleName, "Default", StringComparison.OrdinalIgnoreCase) -> Ok None
        | None -> Error (sprintf "Style '%s' could not be found" styleName)

    let searchDirectories =
        let executableDirectory =
            match Environment.ProcessPath with
            | null -> None
            | processPath -> Path.GetDirectoryName(processPath) |> Option.ofObj

        [ args.stylesPath
          Some currentDirectory
          executableDirectory ]
        |> List.choose id

    match args.configPath, args.stylesPath with
    | Some configPath, _ ->
        if File.Exists(configPath) then Ok (Some configPath)
        else Error (sprintf "Config file not found: %s" configPath)
    | None, _ when args.styleNameSpecified || args.stylesPath.IsSome ->
        tryFindNamedStyle searchDirectories args.styleName
    | None, None ->
        let defaultPath = Path.Combine(currentDirectory, "formattingstyle.json")
        if File.Exists(defaultPath) then Ok (Some defaultPath)
        else Ok None
    | None, Some _ ->
        Ok None

[<EntryPoint>]
let main argv =
    let parsed = parseArgs argv
    match parsed with
    | Error msg ->
        eprintfn "Error: %s" msg
        eprintfn "%s" (formatSqlParser.PrintUsage())
        2
    | Ok args ->
        let config =
            match resolveConfigPath (Directory.GetCurrentDirectory()) args with
            | Error msg ->
                eprintfn "Error: %s" msg
                exit 2
                defaultStyle
            | Ok (Some configPath) ->
                try loadConfig configPath
                with ex ->
                    eprintfn "Error loading config '%s': %s" configPath ex.Message
                    exit 2
                    defaultStyle
            | Ok None ->
                defaultStyle

        if args.inPlace && (args.inputFile.IsNone || args.inputFile = Some "-") then
            eprintfn "Error: --in-place requires a file path argument"
            2
        else
            let inputSql =
                match args.inputFile with
                | None | Some "-" -> Console.In.ReadToEnd()
                | Some path ->
                    if File.Exists(path) then
                        File.ReadAllText(path)
                    else
                        eprintfn "Error: file not found: %s" path
                        exit 2
                        ""

            match format config inputSql with
            | Error errors ->
                for e in errors do
                    eprintfn "Parse error: %s" e
                1
            | Ok formatted ->
                if args.checkMode then
                    if formatted = inputSql then
                        0
                    else
                        eprintfn "Input would change."
                        1
                elif args.inPlace then
                    File.WriteAllText(args.inputFile.Value, formatted)
                    0
                else
                    printf "%s" formatted
                    0
