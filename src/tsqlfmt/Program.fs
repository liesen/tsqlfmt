/// CLI entry point for the T-SQL formatter.
module TSqlFormatter.Program

open System
open System.IO
open Argu
open TSqlFormatter.Config
open TSqlFormatter.Formatter

[<CliPrefix("--")>]
type FormatSqlArgs =
    | [<CustomCommandLine("--authToken", "-t")>] AuthToken of token: string
    | [<CustomCommandLine("--adsStylesPath")>] AdsStylesPath of path: string
    | [<CustomCommandLine("--styleName", "-n")>] StyleName of name: string
    | [<CustomCommandLine("--applyCasing")>] ApplyCasing

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | AuthToken _ ->
                "Accepted for SQL Prompt compatibility and ignored."
            | AdsStylesPath _ ->
                "Path to a directory of SQL Prompt-style formatting files."
            | StyleName _ ->
                "Style name to load from available style files. Defaults to 'Default'."
            | ApplyCasing ->
                "Accepted for SQL Prompt compatibility."

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
    stylesPath: string option
    styleName: string
    styleNameSpecified: bool
    applyCasing: bool
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
        stylesPath = results.TryGetResult <@ AdsStylesPath @>
        styleName = styleName
        styleNameSpecified = results.Contains <@ StyleName @>
        applyCasing = results.Contains <@ ApplyCasing @>
    }

let parseArgs (argv: string[]) : Result<ResolvedArgs, string> =
    try
        match argv with
        | [||] ->
            Error "Missing SQL Prompt command: formatSql"
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
            Error "Missing SQL Prompt command: formatSql"
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
          executableDirectory ]
        |> List.choose id

    match args.stylesPath with
    | _ when args.styleNameSpecified || args.stylesPath.IsSome ->
        tryFindNamedStyle searchDirectories args.styleName
    | None ->
        Ok None
    | Some _ ->
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

        let inputSql = Console.In.ReadToEnd()

        match format config inputSql with
        | Error errors ->
            for e in errors do
                eprintfn "Parse error: %s" e
            1
        | Ok formatted ->
            printf "%s" formatted
            0
