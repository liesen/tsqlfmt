module TSqlFormatter.CliArgs

open Argu

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

let formatSqlParser =
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
