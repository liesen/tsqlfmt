/// CLI entry point for the T-SQL formatter.
module TSqlFormatter.Program

open System
open System.IO
open TSqlFormatter.Config
open TSqlFormatter.Formatter

let private usage = """Usage: tsqlfmt [OPTIONS] [FILE]
       tsqlfmt formatSql [OPTIONS] [FILE]

Format T-SQL code.

When FILE is omitted or '-', reads from stdin.
Formatted SQL is written to stdout.

Options:
  --config PATH   Path to a JSON configuration file.
                   If omitted, loads 'formattingstyle.json' from the current directory when present;
                   otherwise uses built-in defaults.
  --adsStylesPath PATH
                  Path to a directory of SQL Prompt-style formatting files.
  --styleName, -n NAME
                  Style name to load from available style files. Defaults to 'Default'.
  --applyCasing   Accepted for SQL Prompt compatibility.
  --check         Check mode. Exits with code 1 if the input would change.
  --in-place      Overwrite the input file with formatted output.
  --help          Print usage information.

Exit Codes:
  0  Success (or no changes in --check mode)
  1  Input would change (--check mode), or formatting error
  2  Invalid arguments, file not found, or config load error
"""

type CliArgs = {
    configPath: string option
    stylesPath: string option
    styleName: string
    styleNameSpecified: bool
    applyCasing: bool
    checkMode: bool
    inPlace: bool
    showHelp: bool
    inputFile: string option
}

let defaultCliArgs = {
    configPath = None
    stylesPath = None
    styleName = "Default"
    styleNameSpecified = false
    applyCasing = false
    checkMode = false
    inPlace = false
    showHelp = false
    inputFile = None
}

let private isSupportedCommand (arg: string) =
    arg = "formatSql"

let private isKnownButUnsupportedCommand (arg: string) =
    match arg with
    | "listAvailableStyles"
    | "createStyle"
    | "styleDeleted"
    | "styleEdited"
    | "activeStyleChanged"
    | "adsStarted" -> true
    | _ -> false

let parseArgs (argv: string[]) : Result<CliArgs, string> =
    let rec loop i (args: CliArgs) =
        if i >= argv.Length then Ok args
        else
            match argv.[i] with
            | arg when i = 0 && isSupportedCommand arg ->
                loop (i + 1) args
            | arg when i = 0 && isKnownButUnsupportedCommand arg ->
                Error (sprintf "Unsupported SQL Prompt command: %s" arg)
            | "--help" | "-h" ->
                loop (i + 1) { args with showHelp = true }
            | "--config" ->
                if i + 1 < argv.Length then
                    loop (i + 2) { args with configPath = Some argv.[i + 1] }
                else
                    Error "--config requires a PATH argument"
            | "--adsStylesPath" ->
                if i + 1 < argv.Length then
                    loop (i + 2) { args with stylesPath = Some argv.[i + 1] }
                else
                    Error "--adsStylesPath requires a PATH argument"
            | "--styleName" | "-n" ->
                if i + 1 < argv.Length then
                    loop (i + 2) { args with styleName = argv.[i + 1]; styleNameSpecified = true }
                else
                    Error "--styleName requires a NAME argument"
            | "--applyCasing" ->
                loop (i + 1) { args with applyCasing = true }
            | "--check" ->
                loop (i + 1) { args with checkMode = true }
            | "--in-place" ->
                loop (i + 1) { args with inPlace = true }
            | arg when arg.StartsWith("-") && arg <> "-" ->
                Error (sprintf "Unknown option: %s" arg)
            | file ->
                match args.inputFile with
                | None -> loop (i + 1) { args with inputFile = Some file }
                | Some _ -> Error "Only one input file may be specified"
    loop 0 defaultCliArgs

let resolveConfigPath (currentDirectory: string) (args: CliArgs) : Result<string option, string> =
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
        eprintfn "%s" usage
        2
    | Ok args when args.showHelp ->
        printf "%s" usage
        0
    | Ok args ->
        let config =
            match resolveConfigPath (Directory.GetCurrentDirectory()) args with
            | Error msg ->
                eprintfn "Error: %s" msg
                exit 2
                defaultStyle // unreachable
            | Ok (Some configPath) ->
                try loadConfig configPath
                with ex ->
                    eprintfn "Error loading config '%s': %s" configPath ex.Message
                    exit 2
                    defaultStyle // unreachable
            | Ok None ->
                defaultStyle

        // Validate --in-place requires a file
        if args.inPlace && (args.inputFile.IsNone || args.inputFile = Some "-") then
            eprintfn "Error: --in-place requires a file path argument"
            2
        else

        // Read input
        let inputSql =
            match args.inputFile with
            | None | Some "-" -> Console.In.ReadToEnd()
            | Some path ->
                if File.Exists(path) then
                    File.ReadAllText(path)
                else
                    eprintfn "Error: file not found: %s" path
                    exit 2
                    "" // unreachable

        // Format
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
