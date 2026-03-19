/// CLI entry point for the T-SQL formatter.
module TSqlFormatter.Program

open System
open System.IO
open TSqlFormatter.Config
open TSqlFormatter.Formatter

let private usage = """Usage: tsqlfmt [OPTIONS] [FILE]

Format T-SQL code.

When FILE is omitted or '-', reads from stdin.
Formatted SQL is written to stdout.

Options:
  --config PATH   Path to a JSON configuration file.
                  Defaults to 'formattingstyle.json' in the current directory.
  --check         Check mode. Exits with code 1 if the input would change.
  --in-place      Overwrite the input file with formatted output.
  --help          Print usage information.

Exit Codes:
  0  Success (or no changes in --check mode)
  1  Input would change (--check mode), or formatting error
  2  Invalid arguments or missing configuration file
"""

type CliArgs = {
    configPath: string option
    checkMode: bool
    inPlace: bool
    showHelp: bool
    inputFile: string option
}

let private parseArgs (argv: string[]) : Result<CliArgs, string> =
    let mutable args = {
        configPath = None
        checkMode = false
        inPlace = false
        showHelp = false
        inputFile = None
    }
    let mutable i = 0
    let mutable error = None
    while i < argv.Length && error.IsNone do
        match argv.[i] with
        | "--help" | "-h" ->
            args <- { args with showHelp = true }
            i <- i + 1
        | "--config" ->
            if i + 1 < argv.Length then
                args <- { args with configPath = Some argv.[i + 1] }
                i <- i + 2
            else
                error <- Some "--config requires a PATH argument"
        | "--check" ->
            args <- { args with checkMode = true }
            i <- i + 1
        | "--in-place" ->
            args <- { args with inPlace = true }
            i <- i + 1
        | arg when arg.StartsWith("-") && arg <> "-" ->
            error <- Some (sprintf "Unknown option: %s" arg)
        | file ->
            args <- { args with inputFile = Some file }
            i <- i + 1
    match error with
    | Some e -> Error e
    | None -> Ok args

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
        // Resolve config
        let configPath =
            match args.configPath with
            | Some p -> p
            | None ->
                let defaultPath = Path.Combine(Directory.GetCurrentDirectory(), "formattingstyle.json")
                if File.Exists(defaultPath) then defaultPath
                else ""
        let config =
            if configPath <> "" && File.Exists(configPath) then
                try loadConfig configPath
                with ex ->
                    eprintfn "Error loading config '%s': %s" configPath ex.Message
                    exit 2
                    defaultStyle // unreachable
            else
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
