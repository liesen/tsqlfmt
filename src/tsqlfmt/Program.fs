/// CLI entry point for the T-SQL formatter.
module TSqlFormatter.Program

open System
open System.IO
open TSqlFormatter.CliArgs
open TSqlFormatter.Config
open TSqlFormatter.Formatter

let private withOptionalCasing (applyCasing: bool) (config: FormattingStyle) =
    // SQL Prompt treats --applyCasing as a gate on the style's existing casing rules.
    // This is not ideal because the style already contains the casing intent, but we
    // keep the behavior for compatibility with SQL Prompt's CLI contract.
    if applyCasing then
        config
    else
        { config with casing = defaultCasing }

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
            |> withOptionalCasing args.applyCasing

        let inputSql = Console.In.ReadToEnd()

        match format config inputSql with
        | Error errors ->
            for e in errors do
                eprintfn "Parse error: %s" e
            1
        | Ok formatted ->
            printf "%s" formatted
            0
