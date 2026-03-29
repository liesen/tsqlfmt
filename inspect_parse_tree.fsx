#r "nuget: Microsoft.SqlServer.TransactSql.ScriptDom, 170.191.0"
#r "nuget: Argu, 6.2.5"

open System
open System.IO
open System.Reflection
open System.Collections
open Argu
open Microsoft.SqlServer.TransactSql.ScriptDom

type CliArgs =
    | [<AltCommandLine("-f")>] File of path: string
    | [<AltCommandLine("tree")>] Tree
    | [<AltCommandLine("ast")>] Ast

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | File _ -> "read SQL from a file instead of stdin"
            | Tree -> "print the parse tree"
            | Ast -> "print the AST with scalar properties"

let parser = ArgumentParser.Create<CliArgs>(programName = "inspect_parse_tree.fsx")

let results =
    parser.Parse(fsi.CommandLineArgs |> Array.skip 1 |> Array.filter ((<>) "--"), raiseOnUsage = false)

if results.IsUsageRequested then
    printfn "%s" (parser.PrintUsage())
    Environment.Exit(0)

let modeArgs =
    results.GetAllResults()
    |> List.choose (function
        | CliArgs.Tree -> Some CliArgs.Tree
        | CliArgs.Ast -> Some CliArgs.Ast
        | _ -> None)

let mode =
    match modeArgs with
    | [] -> CliArgs.Ast
    | [ mode ] -> mode
    | _ ->
        eprintfn "Specify at most one of: tree, ast"
        Environment.Exit(1)
        CliArgs.Ast

let filePath = results.TryGetResult <@ File @>

let usage () = printfn "%s" (parser.PrintUsage())

let readSql () =
    match filePath with
    | Some path -> File.ReadAllText(path)
    | None ->
        let text = Console.In.ReadToEnd()

        if String.IsNullOrWhiteSpace text then
            usage ()
            Environment.Exit(1)
            ""
        else
            text

let sql = readSql ()

let tsqlParser = TSql160Parser(initialQuotedIdentifiers = true)
let reader = new StringReader(sql)
let mutable errors = null: System.Collections.Generic.IList<ParseError>
let fragment = tsqlParser.Parse(reader, &errors)

if errors <> null && errors.Count > 0 then
    errors |> Seq.iter (fun e -> eprintfn "(%d,%d): %s" e.Line e.Column e.Message)

    Environment.Exit(1)

let indent level = String.replicate level "  "

let skipProps =
    set
        [ "ScriptTokenStream"
          "FirstTokenIndex"
          "LastTokenIndex"
          "StartOffset"
          "FragmentLength"
          "StartLine"
          "StartColumn" ]

let propsOf (o: obj) =
    o.GetType().GetProperties(BindingFlags.Instance ||| BindingFlags.Public)
    |> Array.filter (fun p ->
        p.CanRead
        && p.GetIndexParameters().Length = 0
        && not (skipProps.Contains p.Name))

let childFragments (frag: TSqlFragment) =
    seq {
        for p in propsOf frag do
            let v = p.GetValue(frag)

            match v with
            | null -> ()
            | :? TSqlFragment as child -> yield p.Name, child
            | :? string -> ()
            | :? IEnumerable as items ->
                let mutable i = 0

                for item in items do
                    match item with
                    | :? TSqlFragment as child ->
                        yield sprintf "%s[%d]" p.Name i, child
                        i <- i + 1
                    | _ -> ()
            | _ -> ()
    }
    |> Seq.toList

let simpleValue (v: obj) =
    match v with
    | null -> None
    | :? string as s when String.IsNullOrWhiteSpace s -> None
    | :? string as s -> Some(sprintf "\"%s\"" s)
    | :? bool as b -> Some(string b)
    | :? int16 as n -> Some(string n)
    | :? int as n -> Some(string n)
    | :? int64 as n -> Some(string n)
    | _ when v.GetType().IsEnum -> Some(string v)
    | _ -> None

let rec printParseTree level edge (frag: TSqlFragment) =
    printfn "%s%s%s" (indent level) edge (frag.GetType().Name)

    for name, child in childFragments frag do
        printParseTree (level + 1) (name + ": ") child

let rec printAst level edge (frag: TSqlFragment) =
    printfn "%s%s%s" (indent level) edge (frag.GetType().Name)

    for p in propsOf frag do
        let v = p.GetValue(frag)

        match v with
        | :? TSqlFragment as child -> printAst (level + 1) (p.Name + ": ") child
        | :? string ->
            match simpleValue v with
            | Some s -> printfn "%s%s = %s" (indent (level + 1)) p.Name s
            | None -> ()
        | :? IEnumerable as items when not (v :? string) ->
            let arr = items |> Seq.cast<obj> |> Seq.toArray

            let fragItems =
                arr
                |> Array.choose (function
                    | :? TSqlFragment as child -> Some child
                    | _ -> None)

            if fragItems.Length > 0 then
                printfn "%s%s:" (indent (level + 1)) p.Name

                fragItems
                |> Array.iteri (fun i child -> printAst (level + 2) (sprintf "[%d]: " i) child)
            else
                let simpleItems = arr |> Array.choose simpleValue

                if simpleItems.Length > 0 then
                    printfn "%s%s = [%s]" (indent (level + 1)) p.Name (String.Join(", ", simpleItems))
        | _ ->
            match simpleValue v with
            | Some s -> printfn "%s%s = %s" (indent (level + 1)) p.Name s
            | None -> ()

match mode with
| CliArgs.Tree -> printParseTree 0 "" fragment
| CliArgs.Ast -> printAst 0 "" fragment
| CliArgs.File _ -> invalidOp "impossible mode"
