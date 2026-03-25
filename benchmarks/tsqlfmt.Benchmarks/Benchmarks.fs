module TSqlFormatter.Benchmarks

open System
open System.IO
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open TSqlFormatter.Style
open TSqlFormatter.Formatter

[<MemoryDiagnoser>]
type FormatterBenchmarks() =
    let repoRoot = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
    let testDataDir = Path.Combine(repoRoot, "tests", "tsqlfmt.Tests", "TestData")
    let defaultStylePath = Path.Combine(repoRoot, "default-style.json")

    let inputs =
        [| Path.Combine(testDataDir, "test1.actual.sql")
           Path.Combine(testDataDir, "test15.actual.sql")
           Path.Combine(testDataDir, "test27.actual.sql") |]

    let style = loadStyle defaultStylePath

    [<DefaultValue>]
    val mutable public sql: string

    [<ParamsSource("InputFiles")>]
    member val InputFile = "" with get, set

    member _.InputFiles = inputs

    [<GlobalSetup>]
    member this.Setup() =
        this.sql <- File.ReadAllText(this.InputFile)

    [<Benchmark>]
    member this.Format() =
        match format style this.sql with
        | Ok formatted -> formatted
        | Error errors -> failwith (String.concat "; " errors)

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<FormatterBenchmarks>() |> ignore
    0
