// For more information see https://aka.ms/fsharp-console-apps

open Argu
open SkkKanaGenerator.Types
open SkkKanaGenerator.OutputFormatter

type Arguments =
    | [<ExactlyOnce>] Format of OutputFormat
    | [<MainCommand>] Input of path: string
    | No_Comment

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Format _ -> "output format"
            | Input _ -> "input file path"
            | No_Comment -> "exclude comments"


[<EntryPoint>]
let main _ =
    let parser = ArgumentParser.Create<Arguments>()

    let result =
        try
            parser.ParseCommandLine()
        with e ->
            eprintfn "%s" e.Message
            exit 1

    let includeComment = result.Contains No_Comment |> not
    let inputPath = result.GetResult(Input, "/dev/stdin")

    let formatter: Formatter =
        match (result.GetResult Format) with
        | AquaSkk -> new AquaSkkFormatter(includeComment)
        | MacSKK -> new MacSkkFormatter(includeComment)
        | Libskk -> new LibskkFormatter()
        | Libcskk -> new LibcskkFormatter(includeComment)
        | CorvusSKK -> new CorvusSkkFormatter()
        | Skkeleton -> new SkkeletonFormatter(includeComment)

    SkkKanaGenerator.InputParser.parseFile inputPath
    |> formatter.formatLines
    |> printfn "%s"

    0
