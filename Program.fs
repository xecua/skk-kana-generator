// For more information see https://aka.ms/fsharp-console-apps

open Argu
open SkkKanaGenerator.Types
open SkkKanaGenerator.OutputFormatter

type Arguments =
    | [<ExactlyOnce>] Format of OutputFormat
    | [<MainCommand>] Input of path: string
    | Style of string
    | No_Comment

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Format _ -> "output format"
            | Input _ -> "input file path"
            | Style _ -> "style (CorvusSKK: xml, original(default) / skkeleton: vim(default), lua)"
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

    let excludeComment = result.Contains No_Comment
    let inputPath = result.GetResult(Input, "/dev/stdin")

    let formatter: Formatter =
        match (result.GetResult Format) with
        | AquaSkk -> new AquaSkkFormatter(excludeComment)
        | MacSKK -> new MacSkkFormatter(excludeComment)
        | Libskk -> new LibskkFormatter()
        | Libcskk -> new LibcskkFormatter(excludeComment)
        | CorvusSKK ->
            new CorvusSkkFormatter(excludeComment, result.TryGetResult Style |> Option.defaultValue "original")
        | Skkeleton -> new SkkeletonFormatter(excludeComment, result.TryGetResult Style |> Option.defaultValue "vim")

    SkkKanaGenerator.InputParser.parseFile inputPath
    |> formatter.formatLines
    |> printfn "%s"

    0
