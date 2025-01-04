// For more information see https://aka.ms/fsharp-console-apps

open Argu
open SkkKanaGenerator.Types

type Arguments =
    | [<ExactlyOnce>] Format of OutputFormat
    | [<MainCommand>] Input of path: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Format _ -> "output format"
            | Input _ -> "Input file path"


[<EntryPoint>]
let main _ =
    let parser = ArgumentParser.Create<Arguments>()

    let result =
        try
            parser.ParseCommandLine()
        with e ->
            eprintfn "%s" e.Message
            exit 1

    let inputPath = result.TryGetResult Input |> Option.defaultValue "/dev/stdin"

    let lines = SkkKanaGenerator.InputParser.parseFile inputPath

    0
