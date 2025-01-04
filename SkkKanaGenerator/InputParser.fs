module SkkKanaGenerator.InputParser

open SkkKanaGenerator.Types
open SkkKanaGenerator.Kana

let private convertHira (table: Map<string, string>) (hira: string) =
    hira.ToCharArray()
    |> Array.map (fun c -> Map.tryFind (c.ToString()) table |> Option.defaultValue (c.ToString()))
    |> String.concat ""

let parseLine line =
    match line with
    | "" -> Empty
    | _ when line.StartsWith(";") -> Comment line
    | _ ->
        match line.Split('\t') with
        | [| rom; hira |] ->
            Rule
                { Rom = rom
                  Hira = hira
                  Kata = convertHira hiraToKana hira
                  HanKata = convertHira hiraToHanKata hira
                  Okuri = "" }
        | [| rom; hira; kata |] ->
            Rule
                { Rom = rom
                  Hira = hira
                  Kata = kata
                  HanKata = convertHira hiraToHanKata hira
                  Okuri = "" }
        | [| rom; hira; kata; hanKata |] ->
            Rule
                { Rom = rom
                  Hira = hira
                  Kata = kata
                  HanKata = hanKata
                  Okuri = "" }
        | [| rom; hira; kata; hanKata; okuri |] ->
            Rule
                { Rom = rom
                  Hira = hira
                  Kata = kata
                  HanKata = hanKata
                  Okuri = okuri }
        | _ -> failwith "Invalid line"

let parseFile path =
    System.IO.File.ReadAllLines(path)
    |> Array.mapi (fun i line ->
        try
            parseLine line
        with e ->
            eprintfn $"{e.Message} at line {i}: {line}"
            exit 1)
