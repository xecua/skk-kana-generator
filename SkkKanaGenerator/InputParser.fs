module SkkKanaGenerator.InputParser

open SkkKanaGenerator.Types

let parseLine line =
    match line with
    | "" -> Empty
    | _ when line.StartsWith("#") -> Comment(line[1..].TrimStart())
    | _ ->
        match line.Split('\t') with
        | [| rom; hira |] -> Rule { Rom = rom; Hira = hira; Okuri = None }
        | [| rom; hira; okuri |] ->
            Rule
                { Rom = rom
                  Hira = hira
                  Okuri = Some okuri }
        | _ -> failwith "Invalid line"

let parseFile path =
    System.IO.File.ReadAllLines(path)
    |> Array.mapi (fun i line ->
        try
            parseLine line
        with e ->
            eprintfn $"{e.Message} at line {i}: {line}"
            exit 1)
