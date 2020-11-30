open System
open CommandLineParser

let runDay (day:int) (fileLines: seq<string>) = 
    match day with
    | 1 -> Day1.solve fileLines
    | _ -> eprintf "Could not find solver for day %d" day

[<EntryPoint>]
let main argv =
    let result = Array.toList argv |> parse
    match result with
    | InvalidCommand -> eprintfn "Invalid command"
    | ValidCommand cmd ->
        match cmd.Day with
        | NoDay -> eprintfn "Missing day option (-d)"
        | Day day ->
            match cmd.Input with
            | NoInput -> eprintfn "Missing input option (-i)"
            | Input filename -> 
                IO.File.ReadLines filename |> runDay day 
    0 // return an integer exit code

