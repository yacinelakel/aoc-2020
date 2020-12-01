open System
open CommandLineParser
open Days

let runDay (day:int) (fileLines: seq<string>) = 
    match day with
    | 1 -> Day1.solve fileLines
    | _ -> (None, None)

let printResult prefix result = 
    match result with 
    | None -> 
        printfn "%s: Not implemented or found" prefix
    | Some ans -> 
        printfn "%s: %d" prefix ans

[<EntryPoint>]
let main argv =
    let cmd = Array.toList argv |> parseCommandLine
    match cmd with
    | InvalidCommand msg -> eprintfn "%s" msg
    | ValidCommand cmd ->
        match cmd.Day with
        | NoDay -> eprintfn "Missing day option (-d)"
        | Day day ->
            match cmd.FilePath with
            | NoFilePath -> eprintfn "Missing input option (-i)"
            | FilePath path -> 
                let (partOne, partTwo) = IO.File.ReadLines path |> runDay day
                printResult "Part one" partOne
                printResult "Part two" partTwo  
    0 // return an integer exit code

