open System
open CommandLineParser
open Days

let logPart prefix result = 
    match result with 
    | None -> 
        printfn "%s: Not implemented" prefix
    | Some ans -> 
        printfn "%s: %s" prefix ans

let logDay (one,two)=
     logPart "Part one" one
     logPart "Part two" two  

let getDay day = 
    match day with
    | 1 -> Day1.solve
    | 2 -> Day2.solve
    | 3 -> Day3.solve
    | 4 -> Day4.solve
    | 5 -> Day5.solve
    | 6 -> Day6.solve
    | 7 -> Day7.solve
    | _ -> (fun _ -> (None, None))

let runDay day fileLines = 
    day
    |> getDay 
    <| fileLines 
    |> logDay

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
                IO.File.ReadLines path |> Seq.toList |> runDay day
    0 // return an integer exit code

