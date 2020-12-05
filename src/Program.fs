open System
open CommandLineParser
open Days



let printPart prefix result = 
    match result with 
    | None -> 
        printfn "%s: Not implemented or found" prefix
    | Some ans -> 
        printfn "%s: %A" prefix ans

let printDay<'a, 'b> ((one,two): ('a option) * ('b option)) =
     printPart "Part one" one
     printPart "Part two" two  

let runDay (day:int) (fileLines: seq<string>) = 
    match day with
    | 1 -> 
        fileLines |> Day1.solve |> printDay
    | 2 -> 
        fileLines |> Day2.solve |> printDay
    | 3 -> 
        fileLines |> Day3.solve |> printDay
    | 4 -> 
        fileLines |> Day4.solve |> printDay
    | _ ->  printDay (None, None)

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
                IO.File.ReadLines path |> runDay day
    0 // return an integer exit code

