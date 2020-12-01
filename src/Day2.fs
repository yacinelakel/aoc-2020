module Days.Day2

let solve (fileLines:seq<string>) =

    let partOne input =
        None

    let partTwo input =
        None
    
    let parseInput (fileLines) =
        Seq.map (fun x -> (int x)) fileLines |> Seq.toList

    let input = parseInput fileLines

    (partOne input, partTwo input)
