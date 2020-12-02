namespace Days
type Policy =
    { Low: int
      High: int
      Char: char
      Password: string }

module Day2 =
    open System.Text.RegularExpressions
    open Days.Core

    let solve (fileLines: seq<string>) =
        
        let partOne input =
            let valid (p:Policy) = 
                let occ x = Seq.filter ((=) x) >> Seq.length
                let occP = p.Password |> occ p.Char
                occP >= p.Low && occP <= p.High

            Some(input |> List.filter valid |> List.length)

        let partTwo input =
            let valid (p : Policy) = 
                let hasLow = p.Password.[p.Low - 1] = p.Char
                let hasHigh = p.Password.[p.High - 1] = p.Char
                (hasLow || hasHigh) && not (hasLow && hasHigh) 

            Some (input |> List.filter valid |> List.length)

        let parseLine line =
            match line with
            | Regex @"(\d+)\-(\d+)\s([a-z]): (.+)" [ l; h; c; p ] ->
                { Low = (int l)
                  High = (int h)
                  Char = (char c)
                  Password = p }
            | _ -> failwith "parse error"

        let input = parseInput parseLine fileLines

        (partOne input, partTwo input)
