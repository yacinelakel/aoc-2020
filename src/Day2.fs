namespace Days

type Policy =
    { Low: int
      High: int
      Char: char
      Password: string }

module Day2 =
    open Days.Core

    let solve (fileLines: seq<string>) =

        let isValidPartOne p =
            let occ x = Seq.filter ((=) x) >> Seq.length
            let occP = p.Password |> occ p.Char
            occP >= p.Low && occP <= p.High

        let isValidPartTwo p =
            let hasLow = p.Password.[p.Low - 1] = p.Char
            let hasHigh = p.Password.[p.High - 1] = p.Char
            hasLow <> hasHigh

        let countValidHO pred list =
            (pred, list) ||> List.filter |> List.length

        let countValidRec pred list =
            let rec loop l c =
                match l with
                | p :: rest ->
                    match (pred p) with
                    | true -> loop rest (c + 1)
                    | false -> loop rest c
                | [] -> c

            loop list 0

        let solveHO input =
            (isValidPartOne, input) ||> countValidHO |> Some, (isValidPartTwo, input) ||> countValidHO |> Some

        let solveRec input =
            (isValidPartOne, input) ||> countValidHO |> Some, (isValidPartTwo, input) ||> countValidHO |> Some

        let parseLine line =
            match line with
            | Regex @"(\d+)\-(\d+)\s([a-z]): (.+)" [ l; h; c; p ] ->
                { Low = (int l)
                  High = (int h)
                  Char = (char c)
                  Password = p }
            | _ -> failwith "parse error"

        fileLines |> parseInput parseLine |> solveHO
