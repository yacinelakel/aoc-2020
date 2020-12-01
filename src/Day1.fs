module Days.Day1

let solve (fileLines: seq<string>) =

    let partOne (input) = 
        let rec partOneRec l1 l2 = 
            match l1, l2 with 
                | x::_, y::ys -> 
                    match x + y with
                    | 2020 -> 
                        Some (x*y)
                    | _ -> 
                        partOneRec  l1 ys
                | _::xs, [] -> partOneRec xs input
                | _,_ -> None
        partOneRec input input

    let partTwo (input) = 
        let rec partTwoRec l1 l2 l3 = 
                match l1, l2, l3 with 
                    | x::_, y::_, z::zs -> 
                        match x + y + z with
                        | 2020 -> 
                            Some (x*y*z)
                        | _ -> 
                            partTwoRec l1 l2 zs
                    | _::_, _::ys, [] -> partTwoRec l1 ys input 
                    | _::xs, [], _ -> partTwoRec xs input input
                    | _,_,_ -> None
        partTwoRec input input input

    let parseInput (fileLines: seq<string>) = Seq.map (fun x -> (int x)) fileLines |> Seq.toList

    let input = parseInput fileLines
    
    (partOne input, partTwo input)
