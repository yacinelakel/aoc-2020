module Days.Day6

open Core

let solve (filelines: string list): (string option * string option) =
    
    let toCharSet = Seq.toList >> Set.ofList

    let cntUnion list =
        let union aSet line =
            line 
            |> toCharSet
            |> Set.union aSet

        list |> List.fold union Set.empty |> Set.count

    let cntIntersect list =
        let intersect aSet line =
            line 
            |> toCharSet
            |> Set.intersect aSet

        list
        |> List.fold intersect (Set.ofList [ 'a' .. 'z' ])
        |> Set.count

    let chuncks = split "" filelines

    let p1 = List.sumBy cntUnion chuncks
    let p2 = List.sumBy cntIntersect chuncks

    toSomeStr2 (p1, p2)
