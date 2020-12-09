module Days.Day9

open Core

let solve (filelines: string list) =

    let partOne (nList: int64 list) =
        let twoSum s list=
            let rec twoSumRec s sList =
                    match sList with
                    | [ _ ]
                    | [] -> None
                    | a :: rest ->
                        let b = List.last rest
                        if (a + b) = s then Some(a, b)
                        else if (a + b) > s then twoSumRec s sList.[..(sList.Length - 2)]
                        else twoSumRec s rest

            twoSumRec s (list |> List.sort)

        let rec findInvalid preamble rest =
            match rest with
            | x :: xs ->
                let pair = twoSum x preamble
                match pair with
                | Some _ ->
                    let tail = List.tail preamble
                    findInvalid (tail @ [ x ]) xs
                | None -> Some x
            | _ -> None
        
        findInvalid nList.[0..25] nList.[25..]

    // Naive....
    let partTwoNaive n (list: list<int64>) =
        // Find sum list using previous windows for next window
        let rec findSumList (i: int) wSize (wMap: Map<int, int64>) =
            if wSize = list.Length then
                None
            else if (i + wSize) = list.Length then
                findSumList 0 (wSize + 1) wMap
            else
                let wSum = wMap.[i] + list.[i + wSize]
                if wSum = n then
                    Some(i, wSize)
                else
                    let wMap = Map.add i wSum wMap
                    findSumList (i + 1) wSize wMap

        let initMap =
            list
            |> List.mapi (fun i v -> (i, v))
            |> Map.ofList

        findSumList 0 1 initMap

    let partTwoBetter (n:int64) list =
        let rec findSum sList n rest =  
            let sum = List.sum sList
            if sum > n then findSum (List.tail sList) n rest
            else if sum = n then Some sList
            else 
                match rest with
                | [] -> None
                | x::xs -> findSum (sList @ [x]) n xs
        findSum [] n list

    let numbers = List.map (int64) filelines
    
    let p1 = partOne numbers

    let p2 =
        match p1 with
        | None -> None
        | Some a ->
            match partTwoBetter a numbers with
            | Some cList -> Some((List.min cList) + (List.max cList))
            | None -> None

    toSomeStr2 (p1, p2)