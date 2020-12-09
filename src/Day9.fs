module Days.Day9

open Core

let solve (filelines: string list) =

    // 2SUM impl from Day1
    let twoSum list (sum: int64) =
            // Scan for canidates
            let rec search s sList =
                match sList with
                | [ _ ]
                | [] -> None
                | a :: rest ->
                    let b = List.last rest
                    if (a + b) = s then Some(a, b)
                    else if (a + b) > s then search s sList.[..(sList.Length - 2)]
                    else search s rest

            // input must be sorted
            list |> List.sort |> search sum

    let partOne (nList: int64 list) =
        let rec loop preambleS rest =
            match rest with
            | x :: xs ->
                let pair = twoSum preambleS x
                match pair with
                | Some _ ->
                    let tail = List.tail preambleS
                    loop (tail @ [ x ]) xs
                | None -> Some x
            | _ -> None

        loop nList.[0..25] nList.[25..]

    let partTwo n (list: list<int64>) =
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

    let numbers = List.map (int64) filelines

    let p1 = partOne numbers

    let p2 =
        match p1 with
        | None -> None
        | Some a ->
            match partTwo a numbers with
            | Some (i, step) ->
                let cList = numbers.[i..(i + step)]
                Some((List.min cList) + (List.max cList))
            | None -> None

    toSomeStr2 (p1, p2)
