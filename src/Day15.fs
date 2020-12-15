module Days.Day15

open Core
open System

type Spoken =
    | FirstTime of int
    | LastTwoTimes of int * int


let solve (filelines: string list) =
    let findNthNumber n numList =
        let findNthNumber (lastSpoken, spokenMap) turn =
            let curSpoken =
                    match Map.find lastSpoken spokenMap with
                    | FirstTime _ -> 0
                    | LastTwoTimes (t1, t2) -> t1 - t2

            let newSpoken =
                match Map.tryFind curSpoken spokenMap with
                | Some s ->
                    match s with
                    | FirstTime ft -> LastTwoTimes(turn, ft)
                    | LastTwoTimes (t1, _) -> LastTwoTimes(turn, t1)
                | None -> (FirstTime turn)

            (curSpoken, Map.add curSpoken newSpoken spokenMap)

        let ls, map =
            List.last numList,
            numList
            |> List.mapi (fun i x -> (x, FirstTime(i + 1)))
            |> Map.ofList

        let (ans, _) =
             [(List.length numList + 1) .. n ]
            |> List.fold findNthNumber (ls, map)

        ans

    let nums =
        filelines.[0].Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.map int

    toSomeStr2 (findNthNumber 2020 nums, findNthNumber 30000000 nums)
