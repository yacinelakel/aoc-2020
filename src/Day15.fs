module Days.Day15

open Core
open System
open System.Collections.Generic

let solve (filelines: string list) =

    let findNthNumber n nList =

        let mutable dict = new Dictionary<int, int>()
        nList 
        |> List.mapi (fun i x -> dict.[x] <- i + 1)
        |> ignore

        let rec findNthNumber lastSpoken turn =
            let newSpoken =
                match dict.ContainsKey(lastSpoken) with
                | true -> turn - dict.[lastSpoken]
                | false -> 0

            if n = turn then
                lastSpoken
            else
                dict.[lastSpoken] <- turn
                findNthNumber newSpoken (turn + 1)

        findNthNumber 0 (List.length nList |> (+) 1)

    let nList =
        filelines.[0].Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.map int

    toSomeStr2 (findNthNumber 2020 nList, findNthNumber 30000000 nList)
