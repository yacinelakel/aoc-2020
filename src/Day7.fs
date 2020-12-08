module Days.Day7

open Core

type Bag = { Shade: string; Color: string }

type Rule = Bag * (int * Bag) list

let solve fileline =

    let pareseLine (line: string): (Rule) =
        let rec parseContains state line =
            match line with
            | " no other bags."
            | "." -> state
            | Regex @"^,?\s(\d)\s(\w+)\s(\w+) bag[s]?(.+)" [ amount; shade; color; rest ] ->
                let bag = { Shade = shade; Color = color }
                parseContains ((int amount, bag) :: state) rest
            | _ -> failwith "parseError:"

        match line with
        | Regex @"^(\w+)\s(\w+) bags contain(.+)" [ shade; color; rest ] ->
            ({ Shade = shade; Color = color }, parseContains [] rest)
        | _ -> failwith "parseError"

    let rec getParents (map: Map<Bag, (int * Bag) list>) (bag: Bag): (Set<Bag>) =
        let (parentsMap, _) =
            map
            |> Map.partition (fun _ v -> List.exists (fun (_, b) -> b = bag) v)

        if parentsMap.IsEmpty then
            Set.empty
        else
            let parents =
                parentsMap
                |> Map.toList
                |> List.map (fun (k, _) -> k)

            let parentsParents =
                List.map (fun b -> getParents map b) parents

            List.fold (Set.union) (Set.ofList parents) parentsParents

    let rec getChildBagCnt (map: Map<Bag, (int * Bag) list>) (bag: Bag) =
        match map.TryGetValue bag with
        | (false, _) -> 0
        | (true, children) ->
            match children.IsEmpty with
            | true -> 0
            | false -> List.fold (fun acc (a, b) -> acc + (a + (a * getChildBagCnt map b))) 0 children

    let rulesMap =
        fileline |> List.map pareseLine |> Map.ofList

    let shinyGold = { Shade = "shiny"; Color = "gold" }

    let p1 = shinyGold |> getParents rulesMap |> Set.count

    let p2 = shinyGold |> getChildBagCnt rulesMap

    toSomeStr2 (p1, p2)
