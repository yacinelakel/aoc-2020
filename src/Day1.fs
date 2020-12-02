module Days.Day1

open Days.Core

let solve (fileLines: seq<string>) =

    // Solve using reccursion
    let partOneRecursion (input) =
        let isMatch2 x y = x + y = 2020

        let rec findMatch l1 l2 =
            match l1, l2 with
            | x :: _, y :: ys -> if isMatch2 x y then Some(x * y) else findMatch l1 ys
            | _ :: xs, [] -> findMatch xs input
            | _, _ -> None

        findMatch input input

    let partTwoRecursion (input) =
        let isMatch3 x y z = x + y + z = 2020

        let rec findMatch l1 l2 l3 =
            match l1, l2, l3 with
            | x :: _, y :: _, z :: zs -> if isMatch3 x y z then Some(x * y * z) else findMatch l1 l2 zs
            | _ :: _, _ :: ys, [] -> findMatch l1 ys input
            | _ :: xs, [], _ -> findMatch xs input input
            | _, _, _ -> None

        findMatch input input input

    // Solve using higher order functions
    // allPairs -> creates a cartesian product of two lists
    // tryFind -> finds first element that satisfies predicate
    let partOneHigherOrder input =
        let isMatch2 x y = x + y = 2020

        let ans =
            (input, input)
            ||> List.allPairs
            |> List.tryFind (fun (x, y) -> isMatch2 x y)

        match ans with
        | None -> None
        | Some (x, y) -> Some(x * y)

    let partTwoHigherOrder (input: int list) =
        let isMatch3 x y z = x + y + z = 2020

        let ans =
            (input, input)
            ||> List.allPairs
            |> List.allPairs input
            |> List.tryFind (fun (z, (x, y)) -> isMatch3 x y z)

        match ans with
        | None -> None
        | Some (z, (x, y)) -> Some(x * y * z)

    // Generic solution for both problems (slow)
    // n -> number of elements that sum up to 2020
    let partGeneric input n =
        let isMatchList l = List.sum l = 2020

        let toMatchList l =
            if isMatchList l then Some(List.fold (*) 1 l) else None

        let find2020 list =
            List.fold (fun s l -> if s = None then toMatchList l else s) None list

        // Creates a Cartesian product of a list and a list of list
        // returns a new list of lists
        let allPairs list2 list1 =
            (list1, list2)
            ||> List.allPairs
            |> List.map (fun (l, e) -> (l @ [ e ]))

        // Create function that applys a function n time recursivly
        // f x y -> f x (f x y)
        let rec applyNtimes (f: int list -> int list list -> int list list) (x: int list) (y: int list list) n =
            match n > 1 with
            | true -> applyNtimes f x (f x y) (n - 1)
            | false -> (f x y)

        applyNtimes (allPairs) input [ [] ] n |> find2020

    // Using list generation with for loops to create n-cartesian product list
    let partOneWithForLoopGenerators input =
        let ans =
            [ for x in input do
                for y in input -> [ x; y ] ]
            |> List.tryFind (fun l -> List.sum l = 2020)

        match ans with
        | Some l -> Some(List.fold (*) 1 l)
        | _ -> None

    let partTwoWithForLoopGenerators input =
        let ans =
            [ for x in input do
                for y in input do
                    for z in input -> [ x; y; z ] ]
            |> List.tryFind (fun l -> List.sum l = 2020)

        match ans with
        | Some l -> Some(List.fold (*) 1 l)
        | _ -> None

    // I like this best for bruteforcing. Clean and consise
    let partOneAndTwoWithForLoopGenerator input =
        let cprod2 =
            [ for x in input do
                for y in input -> [ x; y ] ]

        let cprod3 =
            [ for l in cprod2 do
                for z in input -> l @ [ z ] ]

        let findMatch prodList =
            List.tryFind (fun l -> List.sum l = 2020) prodList

        let toResult result =
            match result with
            | Some l -> Some(List.fold (*) 1 l)
            | _ -> None

        (cprod2 |> findMatch |> toResult, cprod3 |> findMatch |> toResult)

    // 'Optimal' solutions
    let partTwoToSum input =
        let twoSum list sum =
            // Scan for canidates
            let rec search s sList =
                match sList with
                | [ _ ] | [] -> None
                | a::rest ->
                    let b = List.last rest
                    if (a + b) = s then Some(a, b)
                    else if (a + b) > s then search s sList.[..(sList.Length - 2)]
                    else search s rest

            // input must be sorted
            list |> List.sort |> search sum 

        match (twoSum input 2020) with
        | Some (a, b) -> Some(a * b)
        | None -> None

    // Quadratic solution to 3SUM https://en.m.wikipedia.org/wiki/3SUM
    let partTwoThreeSum input =
        let threeSum list sum =
            // Scan for second and third canidate
            let rec search2 s sList a =
                match sList with
                | [ _ ] | [] -> None
                | b::rest ->
                    let c = List.last rest
                    if (a + b + c) = s then Some(a, b, c)
                    else if (a + b + c) > s then search2 s sList.[..(sList.Length - 2)] a // remove last item
                    else search2 s rest a // remove first item

            // Choose first element as canidate,
            // search for solution given that canidate
            let rec search3 s sList =
                match sList with
                | [ _; _ ] | [ _ ] | [] -> None
                | x :: rest ->
                    match search2 s sList x with
                    | None -> search3 s rest
                    | Some a -> Some a

            // input must be sorted
            list |> List.sort |> search3 sum

        match (threeSum input 2020) with
        | Some (a, b, c) -> Some(a * b * c)
        | None -> None



    let input = parseInput (fun x -> (int x)) fileLines

    (partTwoToSum input, partTwoThreeSum input)
