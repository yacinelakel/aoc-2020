module Days.Day1

let solve (fileLines: seq<string>) =

    // Solve using reccursion
    let partOneRecursion (input) = 
        let isMatch2 x y = x + y = 2020
        
        let rec findMatch l1 l2 = 
            match l1, l2 with 
                | x::_, y::ys -> 
                    if isMatch2 x y then Some (x*y) else findMatch  l1 ys
                | _::xs, [] -> findMatch xs input
                | _,_ -> None
        findMatch input input

    let partTwoRecursion (input) = 
        let isMatch3 x y z = x + y + z = 2020

        let rec findMatch l1 l2 l3 = 
                match l1, l2, l3 with 
                    | x::_, y::_, z::zs -> 
                         if isMatch3 x y z then Some (x*y*z) else findMatch l1 l2 zs
                    | _::_, _::ys, [] -> findMatch l1 ys input 
                    | _::xs, [], _ -> findMatch xs input input
                    | _,_,_ -> None
        findMatch input input input

    // Solve using higher order functions (exhaustive)
    // allPairs -> creates a cartesian product of two lists
    // tryFind -> finds first element that satisfies predicate
    let partOneHigherOrder input =
        let isMatch2 x y = x + y = 2020

        let ans = 
            (input, input)
            ||> List.allPairs 
            |> List.tryFind (fun (x,y) -> isMatch2 x y)
                     
        match ans with
        | None -> None
        | Some (x,y) -> Some (x*y)

    let partTwoHigherOrder (input: int list) =
        let isMatch3 x y z = x + y + z = 2020
        let ans = 
            (input, input)
            ||> List.allPairs
            |> List.allPairs input
            |> List.tryFind (fun (z,(x,y)) -> isMatch3 x y z)
                     
        match ans with
        | None -> None
        | Some (z,(x,y)) -> Some (x*y*z)

    // Generic solution for both problems (exhaustive)
    // n -> number of elements that sum up to 2020
    let partGeneric input n = 
        let isMatchList l =
                List.sum l = 2020

        let toMatchList l = 
            if isMatchList l then Some (List.fold (*) 1 l) else None 

        let find2020 list = List.fold (fun s l -> if s = None then toMatchList l else s) None list

        // Creates an n-fold Cartesian product
        let allPairs list2 list1 =
            (list1, list2)
            ||> List.allPairs
            |> List.map (fun (l, e) -> (l @ [e]))
           
        let rec applyNtimes (f: int list -> int list list -> int list list)  (x:int list) (y:int list list) n = 
            match n > 1 with
            | true -> applyNtimes f x (f x y) (n-1)        
            | false -> (f x y)
        
        applyNtimes (allPairs) input [[]] n |> find2020

    let parseInput (fileLines: seq<string>) = Seq.map (fun x -> (int x)) fileLines |> Seq.toList

    let input = parseInput fileLines
    
    (partGeneric input 2, partGeneric input 3)


