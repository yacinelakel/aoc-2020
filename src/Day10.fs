module Days.Day10

open Core

type Tree3 =
    | Node of int * Tree3 * Tree3 * Tree3
    | Leaf



let solve (filelines: string list) =


    let partOne sList =
    
        let rec getDiffs (one, two, three) list =
            match list with
            | x :: y :: rest ->
                let l = (y :: rest)
                match y - x with
                | 1 -> getDiffs (one + 1, two, three) l
                | 2 -> getDiffs (one, two + 1, three) l
                | _ -> getDiffs (one, two, three + 1) l
            | [ x ] -> (one, two, three + 1)
            | [] -> (one, two, three)

        let (one, _, three) = getDiffs (1, 0, 0) sList
        one * three


    let partTwo sList =

        let rec insert (tree: Tree3) (value: int): Tree3 =
            match tree with
            | Leaf -> Node(value, Leaf, Leaf, Leaf)
            | Node (v, left, mid, right) ->
                let diff = value - v
                if diff < 0 then
                    failwith "Diff is negative"
                else
                    match diff with
                    | 1 ->
                        match left with
                        | Leaf -> Node(v, Node(value, Leaf, Leaf, Leaf), mid, right)
                        | _ -> failwith "Cannot insert left"
                    | 2 ->
                        match mid with
                        | Leaf -> Node(v, left, Node(value, Leaf, Leaf, Leaf), right)
                        | _ -> failwith "Cannot insert mid"
                    | 3 ->
                        match right with
                        | Leaf -> Node(v, left, mid, Node(value, Leaf, Leaf, Leaf))
                        | _ -> failwith "Cannot insert right"
                    | _ ->
                        match right with
                        | Leaf ->
                            match mid with
                            | Leaf ->
                                match left with
                                | Leaf -> failwith ("Diff is greater than 3")
                                | _ -> Node(v, insert left value, mid, right)
                            | _ -> Node(v, left, insert mid value, right)
                        | _ -> Node(v, left, mid, insert right value)

        let rec count (tree: Tree3) state =
            let countLeft tree state =
                match tree with
                | Leaf -> state
                | Node (_, left, mid, right) ->
                    match left with
                    | Node (vl, Leaf, Leaf, Leaf) ->
                        match right with
                        | Leaf -> count (Node(vl, mid, Leaf, Leaf)) state
                        | Node (_, lr, _, _) -> count (Node(vl, mid, right, lr)) state
                    | _ -> count left state

            let countMid tree state =
                match tree with
                | Leaf -> state
                | Node (_, _, mid, right) ->
                    match mid with
                    | Node (vm, Leaf, Leaf, Leaf) ->
                        match right with
                        | Leaf -> state
                        | Node (_, lr, mr, _) -> count (Node(vm, right, lr, mr)) state
                    | _ -> count mid state

            let countRight tree state =
                match tree with
                | Leaf -> state
                | Node (_, _, _, right) -> count right state

            let getCount tree map =
                match tree with
                | Node (v, _, _, _) -> if (Map.containsKey v map) then map.[v] else 0L
                | Leaf -> 0L

            match tree with
            | Node (v, Leaf, Leaf, Leaf) -> Map.add v 1L state
            | Node (v, left, mid, right) ->
                if (Map.containsKey v state) then
                    state
                else
                    let rState = countRight tree state
                    let mState = countMid tree rState
                    let lstate = countLeft tree mState

                    let lc = getCount left lstate
                    let mc = getCount mid lstate
                    let rc = getCount right lstate

                    Map.add v (lc + mc + rc) lstate
            | Leaf -> state

        let startTree = Node(0, Leaf, Leaf, Leaf)

        let sList' = sList @ [ sList.[sList.Length - 1] + 3 ]
        let tree = List.fold insert startTree sList'

        let sumMap = (count tree Map.empty)

        sumMap.[0]

    let adapters = List.map (int) filelines
    let sAdapters = List.sort adapters

    toSomeStr2 (partOne sAdapters, partTwo sAdapters)
