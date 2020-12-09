module Days.Day8

open Core

type Operation =
    | Acc
    | Jump
    | Noop

type Sign =
    | Positive
    | Negative

type Instruction =
    { id: int
      op: Operation
      sign: Sign
      num: int }

let solve filelines =

    let getOp str =
        match str with
        | "acc" -> Acc
        | "jmp" -> Jump
        | "nop" -> Noop
        | _ -> failwith "parseError"

    let getSign str =
        match str with
        | "+" -> Positive
        | "-" -> Negative
        | _ -> failwith "parseError"

    let parseLine i line =
        match line with
        | Regex @"^(acc|jmp|nop) ([+-])(\d+)" [ op; sign; num ] ->
            { id = i
              op = getOp op
              sign = getSign sign
              num = int num }
        | _ -> failwith "parseError"

    let parse lines = List.mapi parseLine lines

    let jump i sign n =
        match sign with
        | Positive -> i + n
        | Negative -> i - n

    let acc a sign n =
        match sign with
        | Positive -> a + n
        | Negative -> a - n

    let rec runRec (i: int) (a: int) (ranMap: Map<int, bool>) (instructions: Instruction list) =
        if i = instructions.Length then
            (a, true) // terminated successfully
        else
            let inst = instructions.[i]
            match Map.tryFind inst.id ranMap with
            | Some _ -> (a, false) // terminated with loop
            | _ ->
                let (i', a') =
                    match inst.op with
                    | Acc -> (i + 1, acc a inst.sign inst.num)
                    | Jump -> (jump i inst.sign inst.num, a)
                    | Noop -> (i + 1, a)

                let ranMap' = ranMap.Add((inst.id), true)

                runRec i' a' ranMap' instructions

    let run = runRec 0 0 Map.empty

    let updateElement i f list =
        list
        |> List.mapi (fun i' v -> if i = i' then f v else v)

    let changeInstruction inst =
            if inst.op = Noop then { inst with op = Jump } else { inst with op = Noop }

    // Bruteforce implementation
    let runUntilTerminated (instructions: Instruction list) =
        let rec tryFix (i: int) (iList: Instruction list) =
            let inst = iList.[i]
            match inst.op with
            | Acc -> tryFix (i + 1) iList
            | Noop
            | Jump ->
                let nList = iList |> updateElement i (changeInstruction)
                let (res, succ) = run nList
                if succ then (res, nList) else tryFix (i + 1) iList

        tryFix 0 instructions

    let getEdges (iList:Instruction list) = 
        let folder acc inst = 
            let (edges,edgesRev, canEdges) = acc
            let i = inst.id
            let (f, t) = 
                match inst.op with
                | Acc | Noop -> (i, (i + 1))
                 | Jump -> (i, (jump inst.id inst.sign inst.num))
            let canEdges' = 
                match inst.op with
                | Acc -> canEdges
                | Noop -> canEdges @ [(i, (jump i inst.sign inst.num))]
                | Jump -> canEdges @ [(i, (i + 1))]
            let edgesRev' = Map.change t (fun v -> match v with | Some a -> Some (f::a) | None -> Some [f] ) edgesRev
            (Map.add f t edges, edgesRev', canEdges')
        
        List.fold folder (Map.empty, Map.empty, []) iList

    let traverse (map:Map<int, int>) (pos:int) = 
        let rec traverseRec (visited:Set<int>) i (map:Map<int,int>) = 
            if visited.Contains i then visited 
            else
                let visited' = visited.Add i
                if not (map.ContainsKey i) then visited' 
                else traverseRec visited' map.[i] map
        
        traverseRec Set.empty pos map 

    let bfs map root = 
        let rec bfsRec (map:Map<int,int list>) queue (visited:Set<int>) =
            match queue with
            | [] -> visited
            | x::xs ->
                let visited = visited.Add x
                if map.ContainsKey x then
                    let edges = map.[x] 
                    let unvisited = List.filter (visited.Contains >> not) edges
                    bfsRec map (xs @ unvisited) visited
                else 
                    bfsRec map xs visited

        bfsRec  map [root] Set.empty 
     
    let rec findChangeRec (s1:Set<int>) (s2:Set<int>) edges =
        match edges with
        | (f, t)::xs -> 
            if (s1.Contains f) && (s2.Contains t) then Some (f,t) else 
                findChangeRec s1 s2 xs
        | [] -> None 
        

    let instructions = parse filelines
    let (p1, _) = run instructions

    // Brute force solution
    // let (p2,_) = runUntilTerminated instructions


    // Solution using graph representation of the instructions
    // edges: All edges in the graph
    // edgesRev: The reverse of the all the edges in the graph
    // canadiateEdges: All edges that can be added to the graph (all possible jmp and nop changes)
    // Runtime: O(n)
    let (edges, edgesRev, canidateEdges) = getEdges instructions 
    
    // Since each node in the graph has only one edge out of a node, 
    // then we can simply traverse from the start to get all nodes in 
    // the subgraph S1
    // Runtime: O(n)
    let s1 = traverse edges 0

    // Since we reversed all the edges, we need to run bfs from
    // the last node (n + 1) to find all the nodes in the graph S2
    // Runtime: In this graph, the number of edges is the same as
    // the number of nodes, so O(n)
    let s2 = bfs edgesRev instructions.Length
    
    // The instruction that needs to be changed is the the same as 
    // the 'from' node in the edge that allows you to go from S1 to S2
    let edge = findChangeRec s1 s2 canidateEdges

    // Change the instruction and run part one
    let p2 = 
        match edge with 
        | None -> None
        | Some (from,_) -> 
            instructions 
            |> updateElement from (changeInstruction)
            |> run
            |> Some 

    toSomeStr2 (p1, p2)
