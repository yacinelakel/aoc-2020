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

    // ---------------------------------------------------
    let getEdges (iList:Instruction list) = 
        let folder acc inst = 
            let (edges,edgesRev, canEdges) = acc
            let i = inst.id
            let (too, from) = 
                match inst.op with
                | Acc | Noop -> (i, (i + 1))
                 | Jump -> (i, (jump inst.id inst.sign inst.num))
            let canEdges' = 
                match inst.op with
                | Acc -> canEdges
                | Noop -> canEdges @ [(i, (jump i inst.sign inst.num))]
                | Jump -> canEdges @ [(i, (i + 1))]
            let edgesRev' = Map.change from (fun v -> match v with | Some a -> Some (too::a) | None -> Some [too] ) edgesRev
            (Map.add too from edges, edgesRev', canEdges')
        
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
        | (t, f)::xs -> 
            if (s1.Contains t) && (s2.Contains f) then Some (t,f) else 
                findChangeRec s1 s2 xs
        | [] -> None 
        

    let instructions = parse filelines
    let (p1, _) = run instructions
    // let (p2,nList) = runUntilTerminated instructions

    let (edges, edgesRev, canidateEdges) = getEdges instructions
    let s1 = traverse edges 0
    let s2 = bfs edgesRev instructions.Length
    
    let edge = findChangeRec s1 s2 canidateEdges
    let p2 = 
        match edge with 
        | None -> None
        | Some (t,f) -> 
            instructions 
            |> updateElement t (changeInstruction)
            |> run
            |> Some 

    toSomeStr2 (p1, p2)

    // toSomeStr2 (p1, p2)
