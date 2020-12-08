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

    let runUntilTerminated (instructions: Instruction list) =
        let changeInstruction inst =
            if inst.op = Noop then { inst with op = Jump } else { inst with op = Noop }

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

    let instructions = parse filelines
    let (p1, _) = run instructions
    let (p2, _) = runUntilTerminated instructions

    toSomeStr2 (p1, p2)
