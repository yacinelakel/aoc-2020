module Days.Day12

open System
open Core

type Cardinal =
    | N
    | S
    | E
    | W

type Direction =
    | L
    | R

type Action =
    | Move of Cardinal
    | Turn of Direction
    | Forward

type Instruction = { Action: Action; Value: int }

let solve filelines =

    let toAction c: Action =
        match c with
        | "N" -> Move N
        | "S" -> Move S
        | "E" -> Move E
        | "W" -> Move W
        | "L" -> Turn L
        | "R" -> Turn R
        | "F" -> Forward
        | _ -> failwith "parse error"

    let parseLine line =
        match line with
        | Regex @"^(\w)(\d+)" [ a; v ] -> { Action = toAction a; Value = int v }
        | _ -> failwith "parse error"

    let move (x, y) car v =
        match car with
        | N -> (x, y + v)
        | S -> (x, y - v)
        | E -> (x + v, y)
        | W -> (x - v, y)

    let rotate (px, py) dir deg =
        let deg =
            match dir with
            | L -> deg
            | R -> -deg

        let (px, py, deg) = float px, float py, float deg
        let theta = deg * Math.PI / 180.0
        let cosT, sinT = Math.Cos theta, Math.Sin theta
        let px' = cosT * (px) - sinT * (py)
        let py' = sinT * (px) + cosT * (py)
        (Convert.ToInt32 px', Convert.ToInt32 py')

    let turn car dir deg =
        let turnLeft c =
            match c with
            | N -> W
            | E -> N
            | S -> E
            | W -> S

        let turnRight c =
            match c with
            | N -> E
            | E -> S
            | S -> W
            | W -> N

        let changeDir dir c =
            match dir with
            | L -> turnLeft c
            | R -> turnRight c

        changeDir dir
        |> List.replicate (deg / 90)
        |> List.fold (fun c f -> f c) car

    let getDist ((x, y), _) = abs x + abs y

    let partOne instructions =
        let doInst (p, car) inst =
            match (inst.Action, inst.Value) with
            | (Move c, v) -> (move p c v, car)
            | (Turn d, v) -> (p, turn car d v)
            | (Forward, v) -> (move p car v, car)

        instructions
        |> List.fold doInst ((0, 0), E)
        |> getDist

    let partTwo instructions =
        let doInst ((x, y), (wx, wy)) inst =
            match (inst.Action, inst.Value) with
            | (Move c, v) -> ((x, y), move (wx, wy) c v)
            | (Turn d, v) -> ((x, y), rotate (wx, wy) d v)
            | (Forward, v) -> ((x + (v * wx), y + (v * wy)), (wx, wy))

        instructions
        |> List.fold doInst ((0, 0), (10, 1))
        |> getDist

    let instructions = filelines |> List.map parseLine

    toSomeStr2 (partOne instructions, partTwo instructions)
