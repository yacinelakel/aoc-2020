module Days.Day14

open Core
open System

type Line =
    | Mask of string
    | Memory of int64 * int64

let solve (filelines: string list) =

    let to36bStr (v: int64) =
        Convert.ToString(v ||| 68719476736L, 2).[1..]

    let parseLine line =
        match line with
        | Regex "^mask = (.+$)" [ mask ] -> Mask mask
        | Regex @"^mem\[(\d+)\]\s=\s(\d+)" [ adr; v ] -> Memory(int64 adr, int64 v)
        | _ -> failwith ("parse error: " + line)

    let run mask lines updateMem =
        let rec _run mask lines mem =
            match lines with
            | x :: xs ->
                match parseLine x with
                | Mask m -> _run m xs mem
                | Memory (a, v) -> updateMem mask (a, v) mem |> _run mask xs
            | [] -> mem

        _run mask lines Map.empty
        |> Map.toList
        |> List.sumBy (fun (_, x) -> x)

    let partOne mask (lines: string list) =
        let applyMask (mask: string) (value: int64): (int64) =
            let folder (a: char) (b: char) (s: string) =
                match a with
                | 'X' -> string b + s
                | _ -> string a + s

            Convert.ToInt64(Seq.foldBack2 folder mask (to36bStr value) "", 2)

        let updateMem mask (a, v) mem = Map.add a (applyMask mask v) mem

        run mask lines updateMem

    let partTwo mask (lines: string list) =
        let rec getAddr acc mask =
            match mask with
            | x :: xs ->
                match x with
                | 'X' -> getAddr (List.append (List.map (fun s -> s + "0") acc) (List.map (fun s -> s + "1") acc)) xs
                | _ -> getAddr (List.map (fun s -> s + string x) acc) xs
            | [] -> List.map (fun x -> Convert.ToInt64(x, 2)) acc

        let applyMask mask adr =
            let maskFolder a b s =
                match a with
                | '0' -> string b + s
                | _ -> string a + s

            Seq.foldBack2 maskFolder mask (to36bStr adr) ""

        let updateMem mask (a, v) mem =
            applyMask mask a
            |> Seq.toList
            |> getAddr [ "" ]
            |> List.fold (fun m addr -> Map.add addr v m) mem

        run mask lines updateMem

    let mask =
        match parseLine filelines.[0] with
        | Mask m -> m
        | _ -> failwith "error"

    let rest = filelines.[1..]

    let p1 = partOne mask rest
    let p2 = partTwo mask rest

    toSomeStr2 (p1, p2)
