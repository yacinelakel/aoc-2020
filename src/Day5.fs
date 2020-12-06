module Days.Day5

open Core

type Dir =
    | Left
    | Right

let solve filelines =

    let rec binSearch (mFunc: 'a -> Dir) (low: int) (high: int) (list: 'a list) =
        match list with
        | [ a ] ->
            match mFunc (a) with
            | Left -> low
            | Right -> high
        | a :: xs ->
            match mFunc (a) with
            | Left ->
                let mid = low + (high - low) / 2
                binSearch mFunc low mid xs
            | Right ->
                let mid = high - ((high - low) / 2)
                binSearch mFunc mid high xs
        | [] -> low

    let getRow (cList: char list) =
        binSearch (fun c -> if 'F' = c then Left else Right) 0 127 cList

    let getCol (cList: char List) =
        binSearch (fun c -> if 'L' = c then Left else Right) 0 7 cList

    let getSeatId (str: string) =
        let row = str.[0..6] |> Seq.toList |> getRow
        let col = str.[0..7] |> Seq.toList |> getCol
        (row * 8) + col

    let getFirstMissing list =
        let rec findFirst list =
            match list with
            | x :: y :: rest -> if y - x <> 1 then y - 1 else findFirst (y :: rest)
            | _ -> 0

        list |> List.sort |> findFirst

    let seatIds = filelines |> List.map getSeatId

    let p1 = seatIds |> List.max

    let p2 = seatIds |> getFirstMissing

    toSomeStr2 (p1, p2)
