module Days.Day11

open Core

type Cell =
    | Floor
    | Empty
    | Occupied

let solve (filelines: string list) =

    let parseLines lines: (Cell list list) =
        let parseChar c =
            match c with
            | '.' -> Floor
            | 'L' -> Empty
            | '#' -> Occupied
            | _ -> failwith "invalid char"

        let parseLine line = line |> Seq.toList |> List.map parseChar

        lines |> List.map parseLine

    let tryGet grid (i, j) =
        match List.tryItem i grid with
        | Some l -> List.tryItem j l
        | None -> None

    let rec tryGetRec f grid (pos: int * int) =
        let nPos = f pos
        match tryGet grid pos with
        | Some c as cell ->
            match c with
            | Floor -> tryGetRec f grid nPos
            | _ -> cell
        | None -> None

    let getUp (i, j) = (i - 1, j)
    let getDown (i, j) = (i + 1, j)
    let getLeft (i, j) = (i, (j - 1))
    let getRight (i, j) = (i, (j + 1))
    let getTopLeft = getUp >> getLeft
    let getTopRight = getUp >> getRight
    let getBottomLeft = getDown >> getLeft
    let getBottomRight = getDown >> getRight

    let dList =
        [ getUp
          getDown
          getRight
          getLeft
          getTopLeft
          getTopRight
          getBottomLeft
          getBottomRight ]

    let updateGrid f grid =
        List.mapi (fun i v -> List.mapi (fun j _ -> f i j grid) v) grid

    let countOccupied grid =
        let folder =
            List.fold (fun acc c -> if c = Occupied then acc + 1 else acc) 0

        List.sumBy folder grid

    let countOccupiedAdj adj =
        adj
        |> List.filter (fun x -> x = Occupied)
        |> List.length

    let rec simulate f grid =
        let newGrid = updateGrid f grid
        if newGrid = grid then countOccupied grid else simulate f newGrid

    let partOne (grid: Cell list list) =

        let getAdj pos (grid: Cell list list) =
            dList
            |> List.map (fun f -> f pos |> tryGet grid)
            |> List.choose id

        let changeSeat i j (grid: Cell list list) =
            let cell = grid.[i].[j]
            match cell with
            | Empty ->
                let adj = getAdj (i, j) grid
                match countOccupiedAdj adj with
                | 0 -> Occupied
                | _ -> cell
            | Occupied ->
                let adj = getAdj (i, j) grid
                match (countOccupiedAdj adj) >= 4 with
                | true -> Empty
                | false -> cell
            | Floor -> cell

        simulate changeSeat grid

    let partTwo (grid: Cell list list) =

        let getAdj pos (grid: Cell list list) =
            dList
            |> List.map (fun f -> tryGetRec f grid pos)
            |> List.choose id

        let changeSeat i j (grid: Cell list list) =
            let cell = grid.[i].[j]
            match cell with
            | Empty ->
                let adj = getAdj (i, j) grid
                match countOccupiedAdj adj with
                | 0 -> Occupied
                | _ -> cell

            | Occupied ->
                let adj = getAdj (i, j) grid
                match (countOccupiedAdj adj) >= 5 with
                | true -> Empty
                | false -> cell
            | Floor -> cell

        simulate changeSeat grid
        
    let grid = parseLines filelines

    toSomeStr2 (partOne grid, partTwo grid)
