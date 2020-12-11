module Days.Day11
open Core

type Cell = | Floor | Empty | Occupied 

let solve (filelines: string list) = 

    let parseLines lines: (Cell list list) = 
        let parseChar c =
            match c with 
            | '.' -> Floor
            | 'L' -> Empty
            | '#' -> Occupied
            | _ -> failwith "invalid char"

        let parseLine line = line |> Seq.toList |> List.map parseChar

        lines |>List.map parseLine 


    let getUp i j (grid:Cell list list) = if (i = 0) then None else Some grid.[i - 1].[j]
    let getDown i j (grid:Cell list list) = if (i = grid.Length - 1) then None else Some grid.[i + 1].[j]
    let getLeft i j (grid:Cell list list) = if (j = 0) then None else Some grid.[i].[j - 1]
    let getRight i j (grid:Cell list list) = if (j = grid.[i].Length - 1) then None else Some grid.[i].[j + 1]
    let getTopLeft i j (grid:Cell list list) = if (i = 0 || j = 0 ) then None else Some grid.[i - 1].[j - 1] 
    let getTopRight i j (grid:Cell list list) = if (i = 0 || j = grid.[i].Length - 1 ) then None else Some grid.[i - 1].[j + 1] 
    let getBottomLeft i j (grid:Cell list list) = if (i = grid.Length - 1 || j = 0 ) then None else Some grid.[i + 1].[j - 1] 
    let getBottomRight i j (grid:Cell list list) = if (i = grid.Length - 1 || j = grid.[i].Length - 1 ) then None else Some grid.[i + 1].[j + 1] 
    
    let updateGrid (f:int -> int -> Cell list list -> Cell) (grid:Cell list list) =
            grid |> List.mapi (fun i v -> List.mapi (fun j _ -> f i j grid ) v)

    let countOccupied grid = 
            let folder = List.fold (fun acc c -> if c = Occupied then acc + 1 else acc) 0 
            List.sumBy folder grid

    let grid = parseLines filelines 

    let partOne (grid:Cell list list) =
        
        let getAdj i j (grid:Cell list list) = 
            let dList = [getUp;getDown;getRight;getLeft;getTopLeft;getTopRight;getBottomLeft;getBottomRight];
            dList |> List.map (fun f -> f i j grid) |> List.choose id

        let changeSeat i j (grid:Cell list list) = 
            let cell = grid.[i].[j]
            match cell with
            | Empty -> 
                let hasNoOccupied = getAdj i j grid |> List.contains Occupied |> not
                if hasNoOccupied then Occupied else cell
            | Occupied -> 
                let numOccupied = getAdj i j grid |> List.filter (fun x -> x = Occupied) |> List.length
                if numOccupied >= 4 then Empty else cell
            | Floor -> cell
        
        let rec shuffle grid = 
            let newGrid = updateGrid changeSeat grid
            if newGrid = grid then countOccupied grid
            else shuffle newGrid 

        shuffle grid

    let partTwo (grid:Cell list list) = 
        let rec getUpRec i j grid = 
            match getUp i j grid with 
            | Some c -> match c with | Floor -> getUpRec (i - 1) j grid | _ -> Some c
            | None -> None
        
        let rec getDownRec i j grid = 
            match getDown i j grid with 
            | Some c -> match c with | Floor -> getDownRec (i + 1) j grid | _ -> Some c
            | None -> None

        let rec getLeftRec i j grid = 
            match getLeft i j grid with 
            | Some c -> match c with | Floor -> getLeftRec i (j - 1) grid | _ -> Some c
            | None -> None

        let rec getRightRec i j grid = 
            match getRight i j grid with 
            | Some c -> match c with | Floor -> getRightRec i (j + 1) grid | _ -> Some c
            | None -> None
        
        let rec getTopLeftRec i j grid = 
            match getTopLeft i j grid with 
            | Some c -> match c with | Floor -> getTopLeftRec (i - 1) (j - 1) grid | _ -> Some c
            | None -> None
        
        let rec getTopRightRec i j grid = 
            match getTopRight i j grid with 
            | Some c -> match c with | Floor -> getTopRightRec (i - 1) (j + 1) grid | _ -> Some c
            | None -> None
        
        let rec getBottomLeftRec i j grid = 
            match getBottomLeft i j grid with 
            | Some c -> match c with | Floor -> getBottomLeftRec (i + 1) (j - 1) grid | _ -> Some c
            | None -> None

        let rec getBottomRightRec i j grid = 
            match getBottomRight i j grid with 
            | Some c -> match c with | Floor -> getBottomRightRec (i + 1) (j + 1) grid | _ -> Some c
            | None -> None

        let getAdj i j (grid:Cell list list) = 
            let dList = [getUpRec;getDownRec;getRightRec;getLeftRec;getTopLeftRec;getTopRightRec;getBottomLeftRec;getBottomRightRec];
            dList |> List.map (fun f -> f i j grid) |> List.choose id

        let changeSeat i j (grid:Cell list list) = 
            let cell = grid.[i].[j]
            match cell with
            | Empty -> 
                let hasNoOccupied = getAdj i j grid |> List.contains Occupied |> not
                if hasNoOccupied then Occupied else cell
            | Occupied -> 
                let numOccupied = getAdj i j grid |> List.filter (fun x -> x = Occupied) |> List.length
                if numOccupied >= 5 then Empty else cell
            | Floor -> cell

        let rec shuffle grid = 
            let newGrid = updateGrid changeSeat grid
            if newGrid = grid then countOccupied grid
            else shuffle newGrid 

        shuffle grid

    toSomeStr2 (partOne grid, partTwo grid)