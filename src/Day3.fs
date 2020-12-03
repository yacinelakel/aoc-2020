namespace Days

module Day3 =
    open Days.Core

    let solve (fileLines: seq<string>) =

        let rec traverseRec right down (list: string list) pos (hits: int64) =
            if down >= list.Length then
                hits
            else
                let line = list.[down]
                let nPos = (pos + right) % line.Length
                if line.[nPos] = '#'
                then traverseRec right down list.[down..] nPos (hits + 1L)
                else traverseRec right down list.[down..] nPos hits

        let traverse right down list = traverseRec right down list 0 0L

        let partOne input = traverse 3 1 input |> Some

        let partTwo input =
            let slopes = [(1,1);(3,1);(5,1);(7,1);(1,2)]

            slopes 
            |> List.map (fun (a,b) -> traverse a b input) 
            |> List.fold (*) 1L
            |> Some

        let input = parseInput (id) fileLines

        (partOne input, partTwo input)
