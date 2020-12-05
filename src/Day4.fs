namespace Days

type PassportField =
    | BirthYear of string
    | IssueYear of string
    | ExpirationYear of string
    | Height of string
    | HairColor of string
    | EyeColor of string
    | PassportId of string
    | CountryId of string

module Day4 =
    open System
    open Days.Core

    let solve (filelines: seq<string>) =

        let toChucks lines =
            let chunkFolder chunks line =
                match line with
                | "" -> [] :: chunks
                | l ->
                    match chunks with
                    | x :: xs -> ([ l ] @ x) :: xs
                    | [] -> [ [ l ] ]

            List.fold chunkFolder [ [] ] lines

        let toPassportField line = 
            match line with 
            | Prefix "byr:" rest -> BirthYear rest |> Some
            | Prefix "iyr:" rest -> IssueYear rest |> Some
            | Prefix "eyr:" rest -> ExpirationYear rest |> Some
            | Prefix "hgt:" rest -> Height rest |> Some
            | Prefix "hcl:" rest -> HairColor rest |> Some
            | Prefix "ecl:" rest -> EyeColor rest |> Some
            | Prefix "pid:" rest -> PassportId rest |> Some
            | Prefix "cid:" rest -> CountryId rest |> Some
            | _ -> None 

        let numStrBetween (str:string) fromY toY = 
            match Int32.TryParse str with | (true, num) -> num >= fromY && num <= toY | _ -> false  

        let byrValid s = 
            match s with 
            | Regex @"^(\d{4})$" [yearS] -> numStrBetween yearS 1920 2002 
            | _ -> false

        let iyrValid s =
            match s with 
            | Regex @"^(\d{4})$" [yearS] -> numStrBetween yearS 2010 2020  
            | _ -> false

        let eyrValid s = 
            match s with 
            | Regex @"^(\d{4})$" [yearS] -> numStrBetween yearS 2020 2030  
            | _ -> false

        let hgtValid s = 
            match s with 
            | Regex @"^(\d+)(in|cm)$" [height; measurement] ->
                match measurement with
                |"cm" -> numStrBetween height 150 193
                |"in" -> numStrBetween height 59 76
                | _ -> false
            | _ -> false

        let hclValid s = 
            match s with 
            | Regex @"^#([0-9]|[a-f]){6}$" [_] -> true
            | _ -> false
        
        let eclValid s = 
            match s with 
            | Regex @"^(amb|blu|brn|gry|grn|hzl|oth|)$" [_] -> true
            | _ -> false

        
        let pidValid s = 
            match s with 
            | Regex @"^(\d{9})$" [_] -> true
            | _ -> false

              
        let isFieldValid field = 
            match field with 
            | BirthYear s -> byrValid s
            | IssueYear s -> iyrValid s
            | ExpirationYear s -> eyrValid s
            | Height s -> hgtValid s
            | HairColor s -> hclValid s
            | EyeColor s -> eclValid s
            | PassportId s -> pidValid s
            | CountryId _ -> true

        let getFields (line:string) = 
            line.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
            |> List.map toPassportField 
            |> List.choose id

        let filterByHasAllFields chunks =
            let hasAllFields chunk =
                let combineFields set line = 
                    line 
                    |> getFields
                    |> Set.ofList
                    |> Set.union set

                let set = List.fold combineFields Set.empty chunk

                let isCid f = match f with | CountryId _ -> true | _ -> false

                match set.Count with
                | 8 -> true
                | 7 -> Set.exists isCid set |> not
                | _ -> false

            chunks |> List.filter hasAllFields

        let filterByAllValid chunks =
            let isValidChunk chunk =
                let isValidLine line =
                    let ans = 
                        line 
                        |> getFields
                        |> List.filter (isFieldValid >> not)
                    ans.Length = 0
                    

                let invalidLines = List.filter (isValidLine >> not) chunk
                invalidLines.Length = 0

            chunks |> List.filter isValidChunk


        let chunks = filelines |> Seq.toList |> toChucks

        let allRequired = chunks |> filterByHasAllFields
        let allRequiredAndValid = allRequired |> filterByAllValid

        let p1 = allRequired |> List.length |> Some
        let p2 = allRequiredAndValid |> List.length |> Some

        (p1, p2)
