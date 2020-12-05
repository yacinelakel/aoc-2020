namespace Days


module Core = 
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success
        then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let (|RegexMacthes|_|) pattern input =
        let matches = Regex.Matches(input, pattern)
        if matches.Count > 0 then
            List.tail [ for m in matches do for g in m.Groups -> g.Value ] |> Some
        else None


    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None

    let parseInput func input = 
        Seq.map (fun x -> (func x)) input
            |> Seq.toList

    let test = 
         match "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd" with
         | RegexMacthes @"byr:|iyr:|ecl:|pid:|eyr:|hcl:|hgt:|cid:" r -> 
            printfn "%A" r
         | _ -> 
            printfn "Nothing"


   