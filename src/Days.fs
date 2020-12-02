namespace Days
open System.Text.RegularExpressions

module Core = 
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success
        then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None

    let parseInput func input = 
        Seq.map (fun x -> (func x)) input
            |> Seq.toList

