module Days.Core

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then Some(s.Substring(p.Length)) else None

let toSomeStr x = Some(string x)

let toSomeStrOption (x: 'a option) =
    match x with
    | Some d -> Some(string d)
    | None -> None

let toSomeStr2 (a, b) = (toSomeStr a, toSomeStr b)
let toSomeStr2Option (a, b) = (toSomeStrOption a, toSomeStrOption b)


let split (seperator:'a) (list: 'a list):('a list list) =
    let folder x state =
        if x = seperator then
            [] :: state
        else
            match state with
            | head :: tail -> (x :: head) :: tail
            | [] -> [ [ x ] ]

    List.foldBack folder list [ [] ]
