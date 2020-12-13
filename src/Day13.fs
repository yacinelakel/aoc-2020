module Days.Day13

open System
open Core

let solve (filelines: string list) =  

    let partOne timestamp busList = 
        let earlistTime timestamp busId =
            let rest = timestamp % busId
            (busId, timestamp + (busId - rest))

        let (eBus, eTime) =
            busList
            |> List.map (fun (id, _) -> earlistTime timestamp id)
            |> List.minBy (fun (_, t) -> t)

        (eTime - timestamp) * eBus

    let partTwo busList =    
        // Coincidently, all bus ids are prime numbers, thus we can apply the Chinese remainder theroem
        // First solved using https://www.dcode.fr/chinese-remainder
        // algorithm found here:
        // https://en.wikipedia.org/wiki/Chinese_remainder_theorem#:~:text=In%20number%20theory%2C%20the%20Chinese,the%20divisors%20are%20pairwise%20coprime.
        // https://rosettacode.org/wiki/Chinese_remainder_theorem#F.23
        let crt aList pList =
            // Find the modular multiplicative inverse of a and m
            // https://rosettacode.org/wiki/Modular_inverse#F.23
            let modInv a m =
                let rec fN n i g e l a =
                    match e with
                    | 0UL -> g
                    | _ ->
                        let o = n / e
                        fN e l a (n - o * e) (i - o * l) (g - o * a)

                (a + (fN a 1UL 0UL m 0UL 1UL)) % a

            // Find the greatest common divisor of a and b
            //https://rosettacode.org/wiki/Greatest_common_divisor#F.23
            let rec gcd (a: uint64) (b: uint64) = if b = 0UL then a else gcd b (a % b)

            match List.fold (fun a p -> if (gcd a p) = 1UL then a * p else 0UL) 1UL pList with
            | 0UL -> None
            | fN -> Some((List.fold2 (fun a i p -> a + i * (fN / p) * (modInv p ((fN / p) % p))) 0UL aList pList) % fN)
        

        let (aList, pList) =
            busList
            |> List.map (fun (x, i) -> if (i % x) = 0UL then (0UL, x) else (x - (i % x), x))
            |> List.unzip       

        crt aList pList

    let timestamp = uint64 filelines.[0]

    let busList =
        filelines.[1].Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.mapi (fun i x -> (x, i))
        |> List.filter (fun (s, _) -> s <> "x")
        |> List.map (fun (s, i) -> (uint64 s, uint64 i))

    toSomeStr2Option (partOne timestamp busList |> Some , partTwo busList)
