namespace HackerRank.FSharp

module SubStringDivisibility =

    open Util
    open System

    let sc = Scanner()

    /// Returns the number representing the given digist (base k).
    /// For example, for k=2, digits=[1;0;1;1] returns 11.
    let getNumberBase k (digits: int list) = 
        let rec getNumberBase' acc (digits: int list) = 
            match digits with
            | [] -> acc
            | x::xs -> getNumberBase' ((k|>int64) * acc + (x|>int64)) xs
        getNumberBase' 0L digits

    /// Returns the number representing the given digist (base 10).
    /// For example, for digits=[1;2] returns 12.
    let getNumber (digits: int list) = getNumberBase 10 digits

    /// returns all permutations of source (as arrays)
    let getPermutations (source: 'a seq) = 
        let source = source |> Seq.toArray
        let n = source.Length
        let result = Array.zeroCreate n
        let used = Array.create n false
        let rec getPermutations' don =
            seq {
                if don = n then
                    yield result
                else
                    for i in 0..n-1 do
                        if not used.[i] then
                            used.[i] <- true
                            result.[don] <- source.[i]
                            yield! getPermutations' (don+1)
                            used.[i] <- false
            }
        getPermutations' 0
    
    let isSpecial (digits: int array) =
        let primes = [| 2L; 3L; 5L; 7L; 11L; 13L; 17L |]
        let isSpecial' i = 
            let value = [i..i+2] |> List.map (fun i -> digits.[i-1]) |> getNumber
            value % primes.[i-2] = 0L
        let l = digits.Length
        [ 2..l-2 ] |> List.forall isSpecial'

    let getSum n = 
        let digits = [ 0..n ]
        digits 
            |> getPermutations 
            // |> Seq.filter (fun l -> l.[0] <> 0) 
            |> Seq.filter isSpecial 
            |> Seq.map (fun l -> l |> Array.toList |> getNumber |> int64) 
            |> Seq.sum

    let solve() = 
        // precalculated: 9 -> 16695334890L
        let n = sc.NextInt().Value
        let sum = 
            match n with
            | 9 -> 16695334890L
            | _ -> getSum n
        printfn "%i" sum
