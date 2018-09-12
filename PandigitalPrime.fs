namespace HackerRank.FSharp

module PandigitalPrime =

    open Util
    open System

    let sc = Scanner()
    let N = 9999999999L

    let isPrime2 n = 
        let sqrt n = Math.Floor(Math.Sqrt (n |> float)) |> int64
        if n <= 1L then false
        else
            let mutable result = true
            for i in 2L..sqrt n do
                if n % i = 0L then result <- false
            result

    let isPrime n = 
        let isDivisor i = n % i = 0L
        let sqrt n = Math.Floor(Math.Sqrt (n |> float)) |> int64
        if n <= 1L then false
        else
            let hasDivisor = [2L..sqrt n] |> List.exists (fun i -> isDivisor i)
            not hasDivisor

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

    let getNumberBase k (digits: int list) = 
        let rec getNumberBase' acc (digits: int list) = 
            match digits with
            | [] -> acc
            | x::xs -> getNumberBase' ((int64 k) * acc + (int64 x)) xs
        getNumberBase' 0L digits

    /// Returns the number representing the given digist (base 10).
    /// For example, for digits=[1;2] returns 12.
    let getNumber (digits: int list) = getNumberBase 10 digits

    let solve() = 
        let allPandigitalPrimes = 
            seq {
                for l in 1..7 do
                    //printfn "doing %i" l
                    let digits = [1..l]
                    for permutation in getPermutations digits do
                        let number = permutation |> Array.toList |> getNumber
                        if isPrime number then
                            yield number
            } |> Seq.toList |> List.sort

//        for p in allPandigitalPrimes do
//            printfn "%i" p

        let solveOne() = 
            let n = sc.NextLong().Value
            let result = -1L :: allPandigitalPrimes |> List.findBack (fun p -> p <= n)
            printfn "%i" result

        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
