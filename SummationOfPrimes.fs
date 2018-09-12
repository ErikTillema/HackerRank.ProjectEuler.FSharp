namespace HackerRank.FSharp

module SummationOfPrimes =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()

    // zeef alle priemgetallen bij elkaar tot en met n
    let primes =
        let n = 1000000
        let notprime = Array.replicate (n+1) false
        notprime.[0] <- true
        notprime.[1] <- true
        for i in 2..Math.Floor(Math.Sqrt(n |> float)) |> int do
            if not notprime.[i] then
                for j in i*i..i..n do
                    notprime.[j] <- true

        seq {
            for i in 2..n do
                if not notprime.[i] then
                    yield i
        } |> Seq.toArray

    let primeSums = 
        let n = primes.Length
        let result = Array.replicate n 0L
        result.[0] <- primes.[0] |> int64
        for i in 1..n-1 do
            result.[i] <- result.[i-1] + (primes.[i] |> int64)
        result

    let rec binarysearch target (arr: int[]) start endd =
        if start >= endd then 
            start
        else
            let mid = (endd+start+1)/2
            if target < arr.[mid] then 
                binarysearch target arr start (mid-1)
            else 
                binarysearch target arr mid endd

    let solveOne() = 
        let n = sc.NextInt().Value
        let i = binarysearch n primes -1 (primes.Length-1)
        let result = primeSums.[i]
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
