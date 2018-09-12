namespace HackerRank.FSharp

module NDigitFibonacciNumber =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()
    let N = 5000
    let b1 = BigInteger.One

    let rec countDigits (n: BigInteger) = 
        Math.Floor(BigInteger.Log10(n)) + 1.0 |> int

    let allResults = 
        let res = Array.replicate (N+1) 0 
        let mutable f1 = b1
        let mutable f2 = b1
        let mutable index = 2
        for i in 2..N do
            while (countDigits f2) < i do
                let fnew = f1 + f2
                f1 <- f2
                f2 <- fnew
                index <- index + 1
            res.[i] <- index
        res

    let solveOne() = 
        let n = sc.NextInt().Value
        let result = allResults.[n]
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
