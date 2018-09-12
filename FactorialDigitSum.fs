namespace HackerRank.FSharp

module FactorialDigitSum =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()
    let b0 = BigInteger.Zero
    let b10 = BigInteger(10)

    let inline sumDigits n = 
        let rec sumDigits' acc (n: BigInteger) = 
            if n = b0 then acc
            else sumDigits' ((n%b10)+acc) (n/b10)
        sumDigits' b0 n

    let solveOne() = 
        let n = sc.NextInt().Value
        let mutable result = BigInteger.One
        for i in 2..n do
            result <- result * (i |> BigInteger)
        let sum = sumDigits result
        printfn "%s" (sum.ToString())

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
