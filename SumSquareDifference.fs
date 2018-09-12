namespace HackerRank.FSharp

module SumSquareDifference =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()
    let b1 = BigInteger.One
    let b2 = BigInteger(2)
    let b3 = BigInteger(3)
    let b6 = BigInteger(6)

    let solveOne() = 
        let n = sc.NextBigInteger().Value
        let a = (n*(n+b1)/b2)*(n*(n+b1)/b2)
        let b = (b2*(n+b1)*(n+b1)*(n+b1) - b3*(n+b1)*(n+b1) + n+b1)/b6
        let result = a - b
        printfn "%s" (result.ToString())

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
