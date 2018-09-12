namespace HackerRank.FSharp

module LargeSum =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()

    let solve() = 
        let n = sc.NextInt().Value
        let mutable result = BigInteger.Zero
        for _ in 0..n-1 do
            let add = BigInteger.Parse(sc.Next().Value)
            result <- result + add
        printfn "%s" (result.ToString().Substring(0,10))
