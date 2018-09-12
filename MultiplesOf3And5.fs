namespace HackerRank.FSharp

module MultiplesOf3And5 =

    open Util
    open System

    let sc = Scanner()

    let solveOne() = 
        let n = sc.NextLong().Value
        let n = n - 1L
        let n3 = n/3L
        let n5 = n/5L
        let n15 = n/15L
        let result = 3L * n3*(n3+1L)/2L + 5L * n5*(n5+1L)/2L - 15L * n15*(n15+1L)/2L;
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()