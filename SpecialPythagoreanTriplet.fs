namespace HackerRank.FSharp

module SpecialPythagoreanTriplet =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()

    let solveOne() = 
        let n = sc.NextInt().Value
        let mutable result = -1
        let updateResult a b c = 
            if a < b && b < c then
                result <- max result (a*b*c)
        for a in 1..n/3 do
            if (n*(n-2*a)) % (2*(n-a)) = 0 then
                let b = (n*(n-2*a)) / (2*(n-a))
                let c = n - a - b
                updateResult a b c
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
