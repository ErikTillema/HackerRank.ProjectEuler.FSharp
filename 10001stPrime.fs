namespace HackerRank.FSharp

module The10001stPrime =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()

    // zeef alle priemgetallen bij elkaar tot en met n
    let primes =
        let n = 120000
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

    let solveOne() = 
        let n = sc.NextInt().Value - 1
        let result = primes.[n]
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
