namespace HackerRank.FSharp

module QuadraticPrimes =

    open Util
    open System

    let sc = Scanner()

    let isPrime n = 
        let sqrt n = Math.Floor(Math.Sqrt (n |> float)) |> int
        if n <= 1 then false
        else
            let mutable result = true
            for i in 2..sqrt n do
                if n % i = 0 then result <- false
            result

    let getPrimeCount a p = 
        let mutable i = 0
        while isPrime (i*i + a*i + p) do
            i <- i + 1
        i

    let solve() = 
        let n = sc.NextInt().Value
        let mutable bestPrimeCount = Int32.MinValue
        let mutable besta = Int32.MinValue
        let mutable bestp = Int32.MinValue
        for p in 2..n do
            if isPrime p then
                for a in -n..n do
                    let trial = getPrimeCount a p
                    if trial > bestPrimeCount then
                        bestPrimeCount <- trial
                        besta <- a
                        bestp <- p
        printfn "%i %i" besta bestp

