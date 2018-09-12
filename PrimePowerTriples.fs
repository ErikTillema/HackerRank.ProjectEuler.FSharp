namespace HackerRank.FSharp

module PrimePowerTriples =

    open Util

    let sc = Scanner()

    let primesUntil (n: int) =
        let sqrt n = n |> float |> sqrt |> floor |> int
        let notprime = Array.replicate (n+1) false
        notprime.[0] <- true
        notprime.[1] <- true
        for i in 2..sqrt n do
            if not notprime.[i] then
                for j in i*i..i..n do
                    notprime.[j] <- true

        seq {
            for i in 2..n do
                if not notprime.[i] then
                    yield i
        } |> Seq.toArray

    let primes = primesUntil 4000

    let getSolution = 
        // NB! If we don't put the array creation inside this function, then hackerrank will give a TLE (!)
        let getAllSolutions() = 
            let N = 10_000_000
            let isCool = Array.create (N+1) false
            for p2 in primes |> Array.takeWhile (fun p -> p*p <= N) do
                let left = N - p2*p2
                for p3 in primes |> Array.takeWhile (fun p -> p*p*p <= left) do
                    let left = N - p2*p2 - p3*p3*p3
                    for p4 in primes |> Array.takeWhile (fun p -> p*p*p*p <= left) do
                        let n = p2*p2 + p3*p3*p3 + p4*p4*p4*p4
                        isCool.[n] <- true
        
            let acc = Array.create (N+1) 0
            for i in 1..N do acc.[i] <- acc.[i-1] + if isCool.[i] then 1 else 0
            acc

        let acc = getAllSolutions()
        (fun n -> acc.[n])

    let solveOne() = 
        let n = sc.NextInt().Value
        let result = getSolution n
        printfn "%d" result
        ()

    let solve() = 
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()
        ()