namespace HackerRank.FSharp

module TotientMaximum =

    open Util
    open System.Numerics

    let sc = Scanner()

    /// Returns all primes <= n in order.
    /// Uses Sieve of Eratosthenes.
    /// O(n log log n)
    /// Because SUM_(p <= sqrt n) { n/p } = O(n log log n)
    let primesUntil (n: int) =
        let sqrt n = floor(sqrt (float n)) |> int
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
    
    let eulerTotientMax =
        let primes = primesUntil 1000 |> Array.map bigint
        (fun (n: int64) ->
            let primesMultiplied = Seq.unfold (fun (value,i) -> Some(value, (value*primes.[i], i+1)) ) (1I,0)
            primesMultiplied |> Seq.takeWhile (fun value -> value < (bigint n)) |> Seq.last |> int64
        )

    let solveOne() = 
        let n = sc.NextLong().Value
        let result = eulerTotientMax n
        printfn "%d" result
        ()

    let solve() = 
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()
