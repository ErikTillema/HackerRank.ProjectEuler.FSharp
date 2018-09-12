namespace HackerRank.FSharp

module PrimeSummations =

    open Util

    let sc = Scanner()

    /// Returns all primes <= n in order.
    /// Uses Sieve of Eratosthenes.
    /// O(n log log n)
    /// Because SUM_(p <= sqrt n) { n/p } = O(n log log n)
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

    let primes = primesUntil 1000

    /// Number of possibilities to write 10 as a sum of primes is
    /// 7+3
    /// 5+5
    /// 5+3+2
    /// 3+3+2+2
    /// 2+2+2+2+2
    /// Suppose poss.[n].[maxPrimeIndex] = number of possibilities to write n as a sum of primes each max maxPrime (the prime at maxPrimeIndex)
    /// Let's look at poss.[n].[maxPrimeIndex] . Each possibility either starts with maxPrime or it doesn't.
    /// So poss.[n].[maxPrimeIndex] = poss.[n].[maxPrimeIndex-1]           (if maxPrimeIndex > 0)
    ///                               + poss.[n-maxPrime].[maxPrimeIndex]  (if n >= maxPrime)
    /// poss.[0].[i] = 1 for all i
    let getSolution n = 
        let primeCount = primes |> Seq.takeWhile (fun p -> p <= n) |> Seq.length
        let poss = Array2D.create (n+1) primeCount 0L
        for i in 0..primeCount-1 do poss.[0,i] <- 1L
        for i in 1..n do
            for maxPrimeIndex in 0..primeCount-1 do
                let maxPrime = primes.[maxPrimeIndex]
                poss.[i,maxPrimeIndex] <- if maxPrimeIndex > 0 then poss.[i,maxPrimeIndex-1] else 0L
                                            + if i >= maxPrime then poss.[i-maxPrime,maxPrimeIndex] else 0L
        poss.[n,primeCount-1]

    let solveOne() = 
        let n = sc.NextInt().Value
        let result = getSolution n
        printfn "%d" result
        ()

    let solve() = 
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()
        ()

