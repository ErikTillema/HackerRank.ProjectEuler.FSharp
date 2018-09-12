namespace HackerRank.FSharp

module CoinPartitions =

    open Util

    let sc = Scanner()

    let m = 1_000_000_007

    /// Number of possibilities to write 5 as a sum is
    /// 5
    /// 4+1
    /// 3+2
    /// 3+1+1
    /// 2+2+1
    /// 2+1+1+1
    /// 1+1+1+1+1
    /// Suppose poss.[n].[max] = number of possibilities to write n as a sum of numbers each max max
    /// Let's look at poss.[n].[max] . Each possibility either starts with max or it doesn't.
    /// So poss.[n].[max] = poss.[n].[max-1] + poss.[n-max].[max] (if n >= max)
    /// poss.[0].[i] = 1 for all i, poss.[i].[0] = 0 for i > 0
    /// time O(n^2), space O(n^2)
    let getSolution_too_slow_and_out_of_memory n = 
        let (+?) a b = (a+b)%m
        let poss = Array2D.create (n+1) (n+1) 0
        for i in 0..n do poss.[0,i] <- 1
        for i in 1..n do
            for max in 1..n do
                poss.[i,max] <- poss.[i,max-1] +? if i >= max then poss.[i-max,max] else 0
        poss.[n,n]

    /// We can reduce space complexity by smartly iterating
    /// If we iterate over max in the outer loop
    /// and store only poss.[n] (so poss.[n] = poss.[n].[last max])
    /// then we can still perform poss.[n].[max] = poss.[n].[max-1] + poss.[n-max].[max]
    /// Because poss.[n].[max-1] is the value that is about to be overwritten and poss.[n-max].[max] are stored.
    /// time O(n^2), space O(n)
    let getSolution_too_slow n = 
        let (+?) a b = (a+b)%m
        let poss = Array.create (n+1) 0
        poss.[0] <- 1
        for max in 1..n do
            for i in 1..n do
                poss.[i] <- poss.[i] +? if i >= max then poss.[i-max] else 0
        poss.[n]

    /// Use https://en.wikipedia.org/wiki/Pentagonal_number_theorem
    /// p(n) = SUM_k (-1)^(k-1) p(n - g_k)
    /// Where g_k = k(3k-1)/2  (The pentagonal numbers)
    /// And p(0) = 1 and p(k) = 0 when k < 0.
    /// The sum is until g_k > n and g_k = O(k^2), so the sum is over O(sqrt(n)) terms.
    /// Since we calculate all N values, we get that we calculate all solutions in
    /// O(N sqrt(N))
    let getSolution = 
        let getAllSolutions() = 
            let N = 60_000
            let result = Array.create (N+1) 0
            result.[0] <- 1

            let modulo m a = ((a%m)+m)%m
            let (+?) a b = modulo m (a+b)
            let g k = k*(3*k-1)/2
            let term n k = (if modulo 2 (k-1) = 0 then 1 else -1) * result.[n - g k]
            for i in 1..N do
                result.[i] <-  Seq.initInfinite (fun k -> k+1) 
                                    |> Seq.collect (fun k -> [k;-k]) 
                                    |> Seq.takeWhile (fun k -> i - g k >= 0) 
                                    |> Seq.fold (fun sum k -> sum +? term i k) 0
            result

        let allSoltutions = getAllSolutions()
        (fun n -> allSoltutions.[n])

    let solveOne() = 
        let n = sc.NextInt().Value
        let result = getSolution n
        printfn "%d" result
        ()

    let solve() = 
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()
        ()

