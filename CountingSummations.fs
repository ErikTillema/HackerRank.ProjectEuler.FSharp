namespace HackerRank.FSharp

module CountingSummations =

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
    /// poss.[0].[0] = 0, poss.[0].[i] = 1 for i > 0, poss.[i].[0] = 0 for i > 0
    let getSolution n = 
        let (+?) a b = (a+b)%m
        let poss = Array2D.create (n+1) (n+1) 0
        for i in 1..n do poss.[0,i] <- 1
        for i in 1..n do
            for max in 1..n do
                poss.[i,max] <- poss.[i,max-1] +? if i >= max then poss.[i-max,max] else 0
        poss.[n,n] - 1

    let solveOne() = 
        let n = sc.NextInt().Value
        let result = getSolution n
        printfn "%d" result
        ()

    let solve() = 
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()
        ()

