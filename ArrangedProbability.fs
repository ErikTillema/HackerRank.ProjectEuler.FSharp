namespace HackerRank.FSharp

module ArrangedProbability =

    open Util

    let sc = Scanner()

    // b+r discs, => P(BB) = b/(b+r) * (b-1)/(b+r-1) = b*(b-1)/((b+r)*(b+r-1))
    // P(BB) = p/q  => b*(b-1)*q = p*(b+r)*(b+r-1)
    // 

    let solveOne() =
        let p = sc.NextInt().Value
        let q = sc.NextInt().Value
        let d = sc.NextLong().Value
        let result = 42
        printfn "%d" result
        ()

    let solve() =
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()
        ()