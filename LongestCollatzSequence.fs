namespace HackerRank.FSharp

module LongestCollatzSequence =

    open Util
    open System
    open System.Collections.Generic

    let sc = Scanner()
    let mutable cnt = 0
    let mutable maxn = 0L
    let mutable steps = 0
    let cache = Array.replicate 5000001 -1 // cache only first 5000000 values. Use array instead of Dictionary because it uses less memory and is faster.

    let rec getCollatzLength n = 
        steps <- steps + 1
        if n <= 5000000L && cache.[n |> int] <> -1 then cache.[n |> int]
        else
            let res = 
                match n with
                | 1L -> 1
                | n when n%2L=0L -> 1 + getCollatzLength (n/2L)
                | _ -> 1 + getCollatzLength (3L*n+1L)
            if n <= 5000000L then cache.[n |> int] <- res
            cnt <- cnt + 1
            maxn <- max maxn n
            res

    let solve() = 
        let allResults = Array.replicate 5000001 (0,0)
        allResults.[1] <- (1,1)
        for i in 2..5000000 do
            allResults.[i] <- 
                let (n,l1) = allResults.[i-1] 
                let l2 =  getCollatzLength (i |> int64)
                if l2 >= l1 then (i,l2)
                else (n,l1)

        let n = sc.NextInt().Value
        for i in 1..n do
            let n = sc.NextInt().Value
            let result = allResults.[n] |> fst
            printfn "%i" result
            
