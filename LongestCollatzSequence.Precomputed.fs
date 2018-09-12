namespace HackerRank.FSharp

module LongestCollatzSequence_Precomputed =

    open Util
    open System
    open System.Collections.Generic

    let sc = Scanner()
    let mutable cnt = 0
    let mutable maxn = 0L
    let mutable steps = 0
    let cache = Dictionary<int64,int>()

    let rec getCollatzLength n = 
        steps <- steps + 1
        if cache.ContainsKey(n) then cache.[n]
        else
            let res = 
                match n with
                | 1L -> 1
                | n when n%2L=0L -> 1 + getCollatzLength (n/2L)
                | _ -> 1 + getCollatzLength (3L*n+1L)
            cache.Add(n,res)
            cnt <- cnt + 1
            maxn <- max maxn n
            res

    let precompute() = 
        let allResults = Array.replicate 5000001 (0,0)
        allResults.[1] <- (1,1)
        for i in 2..5000000 do
            allResults.[i] <- 
                let (n,l1) = allResults.[i-1] 
                let l2 =  getCollatzLength (i |> int64)
                if l2 >= l1 then (i,l2)
                else (n,l1)

        let mutable last = -1
        for i in 1..5000000 do
            let (n,l) = allResults.[i]
            if n <> last  then
                printfn "%i" n // print and save answers in precomputed array below
                last <- n

    let answers = [| 1; 2; 3; 6; 7; 9; 18; 19; 25; 27; 54; 55; 73; 97; 129; 171; 231; 235; 313; 327; 649; 654; 655; 667; 703; 871; 1161; 2223; 2322; 2323; 2463; 2919; 3711; 6171; 10971; 13255; 17647; 17673; 23529; 26623; 34239; 35497; 35655; 52527; 77031; 106239; 142587; 156159; 216367; 230631; 410011; 511935; 626331; 837799; 1117065; 1126015; 1501353; 1564063; 1723519; 2298025; 3064033; 3542887; 3732423 |]

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            let n = sc.NextInt().Value
            let result = answers |> Array.findBack (fun i -> i <= n)
            printfn "%i" result
            
