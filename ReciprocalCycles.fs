namespace HackerRank.FSharp

module ReciprocalCycles =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()
    let N = 10000

    let getCycleLength d = 
        let pos = Array.replicate d -1
        let mutable found = false
        let mutable result = 0
        let mutable n = 1
        let mutable curpos = 0
        while not found do
            n <- n * 10
            let remainder = n % d
            if remainder = 0 then
                found <- true
            else
                if pos.[remainder] <> -1 then
                    found <- true
                    result <- curpos - pos.[remainder]
                else
                    pos.[remainder] <- curpos
                curpos <- curpos + 1
                n <- remainder
        result

    let allCycleLengths = Array.init (N+1) (fun i -> if i < 2 then 0 else getCycleLength i)

    let allResults = 
        let res = Array.replicate (N+1) (0,0)
        res.[3] <- (3,1)
        for i in 4..N do
            let (n,l) = res.[i-1]
            let l2 = allCycleLengths.[i]
            if l2 > l then
                res.[i] <- (i, l2)
            else
                res.[i] <- res.[i-1]
        res

    let solveOne() = 
        let n = sc.NextInt().Value - 1
        let result = allResults.[n] |> fst
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
