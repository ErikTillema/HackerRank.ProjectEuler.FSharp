namespace HackerRank.FSharp

module DistinctPowers =

    open Util
    open System
    open System.Collections.Generic

    let sc = Scanner()

    let getDistinctMultiples b = 
        let res = Array.replicate 17 0
        let set = HashSet<int>()
        for i in 1..16 do
            for j in 2..b do
                let pr = i * j
                ignore(set.Add(pr))
            res.[i] <- set.Count
        res

    let log a b = 
        let a = a |> int64
        let b = b |> int64
        let result1 = Math.Floor( Math.Log(b |> float) / Math.Log(a |> float) ) |> int
        let mutable result2 = 0
        let mutable multitude = 1L
        while multitude*a <= b do
            multitude <- multitude*a
            result2 <- result2 + 1
        result2

    let solve() = 
        let n = sc.NextInt().Value
        let skip = Array.replicate (n+1) false
        let mutable result = 0L
        let distinctMultiples = getDistinctMultiples n
        for i in 2..n do
            if not skip.[i] then
                let a = log i n
                result <- result + (distinctMultiples.[a] |> int64)
                let mutable multiple = (i |> int64) * (i |> int64)
                while multiple <= (n |> int64) do
                    skip.[multiple |> int] <- true
                    multiple <- multiple * (i |> int64)
        printfn "%i" result
