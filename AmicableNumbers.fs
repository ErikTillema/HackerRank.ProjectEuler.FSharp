namespace HackerRank.FSharp

module AmicableNumbers =

    open Util
    open System

    let solve() = 
        let sc = Scanner()

        let getDivisors n = 
            let sqrt n = Math.Floor(Math.Sqrt (n |> float)) |> int
            seq {
                for i in 1..sqrt n do
                    if n%i=0 then
                        yield i
                        if i <> (n/i) then yield (n/i)
            } |> Seq.sort |> Seq.toList

        let getDivisorSum n = (getDivisors n |> List.sum) - n

        let divisorSums = Array.init 100001 (fun i -> getDivisorSum i)

        let isAmicable = Array.init 100001 (fun i ->    let s = divisorSums.[i]
                                                        let ss = if s <= 100000 then divisorSums.[s] else getDivisorSum s
                                                        ss = i && i <> s)

        let allResults = 
            let res = Array.replicate 100001 0
            res.[0] <- 0
            for i in 1..100000 do
                res.[i] <- res.[i-1] + if isAmicable.[i] then i else 0
            res

        let solveOne() = 
            let n = sc.NextInt().Value
            let result = allResults.[n]
            printfn "%i" result

        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
