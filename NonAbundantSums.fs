namespace HackerRank.FSharp

module NonAbundantSums =

    open Util
    open System

    let sc = Scanner()
    let N = 100000

    let getDivisors n =
        let sqrt n = Math.Floor(Math.Sqrt (n |> float)) |> int
        seq {
            for i in 1..sqrt n do
                if n%i=0 then
                    yield i
                    if i <> (n/i) then yield (n/i)
        } |> Seq.sort |> Seq.toList

    let getIsAbudant n = (getDivisors n |> List.sum) - n > n

    let abundant = 
        seq { 
            for i in 1..N+1 do 
                if getIsAbudant i then yield i 
        } |> Set.ofSeq

    let solveOne() = 
        let n = sc.NextInt().Value
        let result = abundant |> Seq.exists (fun a -> abundant.Contains(n-a))
        printfn "%s" (if result then "YES" else "NO")

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
