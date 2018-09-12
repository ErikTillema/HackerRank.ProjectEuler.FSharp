namespace HackerRank.FSharp

module NamesScores =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()

    let score (name: String) = 
        name |> Seq.map (fun c -> (c |> int) - ('A' |> int) + 1) |> Seq.sum

    let solve() = 
        let names = sc.NextInt().Value
        let name = seq { for _ in 0..names-1 -> sc.Next().Value } |> Seq.sort |> Seq.mapi (fun i v -> (v,i)) |> Map.ofSeq
        let n = sc.NextInt().Value
        for i in 1..n do
            let q = sc.Next().Value
            let score1 = score q
            let index = name.[q] + 1
            let result = index * score1
            printfn "%i" result
