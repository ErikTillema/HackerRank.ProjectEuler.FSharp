namespace HackerRank.FSharp

module CubicPermutations =

    open System
    open Util

    let sc = Scanner()

    let getSortedDigits (n: int64) =
        n |> string |> Seq.sort |> Seq.toArray |> String

    let solve() = 
        let n = sc.NextInt().Value
        let k = sc.NextInt().Value
        let result = 
            seq { 1..n-1 } |> Seq.map int64 |> Seq.map (fun i -> i*i*i)
                |> Seq.groupBy getSortedDigits
                |> Seq.filter (fun (_,cs) -> Seq.length cs = k) |> Seq.map (snd >> Seq.min)
                |> Seq.sort |> Seq.map string |> String.concat "\n"
        printfn "%s" result
        ()
