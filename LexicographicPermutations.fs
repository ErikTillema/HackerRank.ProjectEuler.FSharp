namespace HackerRank.FSharp

module LexicographicPermutations =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()
    let l = 13

    let rec factorial n =
        match n with 
        | 0 -> 1
        | 1 -> 1
        | _ -> n * factorial (n-1)

    let solveOne() = 
        let mutable n = sc.NextInt().Value - 1
        let used = Array.replicate l false
        let letter = Array.replicate l 0
        for i in 0..l-1 do
            let f = factorial (l-1-i)
            let a = n/f
            let newLetter = used |> Seq.mapi (fun i u -> (i,u)) |> Seq.filter (snd >> not) |> Seq.skip a |> Seq.head |> fst
            used.[newLetter] <- true
            letter.[i] <- newLetter
            n <- n%f
        let result = letter |> Array.map (fun l -> (('a' |> int) + l) |> char) |> String
        printfn "%s" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
