namespace HackerRank.FSharp

module GoldbachsOtherConjecture =

    open Util
    open System

    let sc = Scanner()

    let isPrime n = 
        let isDivisor i = n % i = 0
        let sqrt n = Math.Floor(Math.Sqrt (float n)) |> int
        if n <= 1 then false
        else
            let hasDivisor = [2..sqrt n] |> List.exists (fun i -> isDivisor i) // List.exists is lazy
            not hasDivisor

    let solveOne() = 
        let N = sc.NextInt().Value
        let squares = Seq.initInfinite (fun i -> 2*(i+1)*(i+1))
        let result = squares |> Seq.takeWhile (fun a -> a <= N-1) |> Seq.filter (fun a -> isPrime (N-a)) |> Seq.length
        printfn "%d" result

    let solve() = 
        let t = sc.NextInt().Value
        for _ in 1..t do 
            solveOne()
        ()