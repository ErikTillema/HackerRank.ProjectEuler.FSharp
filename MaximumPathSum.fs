namespace HackerRank.FSharp

module MaximumPathSum =

    open Util
    open System

    let sc = Scanner()
    
    let solveOne() = 
        let n = sc.NextInt().Value
        let arr = Array.init n (fun i -> Array.init (i+1) (fun _ -> sc.NextInt().Value) )
        let sum = Array.init n (fun i -> Array.init (i+1) (fun _ -> 0) )
        sum.[0].[0] <- arr.[0].[0]
        for y in 1..n-1 do
            for x in 0..y do
                if x = 0 then 
                    sum.[y].[x] <- sum.[y-1].[x] + arr.[y].[x]
                elif x = y then 
                    sum.[y].[x] <- sum.[y-1].[x-1] + arr.[y].[x]
                else 
                    sum.[y].[x] <- (max sum.[y-1].[x] sum.[y-1].[x-1]) + arr.[y].[x]
        let result = sum.[n-1] |> Array.max
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
