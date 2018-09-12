namespace HackerRank.FSharp

module PathSumTwoWays =

    open Util

    let sc = Scanner()

    let solve() = 
        let n = sc.NextInt().Value
        let values = Array2D.init n n (fun _ _ ->  sc.NextLong().Value)
        let minSum = Array2D.create n n System.Int64.MaxValue
        minSum.[0,0] <- values.[0,0]
        for x in 0..n-1 do
            for y in 0..n-1 do
                if x > 0 then minSum.[x,y] <- min minSum.[x,y] (minSum.[x-1,y] + values.[x,y])
                if y > 0 then minSum.[x,y] <- min minSum.[x,y] (minSum.[x,y-1] + values.[x,y])
        let result = minSum.[n-1,n-1]
        printfn "%d" result
        ()
