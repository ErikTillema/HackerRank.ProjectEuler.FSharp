namespace HackerRank.FSharp

module PathSumThreeWays =

    open Util
    open System.Collections.Generic

    let sc = Scanner()
    
    let transpose source =
        Array2D.init (source |> Array2D.length2) (source |> Array2D.length1) (fun x y -> source.[y,x])

    // Tried with Dijkstra but gives time-outs.
    // Dijkstra is O((V+E)log V), so for this problem O((n^2 + 3n^2) log n^2) = O(8n^2 log n)
    // We can optimize by using DP and moving to right one column at a time.
    // We still need some sort of SortedSet with least distance paths so far, but that gets easier now
    // I think it can be O(n(n log n + 3n)) = O(3n^2 log n) which is more than two times faster.
    // fill distance array column by column. 
    // keep a SortedSet with shortest distances, but only those relevant to the column we're filling (so max 1000 items).
    // Traverse sortedset and update rows.
    // Unfortunately, this is still too slow :-(

    // But it might be that O(n^2) is also possible? Yes, by doing 2 sweeps of "going down" and "going up"

    let solve() = 
        let n = sc.NextInt().Value
        let values = Array2D.init n n (fun _ _ -> sc.NextLong().Value) |> transpose

        let result = Array2D.create n n System.Int64.MaxValue
        for y in 0..n-1 do 
            result.[0,y] <- values.[0,y]
        
        for x in 1..n-1 do
            // sweep down
            for y in 0..n-1 do
                let trial = values.[x,y] + min result.[x-1,y] (if y > 0 then result.[x,y-1] else System.Int64.MaxValue)
                result.[x,y] <- min result.[x,y] trial
            // sweep up
            for y in n-1 .. -1 .. 0 do
                let trial = values.[x,y] + min result.[x-1,y] (if y < n-1 then result.[x,y+1] else System.Int64.MaxValue)
                result.[x,y] <- min result.[x,y] trial

        let finalResult = seq { 0..n-1 } |> Seq.map (fun y -> result.[n-1,y]) |> Seq.min
        printfn "%d" finalResult
        ()

