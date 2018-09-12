namespace HackerRank.FSharp

module PathSumFourWays =

    open Util
    open System.Collections.Generic

    type Color = ToDo | InQueue | Done

    let dv = [| (-1,0); (1,0); (0,-1); (0,1) |]
    let (++) (x,y) (dx,dy) = (x+dx, y+dy)
    let sc = Scanner()

    let transpose source =
        Array2D.init (source |> Array2D.length2) (source |> Array2D.length1) (fun x y -> source.[y,x])

    // Solve with Dijkstra, but without fancy Node and Edge records etc, because that will unfortunately cause 
    // time-outs, even though the time consumption is of the same order.
    // Also, don't use Dictionary<pos, 'a> but use arrays + getIndex + getPos, for distance and color.
    // Turns out that filling the Dictionary also takes quite a lot of time, and time is tight.
    let solve() = 
        let n = sc.NextInt().Value
        let values = Array2D.init n n (fun _ _ ->  sc.NextLong().Value) |> transpose

        let getIndex (x,y) = y*n + x
        let getPos index = (index%n, index/n)
        let getNeigbours pos =
            seq { 
                for i in 0..3 do
                    let (nx,ny) = pos ++ dv.[i]
                    if 0 <= nx && nx < n && 0 <= ny && ny < n then
                        yield (nx,ny) 
            }

        let indexSource = getIndex (0, 0)
        let q = SortedSet<int64*int>() // (dist from source, index)
        let distFromSource = Array.create (n*n) System.Int64.MaxValue
        let color = Array.create (n*n) ToDo
        distFromSource.[indexSource] <- 0L
        color.[indexSource] <- InQueue
        q.Add((0L,indexSource)) |> ignore
        while q.Count > 0 do
            let (dist,indexCurrent) = q.Min
            q.Remove((dist,indexCurrent)) |> ignore
            color.[indexCurrent] <- Done
            for (nx,ny) in getNeigbours (getPos indexCurrent) do
                let indexToo = getIndex (nx,ny)
                let oldDist = distFromSource.[indexToo]
                let newDist = dist + values.[nx,ny]
                if newDist < oldDist then
                    if color.[indexToo] = InQueue then
                        q.Remove((oldDist,indexToo)) |> ignore
                        distFromSource.[indexToo] <- newDist
                        q.Add((newDist,indexToo)) |> ignore
                    elif color.[indexToo] = ToDo then
                        distFromSource.[indexToo] <- newDist
                        q.Add((newDist,indexToo)) |> ignore
                        color.[indexToo] <- InQueue

        let result = distFromSource.[getIndex (n-1,n-1)] + values.[0,0]
        printfn "%d" result
        ()
