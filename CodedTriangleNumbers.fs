namespace HackerRank.FSharp

module CodeTriangleNumbers =

    open Util
    open System

    let sc = Scanner()

    let getTriangleIndex a =
        let isOk n = n*(n+1L)/2L = a
        let b = 2.0*(a |> float) - 0.25
        let nn = Math.Sqrt(b) - 0.5
        let n = Math.Floor(nn) |> int64
        if isOk n then Some(n)
        elif isOk (n+1L) then Some(n+1L)
        else None

    let solveOne() =
        let a = sc.NextLong().Value 
        let result = match getTriangleIndex a with
                        | Some(n) -> n
                        | None -> -1L
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()