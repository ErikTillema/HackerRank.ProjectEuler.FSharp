namespace HackerRank.FSharp

module CombinatoricSelections =

    open Util

    let sc = Scanner()

    let solve() = 
        let n = sc.NextInt().Value
        let k = sc.NextLong().Value

        let results = Array2D.create (n+1) (n+1) (Some(0L))
        for i in 0..n do 
            results.[i,0] <- Some(1L)
            results.[i,i] <- Some(1L)
        for i in 1..n do
            for j in 1..i-1 do
                results.[i,j] <-
                    match results.[i-1,j], results.[i-1,j-1] with
                    | (Some(a), Some(b)) when a+b <= k -> Some(a+b)
                    | _ -> None

        let result = results |> Seq.cast<int64 option> |> Seq.filter Option.isNone |> Seq.length                
        printfn "%d" result
        ()
 