namespace HackerRank.FSharp

module MaximumPathSum2 =

    open Util

    let sc = Scanner()

    let solveOne() = 
        let n = sc.NextInt().Value
        let values = Array.init n (fun i -> Array.init (i+1) (fun _ -> sc.NextInt().Value))
        let maxPath = Array.init n (fun i -> Array.create (i+1) 0)
        maxPath.[0].[0] <- values.[0].[0]
        for y in 1..n-1 do
            for x in 0..y do
                maxPath.[y].[x] <-  values.[y].[x] +
                                    match x with
                                    | 0 -> maxPath.[y-1].[x]
                                    | _ when x=y -> maxPath.[y-1].[x-1]
                                    | _ -> max (maxPath.[y-1].[x-1]) (maxPath.[y-1].[x])
        let result = seq {0..n-1} |> Seq.map (fun x -> maxPath.[n-1].[x]) |> Seq.max
        printfn "%d" result
        ()
        
    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
