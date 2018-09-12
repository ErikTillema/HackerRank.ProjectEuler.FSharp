namespace HackerRank.FSharp

module PentagonNumbers =

    open Util
    open System

    let sc = Scanner()

    let solve() = 
        let n = sc.NextInt().Value
        let k = sc.NextInt().Value

        let pentagonNumbers = 
            [|1..n+k+1|] |> Array.map (fun i -> i |> int64 |> (fun i -> i*(3L*i-1L)/2L)) 

        let isPentagonNumber = 
            let set = pentagonNumbers |> Set.ofArray
            (fun a -> set.Contains a)
            
        let isSpecial i =
            isPentagonNumber (pentagonNumbers.[i] + pentagonNumbers.[i-k]) || isPentagonNumber (pentagonNumbers.[i] - pentagonNumbers.[i-k])

        let allSpecial = 
            [k+1..n-1] |> List.filter isSpecial

        allSpecial |> List.map (Array.get pentagonNumbers) |> List.iter (printfn "%i")
