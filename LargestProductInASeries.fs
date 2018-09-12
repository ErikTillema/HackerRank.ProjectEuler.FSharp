namespace HackerRank.FSharp

module LargestProductInASeries =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()

    let getMaxProduct n k (digits: int[]) = 
        let mutable result = Int32.MinValue
        let updateResult zeroes product = 
            if zeroes = 0 then
                result <- max result product
            else 
                result <- max result 0
        let mutable zeroes = 0
        let mutable product = 1
        for i in 0..k-1 do
            if digits.[i] = 0 then
                zeroes <- zeroes + 1
            else
                product <- product * digits.[i]
        
        updateResult zeroes product

        for i in 0..n-k-1 do
            // remove first digit
            if digits.[i] = 0 then zeroes <- zeroes - 1
            else product <- product / digits.[i]
            // add next digit
            if digits.[i+k] = 0 then zeroes <- zeroes + 1
            else product <- product * digits.[i+k]

            updateResult zeroes product

        result

    let solveOne() = 
        let n = sc.NextInt().Value
        let k = sc.NextInt().Value
        let digits = sc.Next().Value.ToCharArray() |> Array.map (fun c -> (c |> int) - ('0' |> int))
        let result = getMaxProduct n k digits
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
