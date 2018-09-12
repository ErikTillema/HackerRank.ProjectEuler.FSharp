namespace HackerRank.FSharp

module DigitFactorials =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()

    let rec getFactorial n =
        match n with 
        | 0 -> 1
        | 1 -> 1
        | _ -> n * getFactorial (n-1)

    let factorial = [| for i in 0..9 -> getFactorial i |]

    let getDigits n = 
        let rec getDigits' acc n = 
            if n = 0 then 
                acc
            else
                let d = n%10
                getDigits' (d::acc) (n/10)

        if n = 0 then [0]
        else getDigits' [] (abs(n))

    let getSumFactorial n = 
        n |> getDigits |> List.map (fun d -> factorial.[d]) |> List.sum

    let isOk n = (getSumFactorial n) % n = 0

    let getResult n = 
        let mutable result = 0
        for i in 10..n-1 do
            if isOk i then 
                //printfn "%i" i
                result <- result + i
        result

    let solve() = 
        let n = sc.NextInt().Value
        let result = getResult n
        printfn "%i" result

