namespace HackerRank.FSharp

module DigitNthPowers =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()

    /// Returns x^y
    let pow x y =
        let rec pow' acc n = 
            if n = 0 then 
                acc
            else 
                pow' (x*acc) (n - 1)
        pow' 1 y

    let getDigits n = 
        let rec getDigits' acc n = 
            if n = 0 then 
                acc
            else
                let d = n%10
                getDigits' (d::acc) (n/10)

        if n = 0 then [0]
        else getDigits' [] (abs(n))

    let isPowerSumOfDigits n a =
        a = (getDigits a |> List.map (fun d -> pow d n) |> List.sum)

//    let precompute n = 
//        let mutable result = 0
//        let max = (pow 9 n) * (n+1)
//        for i in 2..max do
//            if isPowerSumOfDigits n i then
//                printfn "%i" i
//                result <- result + i
//        result
//
//    let allResults = [| for i in 3..6 -> precompute i |]
    
    let allResults = [| 1301; 19316; 443839; 548834 |]

    let solve() = 
        let n = sc.NextInt().Value - 3
        let result = allResults.[n]
        printfn "%i" result
