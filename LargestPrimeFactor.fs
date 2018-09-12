namespace HackerRank.FSharp

module LargestPrimeFactor =

    open Util
    open System

    let sc = Scanner()

    let getDistinctPrimeFactors (n: int64) = 
        let rec getDistinctPrimeFactors' acc (last: int64) (n: int64) = 
            if n = 1L then acc |> List.rev
            else
                let mutable f = max 2L last
                while n%f <> 0L && (f+1L)*(f+1L) <= n do f <- f + 1L
                if n%f <> 0L then f <- n

                if f <> last then
                    getDistinctPrimeFactors' (f::acc) f (n/f)
                else
                    getDistinctPrimeFactors' acc f (n/f)
        getDistinctPrimeFactors' [] -1L n
    
    let solveOne() = 
        let n = sc.NextLong().Value
        let result = getDistinctPrimeFactors n |> List.last
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
