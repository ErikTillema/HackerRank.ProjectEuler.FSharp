namespace HackerRank.FSharp

module HighlyDivisibleTriangularNumber =

    open Util
    open System

    let sc = Scanner()
    let maxn = 1000

    let getPrimeFactors n = 
        let rec getPrimeFactors' acc last n = 
            if n = 1 then acc |> List.rev
            else
                let mutable f = max 2 last
                while n%f <> 0 && (f+1)*(f+1) <= n do f <- f + 1
                if n%f <> 0 then f <- n

                if f <> last then
                    getPrimeFactors' ((f,1)::acc) f (n/f)
                else
                    let (f,m)::xs = acc
                    getPrimeFactors' ((f,m+1)::xs) f (n/f)
        getPrimeFactors' [] -1 n

    let countDivisors n = 
        getPrimeFactors n |> List.fold (fun res (_,m) -> res * (m+1)) 1

    let allResults = 
        seq {
            let mutable n = 0
            let mutable k = 1
            let mutable sum = 1
            while n <= maxn do
                let divisors = countDivisors sum
                while divisors > n do
                    yield sum
                    n <- n + 1
                k <- k + 1
                sum <- sum + k
        } |> Seq.toArray

    let solveOne() = 
        let n = sc.NextInt().Value
        let result = allResults.[n]
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
