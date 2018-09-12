namespace HackerRank.FSharp

module DistinctPrimeFactors =

    open Util
    open System

    let sc = Scanner()

    /// Returns the distinct prime factors of n in order.
    /// So if n = 12 = 2^2 * 3^1, this function will return [ 2; 3 ]
    /// Returns the empty list for n = 1.
    /// n should be at least 1.
    /// O(sqrt(n))
    let getDistinctPrimeFactors n = 
        let rec getDistinctPrimeFactors' acc last n = 
            if n = 1 then acc |> List.rev
            else
                let mutable f = max 2 last
                while n%f <> 0 && (f+1)*(f+1) <= n do f <- f + 1
                if n%f <> 0 then f <- n

                if f <> last then
                    getDistinctPrimeFactors' (f::acc) f (n/f)
                else
                    getDistinctPrimeFactors' acc f (n/f)
        getDistinctPrimeFactors' [] -1 n

    let isOk n k = 
        [0..k-1] |> Seq.forall (fun i -> getDistinctPrimeFactors (n+i) |> List.length = k)

    let solve() = 
        let N = sc.NextInt().Value
        let k = sc.NextInt().Value

        seq {1..N} |> Seq.filter (fun i -> isOk i k) |> Seq.iter (printfn "%d")
        ()