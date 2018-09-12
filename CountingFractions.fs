namespace HackerRank.FSharp

module CountingFractions =

    open Util
    open System.Collections.Generic

    let sc = Scanner()
    let N = 1_000_000

    let getSmallestPrimeFactorsUntil (n: int) =
        let sqrt n = floor(sqrt (float n)) |> int
        let smallestPrimeFactor = Array.replicate (n+1) 0
        smallestPrimeFactor.[0] <- 1
        smallestPrimeFactor.[1] <- 1
        for i in 2..sqrt n do
            if smallestPrimeFactor.[i] = 0 then
                for j in i*i..i..n do
                    if smallestPrimeFactor.[j] = 0 then
                        smallestPrimeFactor.[j] <- i
        smallestPrimeFactor
    
    /// We have to calculate many phi(n), so we can try to make use of the following rule to speed things up:
    /// phi(m n) = phi(m) phi(n) if m and n are relatively prime.
    let rec eulerTotient = 
        let cache = Dictionary<int,int>()
        cache.Add(1,1)
        let smallestPrimeFactor = getSmallestPrimeFactorsUntil N
        let rec factorize p n pn =
            if n%p = 0 then factorize p (n/p) (pn*p)
            else (n,pn)

        (fun n ->
            match cache.TryGetValue(n) with
            | true, value -> value
            | false, _ ->
                let result = 
                    let p = smallestPrimeFactor.[n]
                    if p = 0 then
                        n-1
                    else
                        let (a,pn) = factorize p n 1
                        (eulerTotient a)*(pn/p*(p-1))
                cache.Add(n, result)
                result
        )

    let allSolutions = 
        let result = Array.create (N+1) 0L
        result.[2] <- 1L
        for i in 3 .. N do 
            result.[i] <- result.[i-1] + (i |> eulerTotient |> int64)
        result
    
    let solveOne() = 
        let n = sc.NextInt().Value
        let result = allSolutions.[n]
        printfn "%d" result
        ()

    let solve() = 
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()

