namespace HackerRank.FSharp

module TotientPermutation =

    open Util
    open System.Collections.Generic

    let sc = Scanner()

    let getDigitsBase k n = 
        let rec getDigitsBase' acc n = 
            if n = 0 then 
                acc
            else
                let d = n%k
                getDigitsBase' (d::acc) (n/k)

        if n = 0 then [0]
        else getDigitsBase' [] (abs(n))

    /// Returns the digits in n (base 10) in order
    /// For example, returns [1;2] for n=12.
    /// O(log_10 n)
    /// Consider using ToString() and looking at the chars, instead of this implementation.
    /// In particular for bigints. This can make a huge difference in performance.
    let getDigits n = getDigitsBase 10 n
    
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
        let smallestPrimeFactor = getSmallestPrimeFactorsUntil 10_000_000
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
    
    let isPermutation (a: int) (b: int) = 
        (a |> getDigits |> List.sort) = (b |> getDigits |> List.sort)

    let solveAll() = 
        let mutable min = 100.
        seq { 2..10_000_000 } 
            |> Seq.filter (fun a -> isPermutation (eulerTotient a) a) 
            |> Seq.iter (fun a -> 
                            let trial = (float a)/(float (eulerTotient a))
                            if trial < min then 
                                min <- trial
                                printf "%d; " a
                        )

    let allSolutions = [ 21; 291; 2817; 2991; 4435; 20617; 45421; 69271; 75841; 162619; 176569; 284029; 400399; 474883; 732031; 778669; 783169; 1014109; 1288663; 1504051; 1514419; 1924891; 1956103; 2006737; 2044501; 2094901; 2239261; 2710627; 2868469; 3582907; 3689251; 4198273; 4696009; 5050429; 5380657; 5886817; 6018163; 6636841; 7026037; 7357291; 7507321; 8316907; 8319823; ]

    let solve() = 
        //solveAll()
        let n = sc.NextInt().Value
        let result = allSolutions |> List.takeWhile (fun a -> a < n) |> List.last
        printfn "%d" result
        ()

    let solve_too_slow() = 
        let n = sc.NextInt().Value
        let result = 
            seq { 2..n-1 } 
            |> Seq.filter (fun a -> isPermutation (eulerTotient a) a) 
            |> Seq.minBy (fun a -> (float a)/(float (eulerTotient a)) )
        printfn "%d" result
        ()

