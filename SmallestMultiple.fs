namespace HackerRank.FSharp

module SmallestMultiple =

    open Util
    open System
    open System.Collections.Generic

    let sc = Scanner()

    /// Returns x^y
    let inline pow x y =
        let rec pow' acc n = 
            if n = LanguagePrimitives.GenericZero then 
                acc
            else 
                pow' (x*acc) (n - LanguagePrimitives.GenericOne)
        if x = LanguagePrimitives.GenericZero then 
            LanguagePrimitives.GenericZero
        elif y = LanguagePrimitives.GenericZero then 
            LanguagePrimitives.GenericOne
        else 
            pow' LanguagePrimitives.GenericOne y

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
    
    let updatePrimeFactors (factors: Dictionary<int,int>) toadd =
        for (p,m) in toadd do
            if not (factors.ContainsKey(p)) then
                factors.Add(p,0)
            factors.[p] <- max factors.[p] m

    let solveOne() = 
        let n = sc.NextInt().Value
        let primeFactors = Dictionary<_,_>()
        for i in 1..n do
            updatePrimeFactors primeFactors (getPrimeFactors i)
        let result = primeFactors 
                        |> Seq.map (fun kvp -> (kvp.Key |> int64, kvp.Value)) 
                        |> Seq.fold (fun res (p,m) -> res * (pow p m)) 1L
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
