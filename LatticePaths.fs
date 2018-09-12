namespace HackerRank.FSharp

module LatticePaths =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()
    let m = 1000000007 |> BigInteger
    let b1 = BigInteger.One

    /// Returns a modulo m
    let inline modulo m a = ((a%m)+m)%m

    /// Returns the greatest common divisor of a and b
    let inline getGreatestCommonDivisor a b =
        let rec getGreatestCommonDivisor' a b =
            let a = abs a 
            let b = abs b
            if b < a then
                getGreatestCommonDivisor' b a
            elif a = LanguagePrimitives.GenericZero then
                b
            else    
                getGreatestCommonDivisor' (b % a) a
        getGreatestCommonDivisor' a b

    /// Returns n choose m
    let rec choose n m =
        if m > n/2 then choose n (n - m)
        else
            let mutable res = b1
            for i in 1..m do
                let i = BigInteger(i)
                let m = m |> BigInteger
                let n = n |> BigInteger
                let gcd = getGreatestCommonDivisor res i
                res <- (res / gcd) * ((n - m + i) / (i / gcd));
            res

    let solveOne() = 
        let a = sc.NextInt().Value
        let b = sc.NextInt().Value
        let result = choose (a+b) b
        let result = modulo m result
        printfn "%s" (result.ToString())

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
