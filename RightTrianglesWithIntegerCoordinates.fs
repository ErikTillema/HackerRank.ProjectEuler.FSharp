namespace HackerRank.FSharp

module RightTrianglesWithIntegerCoordinates =

    open Util
    open System
    open System.Collections.Generic
    
    let sc = Scanner()

    /// Returns the greatest common divisor of a and b
    /// O(log min(a, b))
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

    let solve() = 
        let n = sc.NextInt().Value
        let countSimple = n*n*3 |> int64
        let mutable countDifficult = 0L
        for x in 1..n do
            for y in 1..n do
                let g = getGreatestCommonDivisor x y
                let x', y' = x/g, y/g
                let add1 = min (y/x') ((n-x)/y')
                let add2 = min ((n-y)/x') (x/y')
                countDifficult <- countDifficult + (int64 add1) + (int64 add2)
        
        let result = countSimple + countDifficult
        printfn "%d" result
        ()