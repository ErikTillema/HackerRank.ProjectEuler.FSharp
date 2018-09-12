namespace HackerRank.FSharp

module LargeNonMersennePrime =

    open Util
    open System
    open System.Collections.Generic
    open System.Numerics
    open System.Text
    
    // approach:
    // A* B^C + D mod m
    // A, B, C, D < 10^9 = 2^30
    // m = 10^12
    // B^C => log(C) = 30 steps, 30 times half*half*B mod m
    // half*half => 10^12 * 10^12 = 10^24, doesn't fit in int64.
    // so we can do a couple of things:
    // - use doubling, 
    // - or smart trick with digit multiplication?
    //   12+11+10+...2+1=78
    // - or, what if we split a 12 digit number a up in 2 parts of 6 digits: a = a1 + 10^6*a2
    //   by taking a1 = a%10^6, a2 = a/10^6
    //   then b*a = b*a1 + 10^6*b*a2 = b*a1 + 10^6*(b*a2 mod 10^12) and each part fits in int64.
    //   NB: this should work in general I think.
    //   Probably that's why many times we see m = 10^9+7, which is prime.
    //   ---
    //   Let's see. Suppose m = 10^12-x, so prime of 12 digits
    //   We want to calculate a*b mod m
    //   Let's take a = a1 + 10^6*a2
    //   Then (mod m) b*a = b*a1 + 10^6*b*a2 = (b*a1 mod m) + 10^6*(b*a2 mod m) and each part fits into int64 again.

    /// Returns a modulo m
    let inline modulo m a = 
        if a >= LanguagePrimitives.GenericZero then 
            a%m 
        else 
            ((a%m)+m)%m // avoid overflow by adding m only if a%m is negative

    let addModulo (m: int64) (x: int64) (y: int64) = 
        // if m < int.MAX_VALUE / 2, then using int64 to avoid overflow is not necessary.
        let x = modulo m x
        let y = modulo m y
        (x + y) |> modulo m

    let multiplyModulo (m: int64) (x: int64) (y: int64) = 
        let x = modulo m x
        let y = modulo m y
        let y1 = y % 1_000_000L
        let y2 = y / 1_000_000L
        let (+) x y = addModulo m x y
        let x_y2 = x*y2 |> modulo m
        (x * y1 + 1_000_000L * x_y2) |> modulo m

    /// Returns x^y modulo m
    /// O(log y)
    let rec powModulo (m: int64) (x: int64) (y: int) = 
        if y = 0 then 
            modulo m 1L
        else
            let (*) x y = multiplyModulo m x y
            let resultHalf = powModulo m x (y/2)
            let mutable result = resultHalf * resultHalf
            if y%2 = 1 then result <- result * x
            result
        
    let solve() =
        let m = 1_000_000_000_000L
        let (+) x y = addModulo m x y
        let (*) x y = multiplyModulo m x y
        let solveOne() = 
            let tokens = Console.In.ReadLine().Split(" ".ToCharArray())
            let A = Int64.Parse(tokens.[0])
            let B = Int64.Parse(tokens.[1])
            let C = Int32.Parse(tokens.[2])
            let D = Int64.Parse(tokens.[3])
            let result = A*(powModulo m B C) + D
            result

        let n = Int32.Parse(Console.In.ReadLine())
        let sum = seq { 1..n } |> Seq.map (fun _ -> solveOne()) |> Seq.fold (fun s value -> s + value) 0L
        printfn "%012d\n" sum
        ()