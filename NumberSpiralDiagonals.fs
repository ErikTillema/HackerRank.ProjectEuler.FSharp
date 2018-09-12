namespace HackerRank.FSharp

module NumberSpiralDiagonals =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()
    let m = 1000000007

    let inline modulo m a = ((a%m)+m)%m

    /// Returns x^y modulo m
    /// O(log y)
    let rec powModulo m x y = 
        if y = 0 then 
            modulo m 1
        else
            let resultHalf = powModulo m x (y/2) |> int64
            let x = x |> int64
            let m = m |> int64
            let mutable result = modulo m (resultHalf * resultHalf) // avoid overflow by using int64
            if y%2 = 1 then result <- modulo m (result * x)
            result |> int

    let multiplyModulo m x y = 
        let x = modulo m x |> int64
        let y = modulo m y |> int64
        (x * y) |> modulo (m |> int64) |> int

    let addModulo m x y = 
        let x = modulo m x |> int64
        let y = modulo m y |> int64
        (x + y) |> modulo (m |> int64) |> int

    /// Returns the multiplicative inverse of a modulo p,
    /// so returns x for which a*x = 1 modulo p.
    /// p should be prime.
    /// O(log p)
    let inverseModuloPrime p a =
        powModulo p a (p-2)

    let getResult (n: int64) = 
        let (*) a b = multiplyModulo m a b
        let (+) a b = addModulo m a b
        let n = modulo (m |> int64) n |> int
        let inv3 = inverseModuloPrime m 3
        let part1 = 16 * inv3 * n * n * n
        let part2 = 10 * n * n
        let part3 = 26 * inv3 * n 
        part1 + part2 + part3 + 1

    let solveOne() = 
        let n = sc.NextLong().Value
        let n = (n-1L)/2L
        let result = getResult n
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
