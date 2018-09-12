namespace HackerRank.FSharp

module SelfPowers =

    open Util
    open System.Numerics

    let sc = Scanner()

    let m = 10_000_000_000L
    let m2 = 10_000_000_000I
    let m3 = 10_000_000_000UL

    /// Returns a modulo m
    let inline modulo m a = ((a%m)+m)%m

    let addModulo (m: int64) (x: int64) (y: int64) = 
        let x = modulo m x 
        let y = modulo m y
        (x + y) |> modulo m

    let multiplyModulo (m: int64) (x: int64) (y: int) = 
        (x * (int64 y)) |> modulo m

    let squareModulo (x: int64) = 
        let x = bigint x // avoid overflow by using BigInteger
        (x*x) |> modulo m2 |> int64
    
    let rec squareModulo2 (x: int64) = 
        let b = (1L<<<30)
        if x <= b then
            modulo m (x*x)
        else
            if x%2L = 0L then
                let y = x/2L
                multiplyModulo m (squareModulo2 y) 4
            else
                let y = x/2L
                let t = multiplyModulo m (squareModulo2 y) 4
                addModulo m t (4L*y + 1L)

    let rec squareModulo3 (x: int64) = 
        let x = uint64 x
        let b = (1UL<<<30)
        if x <= b then
            modulo m3 (x*x) |> int64
        else
            let x = modulo m3 x
            (   (x >>> 30)*((x <<< 30) % m3) + 
                x*(x &&& ((1UL <<< 30) - 1UL)) 
            ) % m3 |> int64

    /// Returns x^y modulo m
    /// O(log y)
    let rec powModulo (m: int64) (x: int) (y: int) = 
        if y = 0 then 
            modulo m 1L
        else
            let resultHalf = powModulo m x (y/2)
            let mutable result = squareModulo3 resultHalf
            if y%2 = 1 then result <- multiplyModulo m result x
            result

    let solve() = 
        let N = sc.NextInt().Value
        [1..N] |> Seq.map (fun i -> powModulo m i i) |> Seq.fold (addModulo m) 0L |> printfn "%d"
        ()