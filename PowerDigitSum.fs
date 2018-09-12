namespace HackerRank.FSharp

module PowerDigitSum =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()
    let b0 = BigInteger.Zero
    let b1 = BigInteger.One
    let b2 = BigInteger(2)
    let b10 = BigInteger(10)

    let inline sumDigits n = 
        let rec sumDigits' acc (n: BigInteger) = 
            if n = b0 then acc
            else sumDigits' ((n%b10)+acc) (n/b10)
        sumDigits' b0 n
        
    let solveOne() = 
        let n = sc.NextInt().Value
        let result = BigInteger.Pow(b2,n)
        let result = sumDigits result
        printfn "%s" (result.ToString())

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
