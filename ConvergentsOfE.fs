namespace HackerRank.FSharp

module ConvergentsOfE =

    open System
    open Util
    open System.Numerics

    type Fraction = { Numerator: bigint; Denominator: bigint }

    let createFraction n = { Numerator = n; Denominator = 1I }
    let invertFraction { Numerator = n; Denominator = d } = { Numerator = d; Denominator = n }
    let addFraction { Numerator = n1; Denominator = d1 } { Numerator = n2; Denominator = d2 } = { Numerator = n1*d2+n2*d1; Denominator = d1*d2 }

    let sc = Scanner()

    let continuedFractions() = Seq.append ([2;1]) (Seq.initInfinite (fun i -> 2*i+2) |> Seq.collect (fun i -> [i;1;1]) )

    let getConvergent n = 
        let values = continuedFractions() |> Seq.take n |> Seq.rev |> Seq.map bigint |> Seq.toList
        let (head, tail) = values |> List.splitAt 1
        let start = head |> List.head |> createFraction
        let folder state value = state |> invertFraction |> addFraction (createFraction value)
        tail |> Seq.fold folder start

    let getDigits (n: bigint) = 
        n |> string |> Seq.map (fun c -> (int c) - (int '0'))

    let solve() = 
        let n = sc.NextInt().Value
        let { Numerator = numerator; Denominator = denominator } = getConvergent n
        let gcd = bigint.GreatestCommonDivisor(numerator,denominator)
        let result = numerator/gcd |> getDigits |> Seq.sum
        printfn "%d" result
        ()
