namespace HackerRank.FSharp

module ArithmeticExpressions =

    open Util
    open System
    open System.Collections.Generic
    
    let sc = Scanner()

    // approach: for every subset of S calculate all the rational numbers you can make with that subset.
    // Do that by dividing the subset into two smaller subsets and combining the results of those two subsets.
    // For example:
    // {1} -> [ 1 ]  {2} -> [ 2 ]  {3} -> [ 3 ]  {4} -> [ 4 ]
    // {1,2} -> [ 1 ] *+/- [ 2 ]
    //          [ 2 ] /-   [ 1 ] -> [ 2; 3; 1/2; -1; 1; ]
    // {1,3} -> ...
    // {1,2,3} -> {1,2} *+/- {3}
    //            {1} *+/- {2,3}
    //            {}

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

    type Fraction = { Numerator: int64; Denominator: int64 }

    let createFraction n d = 
        let gcd = getGreatestCommonDivisor n d
        let n,d = if d < 0L then (-n,-d) else (n,d)
        { Numerator = n/gcd; Denominator = d/gcd }
    let (++)   { Numerator = n1; Denominator = d1 } { Numerator = n2; Denominator = d2 } = createFraction (n1*d2+n2*d1) (d1*d2)
    let (--)   { Numerator = n1; Denominator = d1 } { Numerator = n2; Denominator = d2 } = createFraction (n1*d2-n2*d1) (d1*d2)
    let ( **)  { Numerator = n1; Denominator = d1 } { Numerator = n2; Denominator = d2 } = createFraction (n1*n2) (d1*d2)
    let (/-/)  { Numerator = n1; Denominator = d1 } { Numerator = n2; Denominator = d2 } = createFraction (n1*d2) (d1*n2)
    let isInteger  { Numerator = _ ; Denominator = d  } = d = 1L
    let toInteger  { Numerator = n ; Denominator = d  } = n/d

    /// returns all subsets of source (as lists)
    let getSubsets (source: 'a seq) = 
        let source = source |> Seq.toArray
        let rec getSubsets' don acc =
            seq {
                if don = source.Length then
                    yield List.rev acc
                else
                    yield! getSubsets' (don+1) acc
                    yield! getSubsets' (don+1) (source.[don] :: acc)
            }
        getSubsets' 0 []

    let rec getExpressionValues = 
        let cache = Dictionary<int64 list, HashSet<Fraction>>()
        (fun values -> 
            if cache.ContainsKey(values) then cache.[values]
            else
                let result = HashSet<Fraction>()
                match values with
                | [ x ] ->  result.Add(createFraction x 1L) |> ignore
                | _ ->
                    for subset in getSubsets values do
                        if subset.Length <> values.Length && subset.Length <> 0 then
                            let s1 = subset
                            let s2 = values |> List.except s1
                            for i1 in getExpressionValues s1 do
                                for i2 in getExpressionValues s2 do
                                    result.Add(i1 ++ i2) |> ignore
                                    result.Add(i1 ** i2) |> ignore
                                    result.Add(i1 -- i2) |> ignore
                                    result.Add(i2 -- i1) |> ignore
                                    result.Add(i1 /-/ i2) |> ignore
                                    result.Add(i2 /-/ i1) |> ignore
                            ()
                cache.Add(values, result)
                result
        )

    let solve() = 
        let values = sc.NextInt().Value
        let value = List.init values (fun _ -> sc.NextLong().Value) |> List.sort
        let result = getExpressionValues value 
                        |> Seq.filter isInteger |> Seq.map toInteger |> Seq.filter (fun i -> i >= 1L) |> Seq.sort
                        |> Seq.mapi (fun index value -> (index, value)) |> Seq.tryFindBack (fun (index,value) -> (int64 index)+1L = value)
                        |> Option.map snd
                        |> Option.defaultValue 0L
        printfn "%d" result
        ()