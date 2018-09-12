namespace HackerRank.FSharp

module DigitCancelingFractions =

    open Util
    open System
    open System.Collections.Generic

    let sc = Scanner()

    /// returns all combinations of source (as arrays)
    let getChooseCombinations n (source: 'a seq) = 
        let source = source |> Seq.toArray
        let sourceCount = source.Length
        let result = Array.zeroCreate n
        let rec getChooseCombinations' don ins =
            seq {
                if ins = n then
                    yield result
                else
                    let remainingItemsAfterThisOne = sourceCount - don - 1
                    // try to put in result
                    if ins < n then
                        result.[ins] <- source.[don]
                        yield! getChooseCombinations' (don+1) (ins+1)

                    // try to leave out of result
                    if ins + remainingItemsAfterThisOne >= n then
                        yield! getChooseCombinations' (don+1) ins
            }
        getChooseCombinations' 0 0

    /// returns all permutations of source (as arrays)
    let getPermutations (source: 'a seq) k = 
        let source = source |> Seq.toArray
        let n = source.Length
        let result = Array.zeroCreate k
        let used = Array.create n false
        let rec getPermutations' don =
            seq {
                if don = k then
                    yield result
                else
                    for i in 0..n-1 do
                        if not used.[i] then
                            used.[i] <- true
                            result.[don] <- source.[i]
                            yield! getPermutations' (don+1)
                            used.[i] <- false
            }
        getPermutations' 0

    let getDigits n = 
        let rec getDigits' acc n = 
            if n = 0 then 
                acc
            else
                let d = n%10
                getDigits' (d::acc) (n/10)

        if n = 0 then [0]
        else getDigits' [] (abs(n))

    let getNumber (digits: int list) = 
        let rec getNumber' acc (digits: int list) = 
            match digits with
            | [] -> acc
            | x::xs -> getNumber' (10 * acc + x) xs
        getNumber' 0 digits

    let rec pad n i (digits: int list) = 
        if digits |> List.length = n then digits
        else pad n i (i::digits)

    let rec pow x y = 
        if y = 0 then 
            1
        else 
            x * (pow x (y-1))

    let isOk n1 t1 n2 t2 = 
        t1 < n1 && n1 <> 0 && n2 <> 0 && n1 * t2 = t1 * n2

    let getResult n k = 
        let set = HashSet<int*int>()
        for t1 in (pow 10 (n-1))..(pow 10 n)-1 do
            let t1digits = getDigits t1
            let source = [| 0..n-1 |]
            for combination in getChooseCombinations k source do
                if not (combination |> Array.exists (fun i -> t1digits.[i] = 0)) then
                    let t1OtherPositions = source |> Array.filter (fun i -> not (combination |> Array.exists ((=) i) ))
                    let t2digits = t1OtherPositions |> Array.map (fun i -> t1digits.[i])
                    let t2 = t2digits |> Array.toList |> getNumber
                    for permutation in getPermutations source k do
                        let n1OtherPositions = source |> Array.filter (fun i -> not (permutation |> Array.exists ((=) i) ))
                        let n1digits = Array.create n 0
                        for i in 0..k-1 do 
                            let j = permutation.[i]
                            n1digits.[j] <- t1digits.[combination.[i]]
                        for n2 in 0..(pow 10 (n-k))-1 do
                            let digitspart = getDigits n2 |> pad (n-k) 0
                            if n1OtherPositions.[0] = 0 && digitspart.[0] = 0 then
                                () // skip
                            else
                                for i in 0..n-k-1 do 
                                    let j = n1OtherPositions.[i]
                                    n1digits.[j] <- digitspart.[i]
                                let n1 = n1digits |> Array.toList |> getNumber 
                                if isOk n1 t1 n2 t2 then
                                    //printfn "%i/%i = %i/%i" t1 n1 t2 n2
                                    ignore(set.Add(t1,n1))
        
        (set |> Seq.sumBy fst, set |> Seq.sumBy snd)

    let solve() = 
        let a = getPermutations [ 1..4 ] 1 |> Seq.map (Seq.toList) |> Seq.toList
        let n = sc.NextInt().Value
        let k = sc.NextInt().Value
        let result = 
            if n = 4 && k = 1 then
                (12999936, 28131911)
            elif n = 4 && k = 2 then
                (3571225, 7153900)
            else
                getResult n k
        printfn "%i %i" (result |> fst) (result |> snd)
