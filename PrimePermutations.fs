namespace HackerRank.FSharp

module PrimePermutations =

    open Util
    open System.Numerics

    let sc = Scanner()

    let primesUntil n =
        let sqrt n = floor (sqrt (float n)) |> int
        let notprime = Array.replicate (n+1) false
        notprime.[0] <- true
        notprime.[1] <- true
        for i in 2..sqrt n do
            if not notprime.[i] then
                for j in i*i..i..n do
                    notprime.[j] <- true

        seq {
            for i in 2..n do
                if not notprime.[i] then
                    yield i
        } |> Seq.toArray

    /// Returns the digits in n (base k) in order
    /// For example, for k=2, n=11 returns [1;0;1;1].
    /// O(log_k n)
    let getDigitsBase k n = 
        let rec getDigitsBase' acc n = 
            if n = 0 then 
                acc
            else
                let d = n%k
                getDigitsBase' (d::acc) (n/k)

        if n = 0 then [0]
        else getDigitsBase' [] (abs(n))

    /// Returns the digits in n (base 10) in order
    /// For example, returns [1;2] for n=12.
    /// O(log_10 n)
    let getDigits n = getDigitsBase 10 n
    
    let getDigitCounts a = 
        let digits = getDigits a
        let result = Array.replicate 10 0
        for digit in digits do
            result.[digit] <- result.[digit] + 1
        result |> Array.toList

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

    // When I write triplet this means a triplet (k=3) or a quartet (k=4)
    let solve() = 
        let n = sc.NextInt().Value
        let k = sc.NextInt().Value

        let getTripletsForPermutedPrimes permutedPrimesWithDigitCount =
            let primes = permutedPrimesWithDigitCount |> Seq.map fst |> Seq.sort |> Seq.toArray
            let primesSet = primes |> Set.ofArray
            let l = Array.length primes

            let getTriplet [| pa; pb |] =
                let pa,pb = (min pa pb, max pa pb) // just to be sure..
                if pa < n then
                    let pothers = [2..k-1] |> List.map (fun i -> pa + i * (pb-pa))
                    let isOk = pothers |> Seq.forall (fun c -> Set.contains c primesSet)
                    if isOk then
                        let triplet = pa :: pb :: pothers
                        Some(triplet)
                    else  
                        None
                else
                    None

            if l >= k then
                primes |> getChooseCombinations 2 |> Seq.map getTriplet |> Seq.filter Option.isSome |> Seq.map Option.get
            else
                Seq.empty

        let triplets = 
            primesUntil 1_000_000
            |> Seq.map (fun p -> (p, getDigitCounts p)) 
            |> Seq.groupBy snd 
            |> Seq.map snd
            |> Seq.collect getTripletsForPermutedPrimes

        let toBigInt (triplet: int seq) = 
            let s = triplet |> Seq.map string |> String.concat ""
            BigInteger.Parse(s)
            
        triplets |> Seq.map toBigInt |> Seq.sort |> Seq.iter (printfn "%A")
        ()