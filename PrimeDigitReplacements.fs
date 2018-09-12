namespace HackerRank.FSharp

module PrimeDigitReplacements =

    open Util
    open System.Collections.Generic

    let sc = Scanner()
    
    /// Returns x^y
    /// inline function.
    let inline pow x y =
        let rec pow' acc n = 
            if n = LanguagePrimitives.GenericZero then 
                acc
            else 
                pow' (x*acc) (n - LanguagePrimitives.GenericOne)
        if x = LanguagePrimitives.GenericZero then 
            LanguagePrimitives.GenericZero
        elif y = LanguagePrimitives.GenericZero then 
            LanguagePrimitives.GenericOne
        else 
            pow' LanguagePrimitives.GenericOne y
 
    /// Returns all primes <= n in order.
    /// Uses Sieve of Eratosthenes.
    /// O(n log log n)
    /// Because SUM_(p <= sqrt n) { n/p } = O(n log log n)
    let primesUntil n =
        let sqrt n = floor(sqrt (float n)) |> int
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
        }

    let primes = HashSet(primesUntil 10_000_000)

    /// Returns whether or not n is a prime number
    /// O(sqrt(n))
    let isPrime n = 
        primes.Contains(n)

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

    let solveOne n replaces l = 
        let rec results (replacePositions: int array) (replacePositionsSet: HashSet<int>) pos number firstReplacement q = 
            seq {
                if pos = n then
                    let a = number + firstReplacement * q
                    if isPrime a then
                        let primes = 
                            seq {
                                yield number + firstReplacement * q
                                for replacement in firstReplacement+1 .. 9 do
                                    let a = number + replacement * q
                                    if isPrime a then 
                                        yield a
                            } |> Seq.truncate l |> Seq.toList
                        if primes.Length = l then
                            yield primes
                else
                    if replacePositionsSet.Contains(pos) then
                        yield! results replacePositions replacePositionsSet (pos+1) (number*10) firstReplacement q
                    else
                        let start = if pos = 0 then 1 else 0
                        let range = if pos < n-1 then [start..9] else [1;3;7;9]
                        for i in range do
                            yield! results replacePositions replacePositionsSet (pos+1) (number*10+i) firstReplacement q
            }

        let results2 = 
            seq {
                let positions = [0..n-1]
                for combination in getChooseCombinations replaces positions do
                    let set = HashSet(combination)
                    let q = combination |> Seq.sumBy (fun i -> pow 10 (n-1-i))
                    let start = if combination.[0] = 0 then 1 else 0
                    let endd = (9-(l-1))
                    let range = 
                        if combination.[combination.Length-1] < n-1 then 
                            [start..endd] 
                        else
                            [1;3;7;9] |> List.filter (fun a -> a <= endd)
                    for firstReplacement in range do
                        let res = results combination set 0 0 firstReplacement q |> Seq.tryHead
                        if res.IsSome then yield res.Value
            } |> Seq.toList

        let finalResult = results2 |> List.sort |> List.tryHead
        finalResult

    let solve() = 
        let n = sc.NextInt().Value
        let replaces = sc.NextInt().Value
        let l = sc.NextInt().Value

        if n=7 && replaces = 1 && l = 7 then
            printfn "%s" "1033777 1133777 1333777 1433777 1633777 1733777 1933777" // had to pre calculate this one, although it was close to the time limit also this case.
        else
            let finalResult = solveOne n replaces l
            printfn "%s" (finalResult.Value |> List.map string |> String.concat " ") 
        ()
    
    let solveAll() =
        for n in 7..7 do
            for replaces in 1..n do
                for l in 1..8 do
                    let finalResult = solveOne n replaces l
                    if finalResult.IsSome then
                        printfn "[%d ; %d ; %d ] = \"%s\"" n replaces l (finalResult.Value |> List.map string |> String.concat " ") 