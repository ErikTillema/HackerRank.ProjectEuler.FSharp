namespace HackerRank.FSharp

module DigitFactorialChains =

    open Util
    open System.Collections.Generic

    let sc = Scanner()
    
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
    /// Consider using ToString() and looking at the chars, instead of this implementation.
    /// In particular for bigints. This can make a huge difference in performance.
    let getDigits n = getDigitsBase 10 n

    let factorial = 
        let result = Array.create 10 1
        for i in 2..9 do result.[i] <- result.[i-1] * i
        (fun n -> result.[n])

    let getNext = 
        let cache = Array.create 3_000_000 -1 // > 6*9! (for 999999)
        (fun n ->
            match cache.[n] with
            | -1 ->     let result = n |> getDigits |> List.sumBy factorial
                        cache.[n] <- result
                        result
            | result -> result
        )

    // ok, apparently 30*10^6 lookups in a HashSet / Dictionary / F# set etc is too slow and takes at least 10 seconds.
    let getChainLength_slow n =
        let set = HashSet()
        set.Add(n) |> ignore
        let rec getChainLength' n = 
            let next = getNext n
            if set.Contains(next) then
                set.Count
            else
                set.Add(next) |> ignore
                getChainLength' next
        getChainLength' n

    // OK, if I only had made a drawing before...
    // Thinking of this problem as a graph problem, we have the following situation:
    // Every node has exactly 1 outgoing edge. Every sub-graph contains a cycle (as stated in the problem statement).
    // So every sub-graph looks like a cycle with zero or more lines, leading to the cycle.
    // If we know the chain length of all numbers in the sub-graphs, and we are looking at a new number y with next y = x,
    // Then there are 2 possibilities:
    // - y and x not known yet, so build a new cycle and set all chain lengths for all numbers in the chain
    //   all numbers in the cycle have chain length = length of cycle. 
    //   All numbers in the line leading to the cycle have chain length = length of cycle + length away from cycle.
    // - y is unknown, x is known, so part of a cycle, so chain length of y = 1 + chain length of x
    // - y is known, return result immediately.
    let getChainLength2 = 
        let chainLength = Dictionary<int,int>()
        let getChain n = 
            let rec getChain' n map = 
                let next = getNext n
                let l = Map.count map
                match Map.tryFind next map with
                | Some(index) -> (map,index) // (map, startOfCycle)
                | None -> getChain' next (Map.add next l map)
            getChain' n (Map([n,0]))

        (fun n ->
            match chainLength.TryGetValue(n) with
            | true, l -> l
            | false, _ ->
                let x = getNext n
                match chainLength.TryGetValue(x) with
                | true, l ->    l+1
                | false, _ ->   let (chain, startOfCycle) = getChain n
                                let cycleLength = (Map.count chain) - startOfCycle
                                chain |> Map.iter (fun number index ->  let result = 
                                                                            if index < startOfCycle then cycleLength+startOfCycle-index 
                                                                            else cycleLength
                                                                        chainLength.[number] <- result
                                                  )
                                chainLength.[n]
        )
    
    // now let's make it even better:
    // when looking for the chain length of x, if x is not found in cache, then start counting
    // until a cycle is found.
    let getChainLength = 
        let chainLengths = Dictionary<int,int>()

        let rec getChain a map = // returns (chain length of a, whether a is inside cycle, start index of cycle if a is inside cycle)
            match chainLengths.TryGetValue(a) with
            | true, l -> (l, false, 0)
            | false, _ ->
                let n = Map.count map
                let next = getNext a
                match Map.tryFind next map with
                | Some(index) -> // cycle found
                    chainLengths.[a] <- n-index
                    (n-index, true, index)
                | None -> 
                    let currentIndex = n-1
                    let nextIndex = n
                    let (chainLength, insideCycle, startOfCycle) = getChain next (Map.add next nextIndex map)
                    let stillInsideCycle = insideCycle && currentIndex >= startOfCycle
                    let result = chainLength + (if stillInsideCycle then 0 else 1)
                    chainLengths.[a] <- result
                    (result, stillInsideCycle, startOfCycle)

        (fun n ->   let (result, _, _) = getChain n (Map([n,0]))
                    result )

    let solveOne() = 
        let n = sc.NextInt().Value
        let l = sc.NextInt().Value
        let results = seq { 0..n } |> Seq.filter (fun i -> getChainLength i = l) |> Seq.toList
        if results.Length = 0 then
            printfn "-1"
        else            
            printfn "%s" (results |> List.map string |> String.concat " ")
        ()

    let solve() = 
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()
        ()
