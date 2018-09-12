namespace HackerRank.FSharp

module AmicableChains =

    open Util
    open System
    open System.Collections.Generic
    open System.Numerics
    
    let sc = Scanner()

    // solved in Java finally, but with exact same approach (and finally got it accepted after using the precalculated chains).

    let getNext n = 
        let isDivisor i = n % i = 0
        let sqrt n = n |> float |> sqrt |> floor |> int
        let result = 
            seq { 1 .. sqrt n } |> Seq.filter isDivisor |> Seq.map (fun d -> if d*d=n then d else d+(n/d)) |> Seq.sum
        result - n
    
    let getMax cycleResult1 cycleResult2 = 
        match cycleResult1, cycleResult2 with
        | None, None -> None
        | None, _ -> cycleResult2
        | _, None -> cycleResult1
        | Some(l1,i1), Some(l2,i2) when l1 > l2 -> Some(l1,i1)
        | Some(l1,i1), Some(l2,i2) when l2 > l1 -> Some(l2,i2)
        | Some(l1,i1), Some(l2,i2) -> Some(l1, min i1 i2)
    
    let getAllChains maxN = 
        let rec getAllChains' seen acc i = 
            if i > maxN then
                List.rev acc
            elif Set.contains i seen then
                // skip
                getAllChains' seen acc (i+1)
            else
                // look for chain from i
                let rec getChainFrom cur index chainMap chainList = 
                    if Map.containsKey cur chainMap then
                        // cycle found
                        let indexCycleStart = Map.find cur chainMap
                        let cycle = chainList |> List.rev |> List.skip indexCycleStart
                        let cycleResult = (cycle |> List.length, cycle |> List.min, cycle |> List.max)
                        (chainMap, Some(cycleResult))
                    elif cur <= 1 || cur > maxN || Set.contains cur seen then
                        // no chain found, return None
                        (chainMap, None)
                    else 
                        getChainFrom (getNext cur) (index+1) (Map.add cur index chainMap) (cur :: chainList)
                
                let (chainMap, cycleResult) = getChainFrom i 0 Map.empty List.empty
                let added = chainMap |> Seq.map (fun kvp -> kvp.Key) |> Set.ofSeq
                let isBetter (l1,min1,max1) (l2,min2,max2) = max1 <= max2 && (l1 > l2 || (l1 = l2 && min1 <= min2))
                match cycleResult with
                | None ->           getAllChains' (Set.union seen added) acc (i+1)
                | Some(chain) ->    
                    if acc |> List.exists (fun c -> isBetter c chain) then
                        getAllChains' (Set.union seen added) acc (i+1)
                    else
                        getAllChains' (Set.union seen added) (chain::acc) (i+1)
                
        getAllChains' Set.empty [] 2

    let getLongestChain maxN = 
        let rec getLongestChain' seen result i = 
            if i > maxN then
                result
            elif Set.contains i seen then
                // skip
                getLongestChain' seen result (i+1)
            else
                // look for chain from i
                let rec getLongestChainFrom cur index chainMap chainList = 
                    if Map.containsKey cur chainMap then
                        // cycle found
                        let indexCycleStart = Map.find cur chainMap
                        let cycle = chainList |> List.rev |> List.skip indexCycleStart
                        let cycleResult = (cycle |> List.length, cycle |> List.min)
                        (chainMap, Some(cycleResult))
                    elif cur <= 1 || cur > maxN || Set.contains cur seen then
                        // no chain found, return None
                        (chainMap, None)
                    else 
                        getLongestChainFrom (getNext cur) (index+1) (Map.add cur index chainMap) (cur :: chainList)
                
                let (chainMap,cycleResult) = getLongestChainFrom i 0 Map.empty List.empty
                let newResult = getMax result cycleResult
                let added = chainMap |> Seq.map (fun kvp -> kvp.Key) |> Set.ofSeq
                getLongestChain' (Set.union seen added) newResult (i+1)
            
        getLongestChain' Set.empty None 2

    let solve() = 
        let n = sc.NextInt().Value
        //for i in 2..n do
        //    getNext i |> ignore
        let allChains = getAllChains 1000000 |> Seq.toArray

        let cycleResult = getLongestChain n
        printfn "%d" (cycleResult |> Option.get |> snd)
        ()
        