namespace HackerRank.FSharp

module CyclicalFigurateNumbers =

    open Util

    let sc = Scanner()

    let getPermutations (source: 'a seq) = 
        let source = source |> Seq.toArray
        let n = source.Length
        let result = Array.zeroCreate n
        let used = Array.create n false
        let rec getPermutations' don =
            seq {
                if don = n then
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

    let rec countDigits a = 
        match a with
        | 0 -> 0
        | _ -> 1 + countDigits (a/10)

    let polygonals i = 
        match i with
        | 3 -> Seq.initInfinite (fun j -> j * (j+1) / 2)   |> Seq.skip 1
        | 4 -> Seq.initInfinite (fun j -> j * j)           |> Seq.skip 1
        | 5 -> Seq.initInfinite (fun j -> j * (3*j-1) / 2) |> Seq.skip 1
        | 6 -> Seq.initInfinite (fun j -> j * (2*j-1))     |> Seq.skip 1
        | 7 -> Seq.initInfinite (fun j -> j * (5*j-3) / 2) |> Seq.skip 1
        | 8 -> Seq.initInfinite (fun j -> j * (3*j-2))     |> Seq.skip 1
        | _ -> invalidOp "bad polygonal"
    
    let getHead a = a / 100
    let getTail a = a % 100
    let allDistinct values = 
        (List.length values) = (values |> List.distinct |> List.length)

    let rec getTuples (polygonals: Map<int,int list>[]) don n chosenValues =
        seq {
            if don = n then
                if getTail (List.head chosenValues) = getHead (List.last chosenValues) && (allDistinct chosenValues) then
                    yield chosenValues
            else
                match chosenValues with
                | [] ->     // choose any
                            for value in polygonals.[don] |> Seq.collect (fun kvp -> kvp.Value) do
                                yield! getTuples polygonals (don+1) n (value::chosenValues)
                | x::xs ->  let tail = getTail x
                            match Map.tryFind tail polygonals.[don] with
                            | None -> ()
                            | Some(values) ->   for value in values do
                                                    yield! getTuples polygonals (don+1) n (value::chosenValues)
        }

    let solve() = 
        let n = sc.NextInt().Value
        let ns = Array.init n (fun _ -> sc.NextInt().Value)
        let fourDigitPolygonals = Array.init n (fun i -> polygonals ns.[i] 
                                                            |> Seq.skipWhile (fun a -> countDigits a < 4) 
                                                            |> Seq.takeWhile (fun a -> countDigits a = 4) 
                                                            |> Seq.groupBy getHead
                                                            |> Seq.map (fun (key,value) -> (key,value |> Seq.toList))
                                                            |> Map.ofSeq)
    
        let results = fourDigitPolygonals |> getPermutations |> Seq.collect (fun permutation -> getTuples permutation 0 n [])
        let result = results |> Seq.map (List.sum) |> Seq.distinct |> Seq.sort |> Seq.map string |> String.concat "\n"
        printfn "%s" result
        ()
