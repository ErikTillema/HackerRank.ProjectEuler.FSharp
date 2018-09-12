namespace HackerRank.FSharp

module PasscodeDerivation =

    open Util
    open System.Collections.Generic
    open System

    let sc = Scanner()

    let unfold2 generator =
        let g dummyState = 
            match generator() with
            | Some(element) -> Some(element, dummyState)
            | None -> None
        Seq.unfold g 0

    /// one can look at this problem as being a graph problem, in which each letter is a Node
    /// and "letter X has to occur before letter Y in the passcode" is a directed edge from X to Y. 
    /// You can then repeatedly take the lexicographically smallest letter with in-degree zero (this will be the next letter in the passcode) 
    /// and remove it from the graph. If there is no letter with in-degree zero at a certain point, then something is wrong. 
    /// By using the right data structures, you can do this in O(E log E) where E is the number of edges in the graph.
    let solve() = 
        let n = sc.NextInt().Value
        let edges = Dictionary<char,char list>() // map from -> toos : char -> char list, the outgoing edges of from
        let inDegrees = Dictionary<char,int>() // map node -> inDegree: char -> int
        let addNodeIfNecessary c = 
            if not(edges.ContainsKey(c)) then
                edges.Add(c, [])
                inDegrees.Add(c, 0)

        // build graph
        for _ in 1..n do
            let chars = sc.Next().Value.ToCharArray()
            let newEdges = [ (chars.[0], chars.[1]); (chars.[1], chars.[2]) ]
            for edge in newEdges do
                let from,too = edge
                addNodeIfNecessary from
                addNodeIfNecessary too
                inDegrees.[too] <- inDegrees.[too] + 1
                edges.[from] <- too :: edges.[from]

        let set = SortedSet<int*char>() // nodes sorted by (inDegree,char)
        for c in edges.Keys do
            set.Add((inDegrees.[c], c)) |> ignore

        let getNextLetter() = 
            if set.Count = 0 then
                None // we are done
            else
                let head = set.Min
                set.Remove(head) |> ignore // remove from graph
                if inDegrees.[snd head] <> 0 then
                    None // something is wrong
                else
                    // update in-degrees of nodes with edges from head.
                    for too in edges.[snd head] do
                        let oldToo = (inDegrees.[too], too)
                        set.Remove(oldToo) |> ignore
                        inDegrees.[too] <- inDegrees.[too] - 1
                        let newToo = (inDegrees.[too], too)
                        set.Add(newToo) |> ignore
                    Some(snd head)
        
        let allLetters = unfold2 getNextLetter |> Seq.toArray |> String
        if allLetters.Length = edges.Count then
            printfn "%s" allLetters
        else
            printfn "SMTH WRONG"
        ()
