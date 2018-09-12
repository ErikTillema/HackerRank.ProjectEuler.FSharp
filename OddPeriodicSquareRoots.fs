namespace HackerRank.FSharp

module OddPeriodicSquareRoots =

    open Util

    let sc = Scanner()

    /// Returns a modulo m
    let inline modulo m a = 
        if a >= LanguagePrimitives.GenericZero then 
            a%m 
        else 
            ((a%m)+m)%m // avoid overflow by adding m only if a%m is negative

    let getPeriod n = 
        let getNext x y = 
            let s = n - y*y
            let ynew = s/x
            let xnew = modulo ynew (-y)
            let (fn, fxnew, fynew) = (float n, float xnew, float ynew)
            let xnew = xnew + ((sqrt fn - fxnew)/fynew |> floor |> int) * ynew
            ( ynew, xnew )
        let a0 = n |> float |> sqrt |> floor |> int
        
        // make sequence of values, including index and a map containing all accumulated previous sequence values
        let generator (index, (a,b), map_previous) = Some( (index, (a,b), map_previous), 
                                                           (index+1, getNext a b, Map.add (a,b) index map_previous) )
        let values = Seq.unfold generator (0, (1,a0), Map.empty)
        let (index,key,map) = values |> Seq.find (fun (_,key,map) -> Map.containsKey key map)
        index - (Map.find key map)

    let isSquare n = 
        let r = n |> float |> sqrt |> floor |> int
        r*r = n

    let solve() = 
        let n = sc.NextInt().Value
        let result = seq {1..n} |> Seq.filter (not << isSquare) |> Seq.map getPeriod |> Seq.filter (fun p -> p%2=1) |> Seq.length 
        printfn "%d" result
        ()
