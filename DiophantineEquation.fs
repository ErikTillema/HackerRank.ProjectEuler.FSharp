namespace HackerRank.FSharp

module DiophantineEquation =

    open System
    open Util
    open System.Numerics

    /// Returns a generating function for the sequence.
    /// Use generator by simply calling the function with empty set of parameters ().
    /// Opposite of SeqExt.unfold2: mySequence |> SeqExt.getGenarator |> SeqExt.unfold2 equals mySequence
    let getGenerator (sequence: 'a seq) = 
        let enumerator = sequence.GetEnumerator()
        fun () -> 
            match enumerator.MoveNext() with
            | true -> Some(enumerator.Current)
            | false -> None

    /// Like Seq.unfold, but doesn't require state
    /// This function uses the generator to unfold a sequence, continuing while generator returns Some(element)
    /// and stopping when it returns None.
    /// Opposite of SeqExt.getGenerator: mySequence |> SeqExt.getGenarator |> SeqExt.unfold2 equals mySequence
    let unfold2 generator =
        let g dummyState = 
            match generator() with
            | Some(element) -> Some(element, dummyState)
            | None -> None
        Seq.unfold g 0

    /// Returns source sequence split at index n.
    /// Returns the head as a list, because we need to iterate the first n elements anyway (in case head is never iterated for example).
    /// For example, splitAt 3 (seq{1..10}) = ( [1..3] , seq{4..10} )
    /// Throws a InvalidOperationException if n is greater than source.length
    let splitAt n (source: 'a seq) = 
        let generator = source |> getGenerator
        let head = List.init n  (fun _ ->   match generator() with
                                            | Some(v) -> v
                                            | None -> invalidOp "source does not contain n elements"
                                )
        let tail = unfold2 generator
        (head, tail)

    type Fraction = { Numerator: bigint; Denominator: bigint }

    let createFraction n = { Numerator = n; Denominator = 1I }
    let createFraction2 (n,d) = 
        let gcd = bigint.GreatestCommonDivisor(n,d)
        { Numerator = n/gcd; Denominator = d/gcd }
    let invertFraction { Numerator = n; Denominator = d } = { Numerator = d; Denominator = n }
    let addFraction { Numerator = n1; Denominator = d1 } { Numerator = n2; Denominator = d2 } = createFraction2 (n1*d2+n2*d1, d1*d2)

    let sc = Scanner()

    let isSquare a = 
        let r = a |> float |> sqrt |> floor |> int
        r*r = a

    /// Returns a modulo m
    let inline modulo m a = 
        if a >= LanguagePrimitives.GenericZero then 
            a%m 
        else 
            ((a%m)+m)%m // avoid overflow by adding m only if a%m is negative
    
    /// Returns sqrt n as continued fractions in the form of a non-repeating part (the head) and a repeating part (the repetition).
    /// Let sqrt n = a0 + 1/(a1 + 1/(a2 + 1/(a3 + ...)))
    /// Then {a_n} will have a repeating part at a certain point.
    /// For example for sqrt 2, a_n = { 1, 2, 2, 2, 2, ... } = [1;(2)]
    /// Or, for sqrt 23, a_n = { 4, 1, 3, 1, 8, 1, 3, 1, 8, ...} = [4;(1,3,1,8)]
    /// Returns a tuple of lists, the non-repeating part and the repeating part.
    /// O(l log l) where l is the total length of the non-repeating plus repeating part.
    let getContinuedFractionsWithRepetition n = 
        let getNext x y = 
            let s = n - y*y
            let ynew = s/x
            let xnew = modulo ynew -y
            let (fn, fxnew, fynew) = (float n, float xnew, float ynew)
            let xnew = xnew + ((sqrt fn - fxnew)/fynew |> floor |> int) * ynew
            let an = x*(y+xnew)/s
            (an, ynew, xnew)
        let a0 = n |> float |> sqrt |> floor |> int
        
        // make sequence of values, including index and a map containing all accumulated previous sequence values
        let generator (index, (an,x,y), map_previous) = Some( (index, (an,x,y), map_previous), 
                                                              (index+1, getNext x y, Map.add (x,y) index map_previous) )
        let values = Seq.unfold generator (0, (a0,1,a0), Map.empty) |> Seq.cache
        let (indexRepetitionEnd,(_,x,y),map) = values |> Seq.find (fun (_,(_,x,y),map) -> Map.containsKey (x,y) map)
        let indexRepetitionStart = 1 + Map.find (x,y) map
        let list = values |> Seq.take (1+indexRepetitionEnd) |> Seq.toList
        list |> List.map (fun (_,(an,_,_),_) -> an) |> List.splitAt indexRepetitionStart
    
    /// Returns sqrt n as continued fractions as an infinite sequence.
    /// Let sqrt n = a0 + 1/(a1 + 1/(a2 + 1/(a3 + ...)))
    /// For example for sqrt 2, a_n = { 1, 2, 2, 2, 2, ... }
    /// Or, for sqrt 23, a_n = { 4, 1, 3, 1, 8, 1, 3, 1, 8, ...}
    /// O(1) per step   @@@ make this method faster by not using getContinuedFractionsWithRepetition first, but using getNext directly. 
    let getContinuedFractions n = 
        /// Converts the continued list [4;(1,3,1,8)] into a sequence {4,1,3,1,8,1,3,1,8,...}
        let getContinuedListAsSeq continuedList = 
            Seq.append (fst continuedList) (Seq.initInfinite id |> Seq.collect (fun _ -> snd continuedList)) 
        n |> getContinuedFractionsWithRepetition |> getContinuedListAsSeq

    /// Uses the continued fractions of sqrt n to get the so-called convergents (rational numbers, converging to an irrational number).
    /// See https://en.wikipedia.org/wiki/Continued_fraction#Infinite_continued_fractions_and_convergents
    /// Uses a recursive formula to determine the next convergent numerator and denominator from the previous two convergents.
    /// O(1) per step
    let getConvergents n = 
        seq {
            let fractionValues = n |> getContinuedFractions |> Seq.map bigint
            let [a0;a1], tail = splitAt 2 fractionValues
            let lastResults = [| (a0, 1I); (a0*a1+1I, a1) |]
            yield lastResults.[0] |> createFraction2
            yield lastResults.[1] |> createFraction2
            for an in tail do
                let tmp = lastResults.[1]
                lastResults.[1] <- ( an * (fst lastResults.[1]) + (fst lastResults.[0]) , an * (snd lastResults.[1]) + (snd lastResults.[0]) )
                lastResults.[0] <- tmp
                yield lastResults.[1] |> createFraction2
        }

    /// From wikipedia: the minimal solution in x (x,y) of x^2 - n*y^2 = 1
    /// must be one of the convergents x/y of the (regular) continued fractions of sqrt n
    /// https://en.wikipedia.org/wiki/Pell%27s_equation
    let solveDiophantine (n: int) = 
        let isSolution x y = x*x - (bigint n)*y*y=1I
        n   |> getConvergents 
            |> Seq.map (fun {Numerator=n; Denominator=d} -> (n,d)) 
            |> Seq.find (fun (n,d) -> isSolution n d)

    let solve_precalculated() = 
        let getAllSolutions() = 
            let mutable maxX = 0I
            for d in ([1..10000] |> Seq.filter (not << isSquare)) do
                let (x,y) = solveDiophantine d
                if x > maxX then
                    maxX <- max maxX x
                    printf "%d; " d
        //getAllSolutions()
        let solutions = [5; 10; 13; 29; 46; 53; 61; 109; 181; 277; 397; 409; 421; 541; 661; 1021; 1069; 1381; 1549; 1621; 2389; 3061; 3469; 4621; 4789; 4909; 5581; 6301; 6829; 8269; 8941; 9949; ]
        let n = sc.NextInt().Value
        let result = solutions |> Seq.takeWhile (fun i -> i <=n) |> Seq.last
        printfn "%d" result
    
    let solve() = 
        let testContinuedFractions() = 
            for i in [2;3;5;6;7;8;10;11;12;13;23] do 
                let bla = i |> getContinuedFractionsWithRepetition
                let bla = i |> getContinuedFractions |> Seq.take 20 |> Seq.toList
                let bla = i |> getConvergents |> Seq.take 10 |> Seq.toList
                printfn "%A" bla
        //testContinuedFractions()
        let n = sc.NextInt().Value
        let result = seq { 1..n } |> Seq.filter (not << isSquare) |> Seq.maxBy (solveDiophantine >> fst)
        printfn "%d" result
        ()

    //let solveDiophantine_slow d = 
    //    let y = Seq.initInfinite (fun i -> i+1) |> Seq.map bigint |> Seq.find (fun y -> 1I+d*y*y |> isSquare)
    //    let x = 1I+d*y*y |> float |> sqrt |> bigint
    //    (x,y)

