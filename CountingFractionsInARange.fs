namespace HackerRank.FSharp

module CountingFractionsInARange =

    open Util
    open System.Collections.Generic

    let sc = Scanner()

    let getMedian (a,b) (c,d) = (a+c,b+d)
    let (<?) (a,b) (c,d) = a*d < b*c

    /// Generates the Farey sequence of order n (so fractions with denominator <= n, including 0/1 and 1/1).
    let farey n =
        seq {
            let mutable a, b, c, d = 0L, 1L, 1L, n
            yield (a, b)
            while c <= n do
                let k = (n + b) / d
                let na, nb, nc, nd = c, d, (k*c-a), (k*d-b)
                a <- na
                b <- nb
                c <- nc
                d <- nd
                yield (a, b)
        }
    
    /// We can use the tree once again, traversing the tree and counting all values which have D <= N
    let rec countFractionsDfs maxD left middle right = 
        let (n,d) = middle
        if d <= maxD then
            1L + (countFractionsDfs maxD left (getMedian left middle) middle) + (countFractionsDfs maxD middle (getMedian middle right) right)
        else
            0L
      
    let countFractionsBfs maxD left right = 
        let q = Queue()
        let middle = getMedian left right
        let mutable result = 0L
        q.Enqueue((left,middle,right))
        while q.Count > 0 do
            let left,middle,right = q.Dequeue()
            let (n,d) = middle
            if d <= maxD then
                result <- result + 1L
                q.Enqueue((left,getMedian left middle, middle))
                q.Enqueue((middle, getMedian middle right, right))
        result

    /// We will solve this problem by using the algorithm halfway the pdf:
    /// for x in R, we define rank(x) = number of reduced fractions p/q < x where q <= n.
    /// Then we define Aq = number of reduced fractions p/q < x with denominator q.
    /// So rank(x) = sum_q_1..n Aq
    /// After we see that { fractions in [0,x) with denominator q } = { reduced fractions in [0,x) with denominator d where d|q } = { reduced fractions p/d < x where d|q }
    /// So                floor x*q                                 = sum_d_1..q d|q Ad = Aq + sum_d_1..q-1 d|q Ad
    /// So Aq = floor x*q - sum_d_1..q-1 d|q Ad
    /// So we can solve Aq by dynamic programming and using the lower values Ad.
    /// Finally, we can calculate rank(x) from the Aqs and solve our problem by taking rank(1/A) - rank(1/(A+1)) - 1
    /// O(n log n)
    /// See http://euler.stephan-brumme.com/73/ and http://people.csail.mit.edu/mip/papers/farey/talk.pdf
    let rank n p q = 
        let Aq = Array.init (n+1) (fun i -> (int64 i)*p/q)
        for d in 1..n do
            // subtract Aq.[d] from Aq.[q] if d<q and d|q, so for q=2d, 3d, 4d, ...
            for q in 2*d..d..n do   
                Aq.[q] <- Aq.[q] - Aq.[d]
        seq { 0..n } |> Seq.sumBy (fun i -> Aq.[i])

    let solve() = 
        let A = sc.NextInt().Value |> int64
        let D = sc.NextInt().Value
        let result = (rank D 1L A) - (rank D 1L (A+1L)) - 1L
        printfn "%d" result
        ()

    let solve_stern_brocot_tree_traversal_too_slow() = 
        let A = sc.NextLong().Value
        let D = sc.NextLong().Value
        let left = (1L,A+1L)
        let right = (1L,A) 
        let result = countFractionsDfs D left (getMedian left right) right // results in stack overflow on my computer, but time-out on HackerRank
        //let result = countFractionsBfs D left right // results in memory overflow on my computer and errors and time-outs on HackerRank
        printfn "%d" result
        ()

    let solve_farey_sequence_too_slow() = 
        let A = sc.NextLong().Value
        let D = sc.NextLong().Value
        //let temp = farey D |> Seq.toArray
        let result = farey D |> Seq.takeWhile (fun fraction -> fraction <? (1L,A)) |> Seq.filter (fun fraction -> (1L,A+1L) <? fraction) |> Seq.length 
        printfn "%d" result
        ()
