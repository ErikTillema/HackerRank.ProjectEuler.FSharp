namespace HackerRank.FSharp

module ConsecutivePrimeSum =

    open Util
    open System.Numerics
    open System.Collections.Generic

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

    let N = 6_000_000
    let primes = primesUntil N
    let primesSet = HashSet(primes)
    let primesAcc = 
        let n = primes.Length
        let result = Array.replicate n 0L
        result.[0] <- int64 primes.[0]
        for i in 1..n-1 do
            result.[i] <- (int64 primes.[i]) + result.[i-1]
        //printfn "%d" result.[n-1]
        result

    /// Returns whether or not n is a prime number
    /// O(sqrt(n))
    let isPrime (a:int64) = 
        if a < (int64 N) then
            primesSet.Contains (int a)
        else
            let isDivisor i = a % i = 0L
            let sqrt n = floor(sqrt (float n)) |> int64
            if a <= 1L then false
            else
                let hasDivisor = seq {2L..sqrt a} |> Seq.exists (fun i -> isDivisor i)
                not hasDivisor

    let findPrimeSum l = 
        if l=1 then // not sure why this function doesn't work for l=1, so special case here...
            Some(1,2L,2,2)
        elif l%2=1 then
            let n = primes.Length
            let getIsOk i = isPrime (primesAcc.[i+l] - primesAcc.[i])
            let firstOk = seq { 0 .. n-1-l } |> Seq.find getIsOk
            Some(l, primesAcc.[firstOk+l] - primesAcc.[firstOk], primes.[firstOk+1], primes.[firstOk+l])
        else 
            // start at 2
            if isPrime (primesAcc.[l-1]) then
                Some(l, primesAcc.[l-1], 2, primes.[l-1])
            else 
                None
    
    let solveOne() = 
        let n = sc.NextLong().Value 
        //for n in 3 .. 2 .. 15 do
        //    let (l,p,pstart,pend) = findPrimeSum n
        //    printfn "%d, %d = %d .. %d" l p pstart pend

        // so tactics:
        // find biggest l possible in theory for input (binary search in primesAcc)
        // then keep looking at smaller lengths and find first prime sum <= input
        // 7, 197 = 17 .. 41 this is the first one longer than 6, which is the longest one containing 2 and being a even length sequence.
        // 9, 127 = ... even better!

        // it's a mistake to always search for sequences of odd length
        // because the sequence starting at 2 can also result in a prime. And that sequence is of even length
        let maxl = (seq { primesAcc.Length-1 .. -1 .. 0 } |> Seq.find (fun i -> primesAcc.[i] <= n)) + 1 // binary search would be faster
        let (l,p,_,_) = 
            seq { maxl .. -1 .. 1 }
            |> Seq.map findPrimeSum
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
            |> Seq.find (fun (_,p,_,_) -> p <= n)
        printfn "%d %d" p l
        ()

    let solve() = 
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()
        ()