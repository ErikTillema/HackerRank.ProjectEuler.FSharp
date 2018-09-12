namespace HackerRank.FSharp

module PrimePairSets =

    open Util
    open System.Collections.Generic

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

    /// Returns a modulo m
    let inline modulo m a = 
        if a >= LanguagePrimitives.GenericZero then 
            a%m 
        else 
            ((a%m)+m)%m // avoid overflow by adding m only if a%m is negative

    let multiplyModulo (m: int) (x: int) (y: int) = 
        let x = modulo m x |> int64 // avoid overflow by using int64
        let y = modulo m y |> int64
        (x * y) |> modulo (m |> int64) |> int

    /// Returns whether or not n is a prime number, using the probabilistic Miller-Rabin test.
    /// Faster than isPrime for large numbers.
    let isPrimeMillerRabin (n: int) = 
        let bit = Array.create 32 0
        let seeds = [| 2; 7; 61 |]
        let isProbablyComposite a = 
            let mutable n_1 = n - 1
            let mutable idx = 0
            while n_1 > 0 do
                bit.[idx] <- n_1 % 2
                idx <- idx+1
                n_1 <- n_1 / 2

            let mutable rem = 1
            let isComposite i = 
                let x = rem
                rem <- multiplyModulo n rem rem
                if rem = 1 && x <> 1 && (x <> (n - 1)) then
                    true
                else
                    if bit.[i] = 1 then rem <- multiplyModulo n rem a
                    false

            if seq { idx-1 .. -1 .. 0 } |> Seq.tryFind isComposite |> Option.isSome then
                true
            else 
                rem <> 1

        if n < 2 then false
        elif n = 2 || n = 3 || n = 5 || n = 7 then true
        elif n % 2 = 0 || n % 3 = 0 || n % 5 = 0 || n % 7 = 0 then false
        elif seeds |> Seq.exists ((=) n) then true
        elif seeds |> Seq.exists isProbablyComposite then false
        else true

    /// Returns all primes <= n in order.
    /// Uses Sieve of Eratosthenes.
    /// O(n log log n)
    /// Because SUM_(p <= sqrt n) { n/p } = O(n log log n)
    let primesUntil (n: int) =
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
        } |> Seq.toArray

    let sc = Scanner()

    let solve() = 
        let N = sc.NextInt().Value
        let K = sc.NextInt().Value
        let primes = primesUntil N
        let n = primes.Length

        let isPrime = 
            let Q = 1_000_000
            let primes = primesUntil Q
            let primesSet = HashSet(primes)
            fun a -> 
                if a <= Q then primesSet.Contains(a)
                else isPrimeMillerRabin a

        let rec countDigits a = 
            if a = 0 then 0
            else 1 + countDigits (a/10)

        let primeDigits = primes |> Array.map (countDigits >> pow 10)

        let areCompatible i j = 
            isPrime (primes.[i]*primeDigits.[j] + primes.[j]) && isPrime (primes.[j]*primeDigits.[i] + primes.[i])

        let getCompatibles = 
            let cache = Dictionary<int,Set<int>>()
            (fun i ->
                if cache.ContainsKey(i) then 
                    cache.[i]
                else 
                    let result = seq { i+1..n-1 } |> Seq.filter (fun j -> areCompatible i j) |> Set.ofSeq
                    cache.[i] <- result
                    result
            )
        
        let rec getSetSums don primesTaken compatiblesLeft = 
            seq {
                if don = K then 
                    yield primesTaken |> List.map (Array.get primes)
                elif Set.count compatiblesLeft = 0 then
                    ()
                else
                    // choose a prime from compatiblesLeft
                    for p in compatiblesLeft do
                        yield! getSetSums (don+1) (p::primesTaken) (Set.intersect compatiblesLeft (getCompatibles p))
            }

        let results = getSetSums 0 [] ([0..n-1] |> Set.ofList) |> Seq.toList |> List.sortBy (List.sum)
        let result = results |> List.map (List.sum >> string) |> String.concat "\n"
        printfn "%s" result
        ()
