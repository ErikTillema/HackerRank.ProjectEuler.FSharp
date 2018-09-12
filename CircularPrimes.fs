namespace HackerRank.FSharp

module CircularPrimes =

    open Util
    open System

    let sc = Scanner()

    /// Returns the digits in n (base 10) in order
    /// For example, returns [ 1; 2 ] for n=12.
    /// O(log n)
    let getDigits n = 
        let rec getDigits' acc n = 
            if n = 0 then 
                acc
            else
                let d = n%10
                getDigits' (d::acc) (n/10)

        if n = 0 then [0]
        else getDigits' [] (abs(n))

    let getNumber (digits: int list) = 
        let rec getNumber' acc (digits: int list) = 
            match digits with
            | [] -> acc
            | x::xs -> getNumber' (10 * acc + x) xs
        getNumber' 0 digits

    /// returns all rotations of source (as lists)
    let getRotations (source: 'a seq) = 
        let source = source |> Seq.toList
        let n = source.Length
        seq {
            if n = 0 then
                yield []
            else
                for i in 0..n-1 do
                    yield (source |> List.skip i |> List.take (n-i)) @ (source |> List.take i)
        }

    /// Returns all primes <= n in order.
    /// Uses Sieve of Eratosthenes.
    /// O(n log log n)
    /// Because SUM_(p <= sqrt n) { n/p } = O(n log log n)
    let primesUntil n =
        let sqrt n = Math.Floor(Math.Sqrt (n |> float)) |> int
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

    let isPrime n = 
        let sqrt n = Math.Floor(Math.Sqrt (n |> float)) |> int
        if n <= 1 then false
        else
            let mutable result = true
            for i in 2..sqrt n do
                if n % i = 0 then result <- false
            result

    let isOk n p (primes: Set<_>) = 
        let isPrimee p = 
            if p <= n then primes.Contains(p)
            else isPrime p
        let digits = getDigits p
        //digits |> getRotations |> Seq.iter (fun newDigits -> newDigits |> getNumber |> printf "%i " )
        digits |> getRotations |> Seq.forall (fun newDigits -> newDigits |> getNumber |> isPrimee )

    let solve() = 
        let n = sc.NextInt().Value - 1
        let primes = primesUntil n |> Set
        let mutable result = 0L
        for p in primes do
            //printf "try %i :" p
            if isOk n p primes then 
                //printfn " YES"
                //printfn "%i" p
                result <- result + (p |> int64)
            else
                () //printfn " NO"
        printfn "%i" result
