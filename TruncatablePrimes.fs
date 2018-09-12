namespace HackerRank.FSharp

module TruncatablePrimes =

    open Util
    open System

    let sc = Scanner()

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
        } |> Set

    let isPrime (primes: Set<int>) p = 
        p |> primes.Contains

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

    let isTruncatablePrime primes p = 
        let digits = getDigits p
        let n = digits.Length
        let truncatedNumbers = 
            seq {
                for i in 1..n-1 do
                    yield digits |> List.skip(i) |> getNumber
                for i in 1..n-1 do
                    yield digits |> List.take(n-i) |> getNumber
            }
        truncatedNumbers |> Seq.forall (isPrime primes)

    let solve() = 
        let n = sc.NextInt().Value - 1
        let primes = primesUntil n
        let result = primes |> Set.toList |> List.skip 4 |> List.filter (isTruncatablePrime primes) |> List.sum
        printfn "%i"result
