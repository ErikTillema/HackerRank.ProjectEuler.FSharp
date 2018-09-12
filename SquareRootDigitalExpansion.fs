namespace HackerRank.FSharp

module SquareRootDigitalExpansion =

    open Util
    open System
    open System.Collections.Generic
    open System.Numerics

    let sc = Scanner()

    // We can speed things up by using sqrt 8 = 2 sqrt 2, etc.
    // that way, we should somewhat reduce the number of sqrts to calculate.
    // Unfortunately, that only reduces the number of sqrts to calculate by 1/3.
    // But we could also exploit sqrt 21 = sqrt 3 * sqrt 7. Multiply the two rationals (add some extra digits??) and we have the result
    // So then we only calculate all the sqrt p where p is prime. There are 168 primes <= 1000, 25 primes <= 100

    let isPrime (n: int) = 
        let isDivisor i = n % i = 0
        let sqrt n = n |> float |> sqrt |> floor |> int
        if n <= 1 then false
        else
            // NB: using a sequence here instead of a list makes a huge performance difference for big n
            let hasDivisor = seq { 2..sqrt n } |> Seq.exists (fun i -> isDivisor i) // Seq.exists is lazy 
            not hasDivisor

    let getPrimeFactors n =     
        let rec getPrimeFactors' acc last n = 
            if n = 1 then acc |> List.rev
            else
                let mutable f = max 2 last
                while n%f <> 0 && (f+1)*(f+1) <= n do f <- f + 1
                if n%f <> 0 then f <- n

                if f <> last then
                    getPrimeFactors' ((f,1)::acc) f (n/f)
                else
                    let (f,m)::xs = acc
                    getPrimeFactors' ((f,m+1)::xs) f (n/f)
        getPrimeFactors' [] -1 n
    
    let isSquare a = 
        let s = a |> float |> Math.Sqrt |> floor |> int
        s*s = a

    let sumDigits digits (a: bigint) =
        a.ToString()
            |> Seq.take digits
            |> Seq.sumBy (fun c -> (int c) - (int '0')) 
    
    // Newton-Raphson, aka the Babylonian method for square roots.
    let sqrt3 digits (n: int) = 
        let nn = (n |> bigint) * BigInteger.Pow(10I, 2*digits)
        let start = (Math.Sqrt (float n)).ToString("G17").Replace(".", "").Replace(",", "")
        let l = start.Length
        let startt = (int(Math.Sqrt (float n))).ToString()
        let l2 = startt.Length
        let poww = BigInteger.Pow(10I, digits - (l-l2))
        let start2 = BigInteger.Parse(start) * poww
        let mutable x = start2 + 10I*poww // due to lack of accuracy in the last digit of Math.Sqrt, we need to add 10 times more here.
        let mutable y = start2 - 10I*poww
        let e = 10I // e decides the accuracy level
        while abs (x - y) > e do
            x <- (x + y)/2I
            y <- nn/x
        x

    let rec sqrtSmart = 
        let cache = Dictionary<int,bigint>()
        (fun digits a ->
            if cache.ContainsKey(a) then
                cache.[a]
            else
                let result = 
                    if isSquare a then   Math.Sqrt(float a) |> bigint
                    elif isPrime a then  sqrt3 digits a
                    else
                        let fs = getPrimeFactors a 
                        let f = 
                            if fs |> List.exists (fun (_,c) -> c >= 2) then
                                let ff = fs |> List.filter (fun (_,c) -> c >= 2) |> List.head |> fst
                                ff*ff
                            else 
                                fs |> List.head |> fst
                        let r1 = sqrtSmart digits f
                        let r2 = sqrtSmart digits (a/f)
                        r1 * r2 //|> truncate digits // for some reason, truncating only slows down and doesn't gain.
                cache.Add(a, result)
                result
        )

    let solve() = 
        let n = sc.NextInt().Value
        let digits = sc.NextInt().Value
        let extraDigits = 10

        seq { 2..n } 
            |> Seq.filter (isSquare >> not) 
            |> Seq.map (sqrtSmart (digits + extraDigits))
            |> Seq.map (sumDigits digits) 
            |> Seq.sum |> printfn "%d"
        ()

    // digit per digit calculation of the square root
    // the "pen-and-paper" "long division" method
    // Slower than Newton-Raphson
    let sqrt digits a = 
        let rec sqrt' acc don p rem =
            if don = digits then
                acc
            else
                let pa = match don, a with
                         | 0, a when a >= 100 -> a/100
                         | 1, a when a >= 100 -> a%100
                         | 0, a -> a
                         | _ -> 0
                let c = 100I*rem + (bigint pa)
                let mutable x = 0I
                let _20p = 20I*p
                if p = 0I then
                    x <- Math.Sqrt(float c) |> floor |> bigint
                else
                    x <- c/(_20p)
                    while x*(_20p+x) > c do  x <- x - 1I // -1 should happen only like max 3 times
                    let _xp1 = x+1I
                    if (_xp1)*(_20p+_xp1) <= c then x <- _xp1 // +1 apparently happens only max once
                let y = x*(_20p+x)
                sqrt' (10I*acc+x) (don+1) (10I*p+x) (c-y)
        sqrt' 0I 0 0I 0I
    
    // Some other by digit calculation of the square root.
    // Does this even work? With the right number of steps?
    let sqrt2 digits (x: int) = 
        let mutable a = 1I
        let mutable b = 1I
        let steps = digits |> float |> Math.Log |> floor |> int
        let x = bigint x
        for _ in 1..steps+2 do
            let tmp = a
            a <- a*a+b*b*x
            b <- 2I*tmp*b
        seq {
            while true do
                let div,rem = BigInteger.DivRem(a,b)
                if div < 10I then
                    yield div |> int
                else
                    yield div/10I |> int
                    yield div%10I |> int
                a <- 10I*rem
        } |> Seq.take digits |> Seq.toList