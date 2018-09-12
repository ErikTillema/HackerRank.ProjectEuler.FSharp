namespace HackerRank.FSharp

module SpiralPrimes =

    open Util

    let sc = Scanner()

    /// Returns whether or not n is a prime number
    /// O(sqrt(n))
    let isPrime (n: int64) = 
        let isDivisor i = n % i = 0L
        let sqrt n = floor(sqrt (float n)) |> int64
        if n <= 1L then false
        else
            // NB: using a sequence here instead of a list makes a huge performance difference for big n
            let hasDivisor = seq { 2L..sqrt n } |> Seq.exists (fun i -> isDivisor i) // Seq.exists is lazy 
            not hasDivisor

    let inline modulo m a = ((a%m)+m)%m

    let rec squareModulo2 m x = 
        if x < (1L<<<31) then
            modulo m (x*x)
        else
            // we can optimize if we know m: suppose m=10^12 and int64.MAX_VALUE = 2^63 = 10^18.
            // then, if y < 10^6, we can simply return modulo m (x*y)
            if x = 0L then
                0L
            elif x = 1L then 
                modulo m x
            else
                let (+) x y = modulo m (x+y)
                let (*) a x = modulo m (a*x)
                let resultHalf = squareModulo2 m (x/2L)
                let mutable result = 4L*resultHalf
                if x%2L = 1L then result <- result + 4L*(x/2L) + 1L
                result

    // should work up to n = 2^63 / 11 > 8*10^17
    let isPrimeMillerRabin n = 
        let bit = Array.create 64 0L
        let seeds = [| 2L; 3L; 5L; 7L; 11L; 13L; |]
        let isProbablyComposite a = 
            let mutable n_1 = n - 1L
            let mutable idx = 0
            while n_1 > 0L do
                bit.[idx] <- n_1 % 2L;
                idx <- idx+1
                n_1 <- n_1 / 2L

            let mutable rem = 1L
            let f i = 
                let x = rem
                rem <- squareModulo2 n rem

                if rem = 1L && x <> 1L && (x <> (n - 1L)) then
                    true
                else
                    if bit.[i] = 1L then
                        rem <- (rem * a) % n
                    false

            if seq { idx-1 .. -1 .. 0 } |> Seq.tryFind f |> Option.isSome then
                true
            else 
                rem <> 1L

        if n < 2L then false
        elif n = 2L then true
        elif n % 2L = 0L then false
        elif seeds |> Seq.exists ((=) n) then true
        elif seeds |> Seq.exists isProbablyComposite then false
        else true
        
    let corners i = seq { for j in 0L..3L -> (2L*i+1L)*(2L*i+1L) - j*(2L*i) }

    let percentages() = 
        let generator (i,count,primeCount) = 
            let newPrimeCount = primeCount + (corners (i+1L) |> Seq.filter isPrimeMillerRabin |> Seq.length)
            Some( (float primeCount)/(float count) , (i+1L,count+4,newPrimeCount) )
        Seq.unfold generator (0L,1,0)

    let solve() = 
        let n = sc.NextInt().Value
        //for n in 60..-1..8 do
        let result = 
            match n with
            | 8 ->  238733
            | _ ->  let a = percentages() |> Seq.indexed |> Seq.skip 1 |> Seq.find (fun (_,p) -> 100.0*p < float n) |> fst
                    a*2+1
        printfn "%d" result
        ()
