namespace HackerRank.FSharp

module SquareDigitChains =

    open Util
    open System
    open System.Collections.Generic
    open System.Numerics
    
    let sc = Scanner()
    let m = 1_000_000_007

    // let's try to reverse the problem, looking at the chains leading to 1
    // leads to 1
    // which number directly leads to 1? Only if sum of squares of digits is 1.
    // 1 can only be written as a sum of squares as follows: 1^2 + 0 + 0 + ...
    // So numbers directly leading to 1: 10, 100, 1000, ...
    // Next step, which numbers are leading directly to 10? Only if sum of squares of digits is 10.
    // 10 can only be written as a sum of squares as follows:
    // - 10 * 1^2 -> 1111111111       -> 1111111111, 10111111111, 11011111111, 11101111111, ...
    // - 1 * 2^2 + 6 * 1^2 -> 2111111 -> 2111111, 1211111, ...
    //                                   20111111, 21011111, ...
    // - 2 * 2^2 + 2 * 1^2 -> 2211    -> 2211, 1122, ...
    //                                   20211, 22011, 22101, 22110, ...
    //                                   200211, ...
    //                                   digits: 2+2 (2 ones, 2 twos, but which digits is irrelevant)
    //                                   numbers: 4!/(2!*2!)
    //                                   add zeros, up to 200 digits so up to 200-4=196 zeros
    //                                   adding x zeros -> x+4 digits, first is non-zero, for other x+3 choose 3 non-zero digits
    //                                   this can be done in x+3 over 3 ways. So multiply by x+3 over 3
    //                                   total = 4!/(2!*2!) * (1 + 1+3 over 3 + 2+3 over 3 + 3+3 over 3 + ... + 196+3 over 3)
    // - 1 * 3^2 + 1 * 1^2 -> 31      -> 13, 31
    //                                   103, 130, 301, 310
    // 13 can only be written as a sum of squares as follows:
    // etc
    // so we can per number predict:
    // - how many numbers directly lead to this number (truncated to a certain limit of digits)
    // - which of these numbers could possibly lead to new numbers which fit the limit
    // For example, numbers below 10^200 have 200 digits. 200 digits results in sum of squares of digits of maximum
    // 200 * 9^2 = 200*81 = 16.200
    // So, for any number bigger than 16.200, it's useless to explore further (but just count them)

    // let's make a distinction between generating next and counting
    // generating next needs only up to a small number of digits, say 5
    // counting needs only up to 200 digits.

    // for all numbers resulting from getSquaresPartitions (recursively) 
    // (which are permutations of the list, which form a number < 16.200 = 200*9^2)
    // call count2
    // and sum it all up (mod 10^9+7)

    // For counting:
    // "Would it not be better to explain that : f(n)= sum(sqrt(digit) for every digit in string(n)) is an injection from 10^x space to x*81 space. 
    //  so for K=200 it means 16281 in term of space.
    //  going from 10^x to 10^(x+1) is adding a square (1,4,9,16..81) to {f(10**x)}, so for every cycle it costs at mosts 162810 additions. 
    //  you get 5*10^7 ops, instead of 10^200; 
    //  Well good luck to anyone"
    // I know which of the 16281 numbers lead to 1 and which do not, for example 11358 does.
    // Now we want to know how many numbers of max 200 digits lead to 11358.
    // We can try dynamic programming:
    // count(maxDigits, n) = sum over i=0 to 9 count(maxDigits-1, n-i*i)

    /// Returns the digits in n (base k) in order
    /// For example, for k=2, n=11 returns [1;0;1;1].
    /// O(log_k n)
    let getDigitsBase k n = 
        let rec getDigitsBase' acc n = 
            if n = 0 then 
                acc
            else
                let d = n%k
                getDigitsBase' (d::acc) (n/k)

        if n = 0 then [0]
        else getDigitsBase' [] (abs(n))

    /// Returns the digits in n (base 10) in order
    /// For example, returns [1;2] for n=12.
    /// O(log_10 n)
    /// Consider using ToString() and looking at the chars, instead of this implementation.
    /// In particular for bigints. This can make a huge difference in performance.
    let getDigits n = getDigitsBase 10 n

    /// Returns the number representing the given digist (base k).
    /// For example, for k=2, digits=[1;0;1;1] returns 11.
    let getNumberBase k (digits: int list) = 
        let rec getNumberBase' acc (digits: int list) = 
            match digits with
            | [] -> acc
            | x::xs -> getNumberBase' (k * acc + x) xs
        getNumberBase' 0 digits

    /// Returns the number representing the given digist (base 10).
    /// For example, for digits=[1;2] returns 12.
    /// Consider generating a String and use Parse(), instead of this implementation.
    /// In particular for bigints. This can make a huge difference in performance. (? not checked)
    let getNumber (digits: int list) = getNumberBase 10 digits

    /// returns all permutations of source (as arrays)
    let getPermutations (source: 'a seq) = 
        let source = source |> Seq.toArray
        let n = source.Length
        let result = Array.zeroCreate n
        let used = Array.create n false
        let rec getPermutations' don =
            seq {
                if don = n then
                    yield result
                else
                    for i in 0..n-1 do
                        if not used.[i] then
                            used.[i] <- true
                            result.[don] <- source.[i]
                            yield! getPermutations' (don+1)
                            used.[i] <- false
            }
        getPermutations' 0

    // returns { [2;2;1;1]; [3;1] } for n=10, maxDigits=5
    let getSquaresPartitions n maxDigits = 
        let sqrt n = n |> float |> sqrt |> floor |> int
        let rec getSquaresPartitions' digitsLeft acc maxPart n =
            seq {
                if n = 0 then 
                    yield List.rev acc
                elif digitsLeft = 0 then 
                    ()
                else
                    let maxx = min (min maxPart (sqrt n)) 9
                    for i in 1..maxx do
                        yield! getSquaresPartitions' (digitsLeft-1) (i::acc) i (n-i*i)
            }
        getSquaresPartitions' maxDigits [] Int32.MaxValue n
    
    // returns 10, 100, 1000, 10000 for n=1, maxx=16.200=200*9^2
    // returns 1122, 1212, 1221, ... 10122, 11022, ..., 13, 31, 103, 130, 301, 310, ... for n=10, maxx=16.200
    let getDirectPrevs n maxx = 
        let maxDigits = getDigits maxx |> List.length
        let result = HashSet<int>()
        for partition in getSquaresPartitions n maxDigits do
            let maxZerosToAdd = maxDigits - (partition.Length)
            for zeros in 0..maxZerosToAdd do
                let digits = List.concat [ partition ; List.replicate zeros 0 ]
                for permutation in getPermutations digits do
                    let a = getNumber (permutation |> Array.toList)
                    if a <= maxx && a <> n then
                        result.Add(a) |> ignore
        result |> Seq.toList
    
    // returns all numbers (up to maxx) leading to n
    let rec getPrevs n maxx = 
        seq {
            for p in getDirectPrevs n maxx do
                yield p
                yield! getPrevs p maxx
        }

    let inline modulo m a = 
        if a >= LanguagePrimitives.GenericZero then 
            a%m 
        else 
            ((a%m)+m)%m // avoid overflow by adding m only if a%m is negative

    let addModulo (m: int) (x: int) (y: int) = 
        let x = modulo m x
        let y = modulo m y
        (x + y) |> modulo m
    
    // use DP to count the numbers of ways to get to the sum with exactly d digits
    let count = 
        let getAllSolutions() = 
            let (+) x y = addModulo m x y
            let result = Array2D.create 16201 201 0
            result.[0, 0] <- 1
            for digits in 1..200 do
                for s in 1..16200 do
                    for i in 0..9 do
                        if s-i*i >= 0 then
                            if result.[s-i*i, digits-1] > 0 then
                                result.[s,digits] <- result.[s,digits] + result.[s-i*i, digits-1]
            result
        
        let allSolutions = getAllSolutions()
        (fun sum d -> allSolutions.[sum, d])
    
    let count2 sum maxDigits = seq { 1..maxDigits } |> Seq.sumBy (fun d -> count sum d)

    // returns the chain starting at n, and whether or not it contains 89
    let getChain n = 
        let rec getChain' seen seenInOrder n = 
            let next = n |> getDigits |> List.map (fun d -> d*d) |> List.sum
            if Set.contains next seen then 
                (List.rev seenInOrder), (Set.contains 89 seen)
            else
                getChain' (Set.add next seen) (next::seenInOrder) next
            
        getChain' ([n] |> Set.ofList) [n] n
    
    // counts the number of numbers leading to 89 between 1 and 10^exp - 1
    let count89 exp = 
        let n = Math.Pow(10., float exp) |> int
        seq {1..n-1} |> Seq.map (fun i -> getChain i) |> Seq.filter snd |> Seq.length

    let solve() = 
        let maxDigits = sc.NextInt().Value
        let numbersLeadingToOne = getPrevs 1 16200 |> Seq.toList |> List.append [1]
        let (+) x y = addModulo m x y
        let resultOnes = 
            numbersLeadingToOne 
                |> Seq.map (fun s -> count2 s maxDigits)
                |> Seq.fold (fun s a -> s + a) 0
        let total = BigInteger.ModPow(10I, bigint maxDigits, bigint m)
        let result = ((int total) - 1 - resultOnes) |> modulo m
        printfn "%d" result
        ()

