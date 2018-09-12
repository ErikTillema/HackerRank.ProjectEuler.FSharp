namespace HackerRank.FSharp

module SingularIntegerRightTriangles =

    open Util

    let sc = Scanner()

    /// Returns the greatest common divisor of a and b
    /// O(log min(a, b))
    let inline getGreatestCommonDivisor a b =
        let rec getGreatestCommonDivisor' a b =
            let a = abs a 
            let b = abs b
            if b < a then
                getGreatestCommonDivisor' b a
            elif a = LanguagePrimitives.GenericZero then
                b
            else    
                getGreatestCommonDivisor' (b % a) a
        getGreatestCommonDivisor' a b

    // we can use Euclid's formula perhaps to generate all primitive Pythagorean triplets
    // and then use those to generate all triplets.
    // a = m^2 - n^2, b = 2mn, c = m^2 + n^2
    // m > n > 0 and m and n co-prime and not both odd.
    // L = a+b+c =    2m^2 + 2mn  =  2m(m+n)  if triple is primitive
    // L = a+b+c = k (2m^2 + 2mn) = 2km(m+n)  if triple is not primitive, k=2,3,4...
    // So, find co-prime and not both odd m,n values
    // until 2m(m+n) > L and check if L%(2m(m+n))=0
    let getSolution = 
        let getAllSolutions() = 
            let L = 5_000_000
            let countTriplets = Array.create (L+1) 0
            let maxM = L/2 |> float |> sqrt |> int
            for m in 2..maxM do
                let ns =    if m%2=0 then 
                                seq { 1..m-1 } 
                            else 
                                seq { 2..2..m-1 }
                ns  |> Seq.takeWhile (fun n -> 2*m*(m+n) <= L)
                    |> Seq.filter (fun n -> getGreatestCommonDivisor m n = 1)
                    |> Seq.iter (fun n -> Seq.initInfinite (fun i -> i+1) 
                                            |> Seq.takeWhile (fun k -> 2*k*m*(m+n) <= L) 
                                            |> Seq.iter (fun k -> countTriplets.[2*k*m*(m+n)] <- countTriplets.[2*k*m*(m+n)] + 1)
                                 )

            let result = Array.create (L+1) 0
            for i in 1..L do 
                result.[i] <- result.[i-1] + if countTriplets.[i] = 1 then 1 else 0
            result
        let allSolutions = getAllSolutions()
        (fun n -> allSolutions.[n])
        
    let solveOne() = 
        let n = sc.NextInt().Value
        let result = getSolution n
        printfn "%d" result
        ()

    let solve() = 
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()
        ()

