namespace HackerRank.FSharp

module IntegerRightTriangles =

    // @@@ maak mooie versie van eigen oplossing
    // en vergelijk met methode:
    // - maak primitive pythagorean triplets met  (m*m+n*n , 2*m*n, m*m-n*n)
    // - en dan vermenigvuldigen om ook de niet primitive te krijgen

    open Util
    open System
    open System.Collections.Generic

    let sc = Scanner()
    let N = 5000000L

    let pow x y =
        let rec pow' acc n = 
            if n = 0 then 
                acc
            else 
                pow' (x*acc) (n - 1)
        pow' 1L y

    let getPrimeFactors n = 
        let rec getPrimeFactors' acc last n = 
            if n = 1L then acc |> List.rev
            else
                let mutable f = max 2L last
                while n%f <> 0L && (f+1L)*(f+1L) <= n do f <- f + 1L
                if n%f <> 0L then f <- n

                if f <> last then
                    getPrimeFactors' ((f,1)::acc) f (n/f)
                else
                    let (f,m)::xs = acc
                    getPrimeFactors' ((f,m+1)::xs) f (n/f)
        getPrimeFactors' [] -1L n

    let specialSqrt n = 
        let primeFactors = getPrimeFactors n
        let mutable result = 1L
        for (p,m) in primeFactors do
            result <- result * (pow p ((m+1)/2))   
        result

    let rec forUntil start continueUntil increment = 
        seq {
            let mutable i = start
            if continueUntil i then
                yield i
                i <- increment i
                yield! forUntil i continueUntil increment
        }

    let test = 
        for i in forUntil 0 ((<) 4) ((+) 1) do
            printfn "%i" i

    let solve() = 
        let getAllTriplets N =
            //let triplets = HashSet<int64*int64*int64>()
            let solutions = Array.replicate (1+(N|>int)) 0
            let Q = N/2L //biggest square
            for d in 1L..Q do
                let mutable b = 0L // d
                let mutable cont = true
                let step = specialSqrt d //if isSquare d then sqrt d else d
                while b <= Q && cont do
                    let bb = b*b
                    let a = (bb - d*d)/(2L*d)
                    if a > Q then cont <- false
                    if (bb - d*d)%(2L*d) = 0L && cont then
                        let c = N - a - b //sqrt (a*a + bb)
                        if a > 0L && b > 0L && c > 0L && b < a then //
                            //let [a;b;c] = [a;b;c] |> List.sort
                            //if a*a + b*b <> c*c then invalidOp "fdsfsd"

                            //printfn "%i %i %i" a b c
                            //if triplets.Contains(a,b,c) then 
                                //printfn "%i %i %i" a b c
                                //()
                            //else 
                                //printfn "%i %i %i" a b c
                                //ignore(triplets.Add(a,b,c))
                            let sum = a+b+c |> int
                            if sum <= (N |> int) then
                                solutions.[sum] <- solutions.[sum] + 1
                    b <- b + step
            //triplets
            solutions

        let allResults() = 
            let N = N |> int
//            let solutions = Array.replicate (N+1) 0
//            for (a,b,c) in getAllTriplets (N |> int64) do
//                let sum = a+b+c |> int
//                if sum <= N then
//                    solutions.[sum] <- solutions.[sum] + 1
            let solutions = getAllTriplets (N |> int64)

            let result = Array.zeroCreate (N+1)
            result.[12] <- (12, 1)
            for i in 13..N do
                let s = solutions.[i]
                if s > (result.[i-1] |> snd) then
                    result.[i] <- (i, s)
                else
                    result.[i] <- result.[i-1]

            let mutable result2 = [(12,1)]
            for i in 13..N do
                if result.[i] <> result.[i-1] then
                    result2 <- result.[i] :: result2
            let result2 = List.rev result2

            printf "let result2 = ["
            for (a,b) in result2 do
                printf "(%i,%i); " a b
            printfn "]"

            result

        let result2 = [(12,1); (60,2); (120,3); (240,4); (420,5); (720,6); (840,8); (1680,10); (2520,12); (4620,13); (5040,16); (9240,20); (18480,25); (27720,31); (55440,40); (110880,46); (120120,50); (166320,51); (180180,53); (240240,64); (360360,80); (720720,104); (1081080,105); (1441440,124); (2042040,130); (2162160,135); (2882880,142); (3603600,158); (4084080,168); ]

        let solveOne() = 
            let n = sc.NextInt().Value
            let result = result2 |> List.findBack (fun (m,_) -> m <= n) |> fst
            //let result = allResults.[n] |> fst
            printfn "%i" result

        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
