namespace HackerRank.FSharp

module CuboidRoute =

    open Util
    open System.Collections.Generic

    let sc = Scanner()

    // we can use Euclid's formula to generate all primitive Pythagorean triplets
    // and then use those to generate all triplets.
    // a = m^2 - n^2, b = 2mn, c = m^2 + n^2
    // m > n > 0 and m and n co-prime and not both odd.
    // Once we have a triplet, we have to split up one of the two right sides in two. This should be done such that the split up parts are both
    // smaller than or equal to the non split up side. 
    // So we can split up the smaller right side into two (case 1)
    // Or we can split up the bigger right side into two, but keeping both parts smaller or equal to the other side (case 2).
    // All parts have to smaller or equal to M (aka maxx). 
    // So for case 1, the bigger side can be at most M and the smaller side at most M.
    // So for case 2, the bigger side can be at most 2M and the smaller side at most M.
    // In case 1, we split the smaller side so the bigger side remains equal. All the resulting cuboids are unique.
    // For example, 13^2 = 12^2 + 5^2 . We split 5 in two parts, resulting in the cuboids (12,1,4),   (12,2,3)
    //                                                                                    (a, 1,b-1), (a, 2,b-2), ... (a, b/2, b-b/2)
    // In case 2, suppose we split the bigger side.
    // For example, 73^2 = 55^2 + 48^2 . We split 55 in two parts, resulting in the cuboids (48,48,7),  (48,47, 8), ... (48,28,27)
    //                                                                                      (b, b, a-b),(b, b-1,a-b+1), ... (b, a-a/2, a/2)
    // But haven't we seen one of these before? No, I don't think so. So they are also unique.
    // But what if a equals b? That's not possible.
    // So, given a triplet (a,b,_) where a > b (and b > 1)
    // case 1: b/2 unique cuboids, max side is a
    // case 2: if (2*b >= a) a/2 - (a-b-1) unique cuboids, max side is b
    
    // all sides of the cube (not the triangle) have to be <= max
    // case 1, max side is a, so a <= max, so use upper bound for m that satisfies a <= max
    // so m in 1 .. maxM
    // and then n in 1..m-1
    // and then k in 1..maxx/a
    //
    // case 2, max side is b, so b <= max, but also 2*b >= a, so a <= 2*b <= 2*max, so use upper bound for m that satisfies a <= 2*max
    // so m in 1 .. maxM
    // and then n in 1..m-1
    // and then k in 1..maxx/b
    //
    // produces (a',b',c) = ( k(m^2 - n^2), 2kmn, k(m^2 + n^2) )

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
    
    let getCuboid m n k =
        let getCuboid' a b c = 
            let arr = [|a;b;c|] |> Array.sort  // can happen that a > b but also b > a, so sort ...
            (arr.[1], arr.[0], arr.[2])        // ... and take a and b properly
        let a,b,c = 2*k*m*n, k*(m*m - n*n), k*(m*m + n*n)
        getCuboid' a b c

    let getMAndNs maxm =
        seq {
            for m in 1 .. maxm do
                for n in 1 .. m-1 do
                    if (getGreatestCommonDivisor m n = 1) && (m%2=0 || n%2=0) then
                        yield (m,n)
        }

    let getUpperBoundM maxx = int((maxx |> float |> sqrt) * 1.1)

    let ``check that sqrt(maxx)*1.1 is upper bound for m, resulting in a <= maxx``() =
        for maxx in 5..100_000..400_000 do
            let mutable goodMinM = 1<<<30
            let mutable goodMaxM = 0
            for (m,n) in getMAndNs (2*(maxx |> float |> sqrt |> int)) do
                let a,_,_ = getCuboid m n 1
                if a <= maxx then 
                    goodMinM <- min goodMinM m
                    goodMaxM <- max goodMaxM m
            if goodMaxM > getUpperBoundM maxx then invalidOp "goodMinM > sqrt"

    let getSolution = 
        let MAXX = 400_000
        let count = Array.create (MAXX+1) 0L
        let mutable tmpcnt = 0
        let mutable tmpcnt2 = 0

        for (m,n) in getMAndNs (getUpperBoundM MAXX) do
            let a,_,_ = getCuboid m n 1
            tmpcnt2 <- tmpcnt2 + 1
            for k in 1 .. MAXX/a do
                tmpcnt <- tmpcnt + 1
                let a,b,_ = getCuboid m n k
                let add = b/2
                count.[a] <- count.[a] + int64 add

        for (m,n) in getMAndNs (getUpperBoundM (2*MAXX)) do
            let a,b,_ = getCuboid m n 1
            if 2*b >= a then
                tmpcnt2 <- tmpcnt2 + 1
                for k in 1 .. MAXX/b do
                    tmpcnt <- tmpcnt + 1
                    let a,b,_ = getCuboid m n k
                    let add = a/2 - (a-b-1)
                    count.[b] <- count.[b] + int64 add

        let acc = Array.create (MAXX+1) 0L
        for i in 1..MAXX do acc.[i] <- acc.[i-1] + count.[i]

        (fun maxx -> acc.[maxx])

    let solveOne() = 
        let max = sc.NextInt().Value
        let result = getSolution max
        printfn "%d" result
        ()

    let solve() = 
        //``check that sqrt(maxx)*1.1 is upper bound for m, resulting in a <= maxx``()
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()
        ()