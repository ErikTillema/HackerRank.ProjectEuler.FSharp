namespace HackerRank.FSharp

module AlmostEquilateralTriangles =

    open Util
    open System
    open System.Collections.Generic
    open System.Numerics
    
    let sc = Scanner()

    // Heron: triangle with sides a,b,c and s = (a+b+c)/2
    // has area sqrt(s*(s-a)*(s-b)*(s-c))
    // a,b,c = 5,5,6 -> s = 8 -> A = sqrt(8*3*3*2)=sqrt(144)=12
    // a,b,c = 5,5,4 -> s = 7 -> A = sqrt(7*2*2*3)=sqrt(84)
    // If s is not an integer number, then area cannot be integer. So a, s-a, s-b and s-c all have to be integers.
    // In our case a, b, c = a, a, a +/- 1
    // so  s   = (3*a +/- 1)/2    Has to be integer, so a = odd.   Example, a=5 -> s=16/2=8 or s=14/2=7
    // so  s-a = (  a +/- 1)/2    Will then automatically be an integer as well.  Example, s-a=(5+1)/2=3 or s-a=(5-1)/2=2
    // and s-c = s-a -/+ 1        Example, s-c=s-a-1=3-1=2 or s-c=s-a+1=2+1=3
    // Or, the other way round:
    // a=5 -> s-a=3 or 2, s-c=2 or 3, s=8 or 7
    // for large a: s-a=0.5*a, s-c=0.5*a, s=1.5*a, product=1.5*0.125*a^4=0.1875*a^4
    // so A=0.433*a^2
    // and O=a+b+c=3*a
    // Insight: only s*(s-c) has to be a square. product=1.5*a*0.5*a = 0.75*a^2
    // 5  -> 7*3  = 3*(3*3-2)   or 8*2  = 2*(2*3+2)
    // 7  -> 10*4 = 4*(4*3-2)   or 11*3 = 3*(3*3+2)
    // 9  -> 13*5 = 5*(5*3-2)   or 14*4 = 4*(4*3+2)
    // 11 -> 16*6 = 6*(6*3-2)   or 17*5 = 5*(5*3+2)
    // Let's try by starting at looking at all squares
    // i*(i*3+2) = n^2
    // 3i^2 + 2i - n^2 = 0
    // i^2 + 2/3i - n^2/3 = 0
    // (i + 1/3)^2 - 1/9 - n^2/3 = 0
    // i = +/- sqrt(n^2/3 + 1/9) - 1/3
    // or
    // i = +/- sqrt(n^2/3 + 1/9) + 1/3
    // max perimeter up to 10^18, so a up to 1/3*10^18, so product up to 0.75*1/9*10^36 = 10^35
    // number of squares lower than this product = sqrt 10^35 = 10^17 which is still way too high. So this doesn't help much.
    // 0.75*a^2 = n^2
    // sqrt(0.75)*a=n   1/2sqrt(3) a = n  sqrt(3) a = 2n
    // ...
    // Upon looking at the values, it turns out that there is a fixed ratio of 3.73... between successive values of a.
    // We can exploit that by looking for the next a (start at 3.73*lasta and look around that until you find newa)
    // Probably this magic ratio = 2+sqrt(3)
    // A more rubust solution and explanation is given here: https://www.mathblog.dk/project-euler-94-almost-equilateral-triangles/

    // Newton-Raphson, aka the Babylonian method for square roots.
    // @@@ place in library, this is useful. @@@ make faster by using right start of x and y based on Math.sqrt
    let sqrt (n: bigint) = 
        let mutable x = n
        let mutable y = 1I
        let e = 1I // e decides the accuracy level. @@@ Can't set to 0I, then the function gets into an endless loop. But not sure if e=1 leads always to correct results
        while abs (x - y) > e do
            x <- (x + y)/2I
            y <- n/x
        x
    
    let isSquare (product: bigint) =
        let sr = sqrt product
        sr*sr = product

    let trial1() =
        let mutable lasta = -1L
        let isSquare n = 
            let sr = n |> float |> Math.Sqrt |> floor |> int64
            sr*sr = n
        for a in 3L .. 2L .. 100_000_001L do
            for j in [-1L; 1L] do
                let s = (3L*a+j)/2L
                let s_c = s - (a+j)
                if isSquare (s*s_c) then 
                    printfn "%d %d %d => N = %d" a a (a+j) (3L*a+j)
                    if lasta <> -1L then
                        printfn "    r = %s" ((float(a)/float(lasta)).ToString())
                    lasta <- a
                
    let getAllSolutions() = 
        let isOk (a,j) = 
            let s = (3L*a+j)/2L
            let s_c = s - (a+j)
            isSquare ((bigint s)*(bigint s_c))
        let getNext (a,j,f) =
            let start = ((float a)*f) |> floor |> int64
            Seq.initInfinite id |> Seq.collect (fun delta -> [ delta; -delta ]) 
                |> Seq.map (fun delta -> start + (int64 delta)) // vary around start, +0, -0, +1, -1, +2, -2, ...
                |> Seq.collect (fun a -> [ (a,1L); (a,-1L) ])   // add j
                |> Seq.find isOk
        let magicR = 3.732051
        Seq.unfold (fun s -> 
                        let (newa, newj) = getNext s
                        let (a,_,_) = s
                        let newf = (float newa)/(float a)
                        Some(s, (newa,newj,newf))
                    ) (5L, 1L, magicR)
                
    let getSolution = 
        let allSolutions = getAllSolutions() |> Seq.map (fun (a,j,_) -> (3L*a+j)) 
                                |> Seq.takeWhile (fun N -> N <= 1_000_000_000_000_000_000L) 
                                |> Seq.toList
        (fun maxN -> allSolutions |> List.takeWhile (fun N -> N <= maxN) |> List.sum)

    let solveOne() = 
        let N = sc.NextLong().Value
        let result = getSolution N
        printfn "%d" result
        ()

    let solve() = 
        //trial1()
        let n = sc.NextInt().Value
        for _ in 1..n do 
            solveOne()
        ()
        