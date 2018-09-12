namespace HackerRank.FSharp

module ProductSumNumbers =

    open Util
    open System.Collections.Generic
    
    let sc = Scanner()

    // We could try working the other way around, as follows:
    // 12 = 2*2*3, so we can write 12 as
    // - 2*6:   2*6 - (2+6) = 4  and size {2,6} = 2  so 12 can be used to create k=4+2=6 (by using 1+1+1+1+2+6)
    // - 4*3:   4*3 - (4+3) = 5  and size {4,3} = 2  so 12 can be used to create k=5+2=7 
    // - 2*2*3: 2*2*3 - (2+2+3) = 5 and size {2,2,3}=3 so 12 can be used to create k=5+3=8
    // Working our way up for all numbers until we have filled all ks.
    // wait, a bit different
    //    6 = 6 or 6 = 2*3
    // 2+3=5, so that's a -1 and #{2,3}=2 so that's also a -1
    // so 6 = {0,+2}
    // also 2 = {0}, 3 = {0}, 4 = {0,-1}, 5 = {0}, 6 = {0,-2}, 7 = {0}, 8 = {0,-3,-4}, 9 = {0,-4}, 10 = {0,-4}, 11 = {0}, 
    // 12 = {0,-5,-6,-7}, 13 = {0}, 14 = {0,-6}, 15 = {0,-8}, 16 = {}
    //    8 = 2*4 => 2+4 - 8 = -2 and #{2,4}=2 so that's together -3. Then we can make -3-1=-4 as well.
    // so all in all, 8 = {0,-3,-4}
    //    12 = 2*6 => 2+6 - 12 = -4 => -5. Then we can make -5-2=-7 as well.
    // or 12 = 3*4 => 3+4 - 12 = -5 => -6. Then we can make -6-1=-7 as well.
    // so all in all, 12 = {0,-5,-6,-7}
    //    16 = 2*8 => 2+8 - 16 = -6 => -7. Then we can make -7-3 and -7-4 as well
    // or 16 = 4*4 => 4+4 - 16 = -8 => -9. Then we can make -8-1 and -8-1-1 as well
    // so all in all, 16 = {0,-7,-9,-10,-11}

    let sqrt n = n |> float |> sqrt |> floor |> int
    let isDivisor n i = n % i = 0
    
    let getSolution = 
        let getAllSolutions() = 
            let N = 200_000
            let result = Array.create (N+1) -1
            let M = 201_000
            let vals = Array.init (M+1) (fun _ -> HashSet<int>())

            for a in 2..M do
                vals.[a].Add(0) |> ignore
                for divisor in 2 .. sqrt a do
                    if isDivisor a divisor then
                        let b,c = divisor, a/divisor // a = b*c, 12=2*6
                        let start = a - (b+c) + 1 // 12-(2+6)=4 => 5
                        for addition1 in vals.[b] do
                            for addition2 in vals.[c] do
                                vals.[a].Add(start + addition1 + addition2) |> ignore

                for vall in vals.[a] do
                    if vall > 0 && vall <= N && result.[vall] = -1 then 
                        result.[vall] <- a

            result
        
        let solutions = getAllSolutions()
        (fun n -> solutions |> Seq.skip 1 |> Seq.take (n-1) |> Seq.distinct |> Seq.sumBy int64)

    let solve() = 
        let n = sc.NextInt().Value
        let result = getSolution n
        printfn "%d" result
        ()