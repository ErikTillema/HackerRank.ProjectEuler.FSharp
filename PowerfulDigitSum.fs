namespace HackerRank.FSharp

module PowerfulDigitSum =

    open Util
    open System.Numerics

    let sc = Scanner()

    let getDigitSum n = 
        let rec getDigitSum' acc n = 
            if n = 0I then
                acc
            else
                let d = (n%10I) |> int
                getDigitSum' (d+acc) (n/10I)

        getDigitSum' 0 n
    
    /// Wow, for large bigints, getting the digits base 10 by using ToString()
    /// is much much faster than by modding and dividing!
    let getDigitSumFast (n: bigint) = 
        n |> string |> Seq.sumBy (fun c -> (int c) - (int '0'))

    let getMaxDigitSum n a = 
        let folder (number,maxDigitSum) _ =
            let newNumber = number*a
            (newNumber, max maxDigitSum (getDigitSumFast newNumber))
        let res = seq {1..n-1} |> Seq.fold folder (1I, 1) 
        res |> snd
    
    let solvePrecalculated n = 
        let map = Map.ofList [(180, 1918);
(181, 1918);
(182, 1918);
(183, 1922);
(184, 1954);
(185, 1954);
(186, 1988);
(187, 1988);
(188, 2122);
(189, 2122);
(190, 2122);
(191, 2122);
(192, 2122);
(193, 2122);
(194, 2123);
(195, 2123);
(196, 2169);
(197, 2187);
(198, 2187);
(199, 2187);
(200, 2205);]
        let result = Map.find n map
        result

    let solve() = 
        let n = sc.NextInt().Value
        let result = 
            //if n >= 180 then
            //    solvePrecalculated n
            //else
                seq { 2..n-1 } |> Seq.map (bigint >> (getMaxDigitSum n)) |> Seq.max
        printfn "%d" result
        ()

    let solveAll() = 
        for n in 5..200 do
            let result = seq { 2..n-1 } |> Seq.map (bigint >> (getMaxDigitSum n)) |> Seq.max
            printfn "(%d, %d); " n result
    
