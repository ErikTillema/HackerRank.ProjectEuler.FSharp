namespace HackerRank.FSharp

module AnagramicSquares =

    open Util

    let sc = Scanner()
    
    // this counting digits to rebuild the sorted number is faster than a sort.
    // I guess it's comparable to a count sort.
    let getSorted n = 
        let digitCount = Array.replicate 10 0
        let rec countDigits' n = 
            if n = 0L then ()
            else 
                let digit = n%10L |> int
                digitCount.[digit] <- digitCount.[digit] + 1
                countDigits' (n/10L)
        let rec getNumber' acc don = 
            match don with
            | 10 -> acc
            | _  -> 
                if digitCount.[don] > 0 then
                    digitCount.[don] <- digitCount.[don] - 1
                    getNumber' (10L*acc+(int64 don)) don
                else
                    getNumber' acc (don+1)
        countDigits' n
        getNumber' 0L 0
    
    let squares digits = 
        let min = pown 10L (digits - 1)
        let max = pown 10L digits
        seq { 1..10_000_000 } // Seq.initInfinite id
                |> Seq.map int64 
                |> Seq.map (fun i -> i*i) 
                |> Seq.skipWhile (fun s -> s < min)
                |> Seq.takeWhile (fun s -> s < max) 

    let solve() =
        let n = sc.NextInt().Value
        let result = squares n |> Seq.toArray
                        |> Array.map (fun n -> (n, (getSorted n))) 
                        |> Array.groupBy snd
                        |> Array.maxBy (fun (key,vals) -> (vals |> Array.length, vals |> Array.maxBy fst))
                        |> snd |> Array.map fst |> Array.max

        printfn "%d" result
        ()