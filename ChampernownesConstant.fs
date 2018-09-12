namespace HackerRank.FSharp

module ChampernownesConstant =

    open Util
    open System

    let sc = Scanner()

    let rec getDigitOfNumber n i = 
        if i = 0L then n%10L
        else getDigitOfNumber (n/10L) (i-1L)

    let getDigit index = 
        let rec getDigit index length startNumber numbers =
            if index < numbers * length then
                let number = startNumber + (index / length)
                getDigitOfNumber number (length-1L-(index%length))
            else
                getDigit (index - numbers*length) (length+1L) (startNumber*10L) (numbers*10L)
                
        getDigit index 1L 1L 9L

    let test() =
        let source = [1..100000]
        let d1 = source |> List.map (fun i -> i.ToString()) |> String.concat "" |> Seq.map (fun c -> (c |> int) - ('0' |> int)) |> Seq.toList
        let d2 = [1..d1.Length] |> List.map (fun i -> i-1) |> List.map (int64 >> getDigit >> int)
        if d1 <> d2 then invalidOp "fds"                    

    let solveOne() = 
        let index = Array.init 7 (fun i -> sc.NextLong().Value - 1L)
        let digits = index |> Array.map getDigit 
        let result = digits |> Array.fold (*) 1L
        printfn "%i" result

    let solve() = 
        //test()
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()