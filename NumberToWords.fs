namespace HackerRank.FSharp

module NumberToWords =

    open Util
    open System

    let sc = Scanner()

    let rec getWords (n: int64) = 
        match n with
        | 0L -> []
        | 1L -> [ "One" ]
        | 2L -> [ "Two" ]
        | 3L -> [ "Three" ]
        | 4L -> [ "Four" ]
        | 5L -> [ "Five" ]
        | 6L -> [ "Six" ]
        | 7L -> [ "Seven" ]
        | 8L -> [ "Eight" ]
        | 9L -> [ "Nine" ]
        | 10L -> [ "Ten" ]
        | 11L -> [ "Eleven" ]
        | 12L -> [ "Twelve" ]
        | 13L -> [ "Thirteen" ]
        | 14L -> [ "Fourteen" ]
        | 15L -> [ "Fifteen" ]
        | 16L -> [ "Sixteen" ]
        | 17L -> [ "Seventeen" ]
        | 18L -> [ "Eighteen" ]
        | 19L -> [ "Nineteen" ]
        | 20L -> [ "Twenty" ]
        | 30L -> [ "Thirty" ]
        | 40L -> [ "Forty" ]
        | 50L -> [ "Fifty" ]
        | 60L -> [ "Sixty" ]
        | 70L -> [ "Seventy" ]
        | 80L -> [ "Eighty" ]
        | 90L -> [ "Ninety" ]
        | _ when n < 100L ->
            (getWords (n/10L*10L)) @ (getWords (n%10L))
        | _ when n < 1000L ->
            (getWords (n/100L)) @ ["Hundred"] @ (getWords (n%100L))
        | _ when n < 1000000L ->
            (getWords (n/1000L)) @ ["Thousand"] @  (getWords (n%1000L))
        | _ when n < 1000000000L ->
            (getWords (n/1000000L)) @ ["Million"] @  (getWords (n%1000000L))
        | _ when n < 1000000000000L ->
            (getWords (n/1000000000L)) @ ["Billion"] @  (getWords (n%1000000000L))
        | _ when n < 1000000000000000L ->
            (getWords (n/1000000000000L)) @ ["Trillion"] @  (getWords (n%1000000000000L))

    let solveOne() = 
        let n = sc.NextLong().Value
        let result = getWords n
        let result = result |> String.concat " "
        printfn "%s" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
