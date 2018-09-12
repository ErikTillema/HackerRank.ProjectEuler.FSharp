namespace HackerRank.FSharp

module PowerfulDigitCounts =

    open System
    open Util

    let sc = Scanner()

    let countDigits (n: uint64) = n |> string |> String.length

    let inline pow x y =
        let rec pow' acc n = 
            if n = LanguagePrimitives.GenericZero then 
                acc
            else 
                pow' (x*acc) (n - LanguagePrimitives.GenericOne)
        if x = LanguagePrimitives.GenericZero then 
            LanguagePrimitives.GenericZero
        elif y = LanguagePrimitives.GenericZero then 
            LanguagePrimitives.GenericOne
        else 
            pow' LanguagePrimitives.GenericOne y

    let solve() = 
        let n = sc.NextInt().Value
        let result = seq {1..9} |> Seq.map (uint64 >> (fun i -> pow i (uint64 n))) |> Seq.filter (fun i -> countDigits i = n)
                        |> Seq.map string |> String.concat "\n"
        printfn "%s" result
        ()
