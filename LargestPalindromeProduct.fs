namespace HackerRank.FSharp

module LargestPalindromeProduct =

    open Util
    open System

    let sc = Scanner()

    let getDigits n = 
        let rec getDigits' acc n = 
            if n = 0 then 
                List.rev (0::acc)
            else
                let d = n%10
                getDigits' (d::acc) (n/10)
        getDigits' [] n

    let isPalindrome n = 
        let digits = getDigits n
        digits = List.rev digits

    let allResults = 
        seq {
            for i in 100..999 do 
                for j in i..999 do 
                    let pr = i * j
                    if 100000 <= pr && pr <= 999999 && isPalindrome pr then
                        yield pr
        } |> Seq.sort |> Seq.toArray
    
    let solveOne() = 
        let n = sc.NextInt().Value
        let result = allResults |> Array.findBack (fun pr -> pr < n)
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
