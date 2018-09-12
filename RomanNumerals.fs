namespace HackerRank.FSharp

module RomanNumerals =

    open Util
    open System
    open System.Collections.Generic
    
    let sc = Scanner()

    let parseRomanNumber (s: string) = 
        let parseRomanChar c = 
            match c with
            | 'M' -> 1000
            | 'D' -> 500
            | 'C' -> 100
            | 'L' -> 50
            | 'X' -> 10
            | 'V' -> 5
            | 'I' -> 1
            | _ -> invalidOp "bad character"
        s |> Seq.sumBy parseRomanChar
    
    let getRomanNumber n = 
        let rec getRomanNumber' acc n = 
            match n with
            | 0 -> List.rev acc
            | n when n >= 1000 -> getRomanNumber' ('M'::acc) (n-1000)
            | n when n >= 900  -> getRomanNumber' ('M'::'C'::acc) (n-900)
            | n when n >= 500  -> getRomanNumber' ('D'::acc) (n-500)
            | n when n >= 400  -> getRomanNumber' ('D'::'C'::acc) (n-400)
            | n when n >= 100  -> getRomanNumber' ('C'::acc) (n-100)
            | n when n >= 90  -> getRomanNumber' ('C'::'X'::acc) (n-90)
            | n when n >= 50  -> getRomanNumber' ('L'::acc) (n-50)
            | n when n >= 40  -> getRomanNumber' ('L'::'X'::acc) (n-40)
            | n when n >= 10  -> getRomanNumber' ('X'::acc) (n-10)
            | n when n >= 9  -> getRomanNumber' ('X'::'I'::acc) (n-9)
            | n when n >= 5  -> getRomanNumber' ('V'::acc) (n-5)
            | n when n >= 4  -> getRomanNumber' ('V'::'I'::acc) (n-4)
            | n when n >= 1  -> getRomanNumber' ('I'::acc) (n-1)
            | _ -> invalidOp ""
        getRomanNumber' [] n |> Array.ofList |> String

    let solveOne() = 
        let s = sc.Next().Value
        let n = parseRomanNumber s
        let result = getRomanNumber n
        printfn "%s" result
        ()

    let solve() = 
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()
        ()