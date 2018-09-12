namespace HackerRank.FSharp

module TriangularPentagonalAndHexagonal =

    open Util
    open System

    let sc = Scanner()

    let solve() = 
        let N = sc.NextLong().Value
        let a = sc.NextInt().Value
        let b = sc.NextInt().Value

        let pentagonalNumbers = Seq.initInfinite (fun i -> (int64 (i+1)) * (3L*(int64 (i+1)) - 1L) / 2L) 
        let hexagonalNumbers = Seq.initInfinite (fun i -> (int64 (i+1)) * (2L*(int64 (i+1)) - 1L))

        let isTriangularNumber (a: int64) = 
            let n = sqrt(2.0*(float a) + 0.25) - 0.5 |> floor |> int64
            n * (n+1L) = 2L*a

        let isPentagonNumber (a: int64) = 
            let n = sqrt(2.0*(float a)/3.0 + 1.0/36.0) + 1.0/6.0 |> floor |> int64
            n * (3L*n-1L) = 2L*a

        match a,b with
        | 3,5 -> 
            pentagonalNumbers |> Seq.takeWhile (fun a -> a < N) |> Seq.filter isTriangularNumber |> Seq.iter (printfn "%d")
            //[ 1L; 210L; 40755L; 7906276L; 1533776805L; 297544793910L; 57722156241751L;  ] |> Seq.iter (printfn "%d")
        | 5,6 -> 
            hexagonalNumbers |> Seq.takeWhile (fun a -> a < N) |> Seq.filter isPentagonNumber |> Seq.iter (printfn "%d")
        | _ -> invalidOp "bad input a,b"
        ()
