namespace HackerRank.FSharp

module EvenFibonacciNumbers =

    open Util
    open System
    
    type Matrix = Matrix of int64[,]

    let sc = Scanner()
    let min = 1
    let max = 85 // fibonacci 85 > 10^16
    let M = Matrix( array2D [ [ 1L; 2L; 1L ] ;
                              [ 2L; 3L; 2L ] ;
                              [ 0L; 0L; 1L ] ] )
    let F0 = Matrix( array2D [ [ 2L; 3L; 2L ] ] )

    let multiply (Matrix(a)) (Matrix(b)) = 
        let values = Array2D.init (b.GetLength(0)) (a.GetLength(1)) (fun _ _ -> 0L)
        for x in 0..b.GetLength(0)-1 do 
            for y in 0..a.GetLength(1)-1 do
                let mutable sum = 0L
                for k in 0..a.GetLength(0)-1 do
                    sum <- sum + a.[k,y] * b.[x,k]
                values.[x, y] <- sum
        Matrix(values)

    let allResults =    seq {
                            let mutable Mk = M
                            for i in 0..27 do
                                let (Matrix(Fk)) = multiply Mk F0
                                yield (Fk.[0,0], Fk.[0,2])
                                Mk <- multiply Mk M
                        } |> Seq.toArray

    let solveOne() = 
        let n = sc.NextLong().Value
        let result = allResults |> Array.findBack (fun (f,_) -> f <= n) |> snd
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()