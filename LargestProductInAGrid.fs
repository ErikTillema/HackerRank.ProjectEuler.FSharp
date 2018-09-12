namespace HackerRank.FSharp

module LargestProductInAGrid =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()
    let (w,h) = (20,20)
    let l = 4
    let dx = [| 1; 0; 1; 1 |]
    let dy = [| 0; 1; 1; -1 |]

    let grid = 
        let result = Array2D.init w h (fun _ _ -> 0)
        sc.Lines |> Seq.iteri (fun y s ->
            let sc2 = Scanner(" \t", s)
            sc2.Tokens |> Seq.iteri (fun x s2 ->
                let value = s2 |> int
                result.[x,y] <- value
            )
        )
        result

    let solve() = 
        let mutable result = Int32.MinValue
        for x in 0..w-1 do
            for y in 0..h-1 do
                for i in 0..3 do
                    let (nx,ny) = (x + (l-1)*dx.[i], y + (l-1)*dy.[i])
                    if 0 <= nx && nx < w && 0 <= ny && ny < h then
                        let mutable product = 1
                        for j in 0..l-1 do
                            let (nx2,ny2) = (x + j*dx.[i], y + j*dy.[i])
                            product <- product * grid.[nx2,ny2]
                        result <- max result product
        printfn "%i" result