namespace HackerRank.FSharp

module PandigitalProducts =

    open Util
    open System.Collections.Generic

    let sc = Scanner()

    let getDigits n = 
        let rec getDigits' acc n = 
            if n = 0 then 
                acc
            else
                let d = n%10
                getDigits' (d::acc) (n/10)

        if n = 0 then [0]
        else getDigits' [] (abs(n))

    let isOk n x y = 
        let digitCount = Array.replicate 10 0
        seq {
            yield x
            yield y
            yield x*y
        } |> Seq.iter (fun a -> let digits = getDigits a
                                digits |> List.iter (fun d -> digitCount.[d] <- digitCount.[d] + 1)
                      )
        ([ 1..n ] |> List.forall (fun d -> digitCount.[d] = 1)) && (0 :: [ n+1..9 ] |> List.forall (fun d -> digitCount.[d] = 0))

    let getResult n = 
        let set = HashSet<int>()
        for x in 1..99 do
            let maxy = if x < 10 then 9999 else 999
            for y in x+1..maxy do
                if isOk n x y then
                    //printfn "%i x %i = %i" x y (x*y)
                    ignore(set.Add(x*y))
        set |> Seq.sum

    //let allResults = [ 4..9 ] |> List.map (fun i -> getResult i)

    let solve() = 
        let n = sc.NextInt().Value
        let result = getResult n
        printfn "%i" result