namespace HackerRank.FSharp

module PandigitalMultiples =

    open Util
    open System

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

    let isOk k n = 
        let used = Array.replicate (k+1) false
        let rec isOk' n i don = 
            let digits = (n*i) |> getDigits
            let mutable stillOk = true
            let mutable don = don
            digits |> List.iter (fun d ->   if d > k || d = 0 || used.[d] then 
                                                stillOk <- false
                                            else 
                                                used.[d] <- true
                                                don <- don + 1 )
            if don = k && stillOk then true
            elif don >= k || not stillOk then false
            else isOk' n (i+1) don
        isOk' n 1 0

    let solve() = 
        let n = sc.NextInt().Value - 1
        let k = sc.NextInt().Value
        for i in 2..n do
            if isOk k i then printfn "%i" i
