namespace HackerRank.FSharp

module DoubleBasePalindromes =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()

    let log a b = 
        let a = a |> int64 // avoid overflow with int64
        let b = b |> int64
        let mutable result = 0
        let mutable multiple = 1L
        while multiple*a <= b do
            multiple <- multiple*a
            result <- result + 1
        result

    /// Returns x^y
    let pow x y =
        let rec pow' acc n = 
            if n = 0 then 
                acc
            else 
                pow' (x*acc) (n - 1)
        pow' 1 y

    let based k n =
        let rec based' acc n = 
            match n with
            | 0 -> List.rev acc
            | _ -> based' ((n%k)::acc) (n/k)
        based' [] n

    let getDigits (n: int64) = 
        let rec getDigits' acc (n: int64) = 
            if n = 0L then 
                acc
            else
                let d = n%10L |> int
                getDigits' (d::acc) (n/10L)

        if n = 0L then [0]
        else getDigits' [] (abs(n))

    let getNumber (digits: int list) = 
        let rec getNumber' acc (digits: int list) = 
            match digits with
            | [] -> acc
            | x::xs -> getNumber' (10 * acc + x) xs
        getNumber' 0 digits

    let isPalindrome digits = 
        digits = (digits |> List.rev)

    let isOk k n = 
        n |> based k |> isPalindrome

    let getPalidromesOfDigits d = 
        if d = 1 then
            seq { 1..9 }
        else
            seq {
                let min = pow 10 ((d/2)-1) 
                let max = (pow 10 (d/2)) - 1
                for i in min..max do
                    let digits = getDigits (i |> int64)
                    if d%2 = 0 then
                        yield getNumber (digits @ (digits |> List.rev))
                    else
                        for j in 0..9 do
                            yield getNumber (digits @ [j] @ (digits |> List.rev))
            }

    let getPalindromesUntil n = 
        let digits = (log 10 n) + 1
        seq {
            for d in 1..digits do
                for p in getPalidromesOfDigits d do
                    if p <= n then
                        yield p
        }

    let solve() = 
        let n = sc.NextInt().Value - 1
        let k = sc.NextInt().Value
        let mutable result = 0L
        n |> getPalindromesUntil |> Seq.filter (isOk k) |> Seq.iter (fun p ->   //printfn "%i %s" p (based k p |> List.map (fun i -> i.ToString()) |> String.concat "")
                                                                                result <- result + (p |> int64))
        printfn "%i" result

