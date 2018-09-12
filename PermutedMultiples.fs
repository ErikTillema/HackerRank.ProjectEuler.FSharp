namespace HackerRank.FSharp

module PermutedMultiples =

    open Util
    open System.Collections.Generic

    let sc = Scanner()

    /// Returns the digits in n (base k) in order
    /// For example, for k=2, n=11 returns [1;0;1;1].
    /// O(log_k n)
    let getDigitsBase k n = 
        let rec getDigitsBase' acc n = 
            if n = 0 then 
                acc
            else
                let d = n%k
                getDigitsBase' (d::acc) (n/k)

        if n = 0 then [0]
        else getDigitsBase' [] (abs(n))

    /// Returns the digits in n (base 10) in order
    /// For example, returns [1;2] for n=12.
    /// O(log_10 n)
    let getDigits n = getDigitsBase 10 n

    let isOk k a = 
        let digits = getDigits a |> List.sort
        seq {2..k} |> Seq.forall (fun i -> digits = (i*a |> getDigits |> List.sort))

    let solve() = 
        let n = sc.NextInt().Value
        let k = sc.NextInt().Value

        seq { 1 .. n } 
            |> Seq.filter (isOk k)
            |> Seq.iter (fun a -> printfn "%s" ([1..k] |> List.map ((*) a) |> List.map string |> String.concat " "))

        ()
 