namespace HackerRank.FSharp

module XorDecryption =

    open Util
    open System

    let sc = Scanner()

    let solve() = 
        let n = sc.NextInt().Value
        let vals = sc.Tokens |> Seq.take n |> Seq.map int |> Seq.toArray
        let valsPerChar = Array.init 3 (fun i -> vals |> Seq.indexed |> Seq.filter (fun (j,v) -> j%3=i) |> Seq.map snd |> Seq.toArray)
        let validChars = [ for i in 0..25 -> char(i+int 'a') ]  @ [ for i in 0..25 -> char(i+int 'A') ] @ [ for i in 0..9 -> char(i+int '0') ] @ [' ';'(';')';':';';';',';'.';'\'';'?';'!';'-'] |> List.map int |> Set.ofList
        let isValid a = Set.contains a validChars
        let findChar i = 
            [ 0..25 ] |> Seq.map (fun i -> i + int 'a') |> Seq.find (fun c -> valsPerChar.[i] |> Seq.forall (fun v -> isValid(v ^^^ c)) )
        let result = [| 0..2 |] |> Array.map (findChar >> char) |> String
        printfn "%s" result
        ()
