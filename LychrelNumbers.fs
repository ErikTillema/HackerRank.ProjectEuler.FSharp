namespace HackerRank.FSharp

module LychrelNumbers =

    open Util
    open System
    open System.Collections.Generic

    let sc = Scanner()

    /// Returns the digits in n (base k) in order
    /// For example, for k=2, n=11 returns [1;0;1;1].
    /// O(log_k n)
    let getDigitsBase k n = 
        let rec getDigitsBase' acc n = 
            if n = 0L then 
                acc
            else
                let d = (n%(int64 k)) |> int
                getDigitsBase' (d::acc) (n/(int64 k))

        if n = 0L then [0]
        else getDigitsBase' [] (abs(n))

    /// Returns the digits in n (base 10) in order
    /// For example, returns [1;2] for n=12.
    /// O(log_10 n)
    let getDigits n = getDigitsBase 10 n
    
    /// Returns the number representing the given digist (base k).
    /// For example, for k=2, digits=[1;0;1;1] returns 11.
    let getNumberBase k (digits: int list) = 
        let rec getNumberBase' acc (digits: int list) = 
            match digits with
            | [] -> acc
            | x::xs -> getNumberBase' ((int64 k) * acc + (int64 x)) xs
        getNumberBase' 0L digits

    /// Returns the number representing the given digist (base 10).
    /// For example, for digits=[1;2] returns 12.
    let getNumber (digits: int list) = getNumberBase 10 digits

    let isPalindrome a = 
        let digits = getDigits a
        let digitsRev = digits |> List.rev
        digits = digitsRev
    
    let addReverse a = 
        a + (a |> getDigits |> List.rev |> getNumber)

    let rec getPalindrome = 
        let cache = Dictionary<int64,int64 option>()
        (fun n i -> 
            if cache.ContainsKey(n) then cache.[n]
            else
                let result =
                    if isPalindrome n then Some(n)
                    elif n > (1L<<<60) then None
                    elif i > 0 then getPalindrome (addReverse n) (i-1)
                    else None
                cache.Add(n,result)
                result
        )
    
    let solve() = 
        let n = sc.NextLong().Value
        let count = Dictionary<int64,int>()
        let increment a = 
            if not(count.ContainsKey(a)) then count.Add(a,0)
            count.[a] <- count.[a] + 1
        for i in 1L..n do
            match getPalindrome i 60 with
            | Some(a) -> increment a
            | None -> ()
        
        let result = count |> Seq.sortBy (fun kvp -> (-kvp.Value, kvp.Key)) |> Seq.head 
        printfn "%d %d" (result.Key) (result.Value)
        ()
    