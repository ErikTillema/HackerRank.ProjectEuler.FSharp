namespace HackerRank.FSharp

module CoinSums =

    open Util
    open System

    let sc = Scanner()
    let m = 1000000007
    let N = 100000
    let coins = 8
    let coin = [| 1; 2; 5; 10; 20; 50; 100; 200 |]

    let inline modulo m a = ((a%m)+m)%m

    let allResults = 
        let poss = Array2D.init (coins+1) (N+1) (fun _ _ -> 0)
        poss.[0,0] <- 1
        for coinsUsed in 1..coins do
            for amount in 0..N do
                let c = coin.[coinsUsed - 1]
                if amount >= c then
                    poss.[coinsUsed,amount] <- poss.[coinsUsed-1, amount] + poss.[coinsUsed, amount-c]
                else
                    poss.[coinsUsed,amount] <- poss.[coinsUsed-1, amount]
                poss.[coinsUsed,amount] <- modulo m poss.[coinsUsed,amount]
        poss

    let solveOne() = 
        let n = sc.NextInt().Value
        let result = allResults.[coins,n]
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
