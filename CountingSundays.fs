namespace HackerRank.FSharp

module CountingSundays =

    open Util
    open System
    open System.Numerics

    let sc = Scanner()

    let inline modulo m a = ((a%m)+m)%m

    let isLeapYear (year: int64) = 
        (year % 400L = 0L) || (year % 100L <> 0L && year % 4L = 0L)

    let daysInMonth month (year: int64) = 
        match month with
        | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
        | 4 | 6 | 9 | 11 -> 30
        | 2 ->  if isLeapYear year then 29
                else 28
        | _ -> invalidOp "bad month"

    let countSundays400 = 
        let mutable dayOfWeek = 0 // monday
        let mutable result = 0L
        for y in 0..399 do
            for m in 1..12 do
                if dayOfWeek = 6 then result <- result + 1L
                dayOfWeek <- modulo 7 (dayOfWeek + daysInMonth m ((1900 + y) |> int64))
        result

    /// inclusive
    let countSundaysUntil day month year =
        let mutable dayOfWeek = 0 // monday
        let mutable result = 0L
        for y in 0..year do
            let tillMonth = if y = year then month else 12
            for m in 1..tillMonth do
                if dayOfWeek = 6 then result <- result + 1L
                dayOfWeek <- modulo 7 (dayOfWeek + daysInMonth m ((1900 + y) |> int64))
        result

    let solveOne() = 
        let mutable yearFrom = sc.NextLong().Value - 1900L
        let mutable monthFrom = sc.NextInt().Value 
        let mutable dayFrom = sc.NextInt().Value - 1
        if dayFrom < 1 then
            monthFrom <- monthFrom - 1
            if monthFrom < 1 then
                monthFrom <- 12
                yearFrom <- yearFrom - 1L
            dayFrom <- daysInMonth monthFrom yearFrom

        let yearTo = sc.NextLong().Value - 1900L
        let monthTo = sc.NextInt().Value
        let dayTo = sc.NextInt().Value

//        printfn "From: %i %i %i" dayFrom monthFrom yearFrom
//        printfn "To: %i %i %i" dayTo monthTo yearTo

        let result1 = countSundays400 * (yearFrom / 400L) + (countSundaysUntil dayFrom monthFrom ((yearFrom % 400L) |> int))
        let result2 = countSundays400 * (yearTo / 400L) + (countSundaysUntil dayTo monthTo ((yearTo % 400L) |> int))
        let result = result2 - result1
//        printfn "%i - %i =  %i" result2 result1 result
        printfn "%i" result

    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
