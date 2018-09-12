namespace HackerRank.FSharp

module MagicNGonRing =

    open Util

    let sc = Scanner()

    let sumRange a b = 
        let sumRangeFromOne a = a*(a+1)/2
        (sumRangeFromOne b) - (sumRangeFromOne (a-1))

    let solve() = 
        let n = sc.NextInt().Value
        let s = sc.NextInt().Value

        // let's first find out which numbers we can put in the outer ring, such that the outer and inner ring values
        // add up to n * s
        // outer ring values count once, inner ring values count twice.
        // total inner ring value = total value - total outer ring value
        // total value = 2n*(2n+1)/2
        // => n*s = outer + 2*inner = outer + 2*(total - outer) = 2*total - outer = 2n*(2n+1) - outer
        // => outer = n*(4n+2-s)
        // So, find all values which add up to n*(4n+2-s). The smallest value is at index 0 of the outer ring.
        // Then, we know all values for the inner ring as well.
        // We continue by filling in the inner ring, first at index 0. Then we know  the inner ring value at index 1 as well.
        // Then we start searching: fill in inner values 2..n-1 and check if the outer ring value exists.

        let outerSum = n*(4*n+2-s)

        let rec findOuter don sum maxValueUsed valuesUsed =
            seq {
                if don = n then
                    yield List.rev valuesUsed |> List.toArray
                else
                    let todo = n - don
                    //let minSumRest = sumRange (maxValueUsed+1) (maxValueUsed+todo)
                    //let maxSumRest = sumRange (2*n-todo+1) (2*n)
                    for i in maxValueUsed+1 .. 2*n-todo+1 do
                        let todo2 = todo - 1
                        let minSumRest = sumRange (i+1) (i+todo2)
                        let maxSumRest = sumRange (2*n-todo2+1) (2*n)
                        if sum+i+minSumRest <= outerSum && outerSum <= sum+i+maxSumRest then
                            yield! findOuter (don+1) (sum+i) i (i::valuesUsed)
            }
        
        let innerValues = Array.create n 0

        let rec fill i innerValuesLeft (outerIsAvailable: bool array) =
            seq {
                if i = n then
                    // check last one
                    let outerValue = s - innerValues.[0] - innerValues.[n-1]
                    if outerIsAvailable.[outerValue] then
                        yield innerValues |> Array.copy
                else
                    for innerValue in innerValuesLeft do
                        let outerValue = s - innerValue - innerValues.[i-1]
                        if 1 <= outerValue && outerValue <= 2*n && outerIsAvailable.[outerValue] then
                            outerIsAvailable.[outerValue] <- false
                            innerValues.[i] <- innerValue
                            yield! fill (i+1) (innerValuesLeft |> Set.remove innerValue) outerIsAvailable
                            outerIsAvailable.[outerValue] <- true
            }

        let allResults = 
            seq {
                for outer in findOuter 0 0 0 [] do
                    let outerIsAvailable = Array.create (2*n+1) false
                    outer |> Array.iter (fun v -> outerIsAvailable.[v] <- true)
                    let innerValuesLeft = Set.difference ([1 .. 2*n] |> Set.ofList) (outer |> Set.ofArray)
                    for i0 in innerValuesLeft do
                        // fill in i0 at 0 and then we know also i1 at 1.
                        let o0 = outer.[0]
                        let i1 = s - o0 - i0
                        if i1 <> i0 && Set.contains i1 innerValuesLeft then
                            outerIsAvailable.[o0] <- false
                            innerValues.[0] <- i0
                            innerValues.[1] <- i1
                            yield! fill 2 (innerValuesLeft |> Set.remove i0 |> Set.remove i1) outerIsAvailable
            }

        let getString (inner: int array) = 
            let getOuter i = s - inner.[i] - inner.[(i+1)%n]
            let getValues i = [ getOuter i; inner.[i]; inner.[(i+1)%n] ]
            [0..n-1] |> Seq.collect getValues |> Seq.map string |> String.concat ""
                    
        let result = allResults |> Seq.map getString |> Seq.sort |> String.concat "\n"
        printfn "%s" result
        ()
