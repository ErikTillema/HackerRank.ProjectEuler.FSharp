namespace HackerRank.FSharp

module CountingRectangles =

    open Util

    let sc = Scanner()

    let getCount i = i*(i+1)/2
    let vals() = Seq.initInfinite (fun i -> (i+1, getCount (i+1)) )

    let solveOne() = 
        let target = sc.NextInt().Value
        let getL2 cnt1 = 
            let a = (float target)/(float cnt1)
            let b = sqrt(2.0*a + 0.25) - 0.5
            [ (b |> ceil |> int); (b |> floor |> int) ] // first higher value before lower, to make sure we don't stop the takeWhile too soon.
        let l1,_,l2,_ = 
            vals() 
                |> Seq.collect (fun (l1,cnt1) -> getL2 cnt1 |> Seq.map (fun l2 -> (l1,cnt1,l2,getCount l2)) )
                |> Seq.takeWhile (fun (l1,_,l2,_) -> l1 <= l2)
                |> Seq.maxBy (fun (l1,cnt1,l2,cnt2) -> (-abs((int64 cnt1)*(int64 cnt2) - (int64 target)), l1*l2) )
        printfn "%d" (l1*l2)
        ()

    let solve() =
        let n = sc.NextInt().Value
        for i in 1..n do solveOne()
        ()