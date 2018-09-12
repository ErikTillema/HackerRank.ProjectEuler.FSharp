namespace HackerRank.FSharp

module OrderedFractions =

    open Util

    let sc = Scanner()
    
    let getMedian (a,b) (c,d) = (a+c,b+d)
    let (>?) (a,b) (c,d) = a*d > b*c
    let (<?) (a,b) (c,d) = a*d < b*c

    let binarySearchSternBrocotTree target = 
        let rec binarySearchSternBrocotTree' left right target = 
            let median = getMedian left right
            if median <? target then
                binarySearchSternBrocotTree' median right target
            elif median >? target then
                binarySearchSternBrocotTree' left median target
            else
                left
        binarySearchSternBrocotTree' (0L,1L) (1L,0L) target

    /// See Farey sequence https://en.wikipedia.org/wiki/Farey_sequence
    /// and Stern-Brocot tree https://en.wikipedia.org/wiki/Stern%E2%80%93Brocot_tree
    /// We can traverse the Stern-Brocot tree up to level n, keeping the left L and right bound R,
    /// starting with L=0/1, R=1/0, until we find a/b. 
    /// Then, we keep traversing down going from a/b left once, then right, until level n.
    /// 2/5 + 3/7 -> 5/12 + 3/7 -> 8/19 + 3/7 -> 11/26 + ...
    /// (2+3m)/(5+7m)
    /// 5+7m <= N  =>  m <= (N-5)/7
    let solveOne() = 
        let a = sc.NextLong().Value
        let b = sc.NextLong().Value
        let N = sc.NextLong().Value
        let target = (a,b)
        let (leftN,leftD) = binarySearchSternBrocotTree target
        let m = (N-leftD)/b
        let (resultN, resultD) = (leftN + m*a, leftD + m*b)
        printfn "%d %d" resultN resultD
        ()

    let solve() = 
        let n = sc.NextInt().Value
        for _ in 1..n do solveOne()

