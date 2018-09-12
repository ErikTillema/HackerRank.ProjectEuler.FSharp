namespace HackerRank.FSharp

module MonopolyOdds =

    open Util

    let sc = Scanner()

    let square = [| "GO";   "A1"; "CC1"; "A2";  "T1"; "R1"; "B1";  "CH1"; "B2"; "B3";
                    "JAIL"; "C1"; "U1";  "C2";  "C3"; "R2"; "D1";  "CC2"; "D2"; "D3";
                    "FP";   "E1"; "CH2"; "E2";  "E3"; "R3"; "F1";  "F2";  "U2"; "F3";
                    "G2J";  "G1"; "G2";  "CC3"; "G3"; "R4"; "CH3"; "H1";  "T2"; "H2" |]

    type Move = 
        | AbsoluteMove of int
        | RelativeMove of int
        | MoveToNextR
        | MoveToNextU

    let moves pos = 
        match square.[pos] with
        | "G2J" -> [ (1., AbsoluteMove(10)) ]
        | "CC1" | "CC2" | "CC3" -> [    (14./16., RelativeMove(0)); 
                                        (1./16., AbsoluteMove(0)); 
                                        (1./16., AbsoluteMove(10)) ]
        | "CH1" | "CH2" | "CH3" -> [    (6./16., RelativeMove(0)); 
                                        (1./16., AbsoluteMove(0)); 
                                        (1./16., AbsoluteMove(10)); 
                                        (1./16., AbsoluteMove(11)); 
                                        (1./16., AbsoluteMove(24)); 
                                        (1./16., AbsoluteMove(39)); 
                                        (1./16., AbsoluteMove(5)); 
                                        (2./16., MoveToNextR); 
                                        (1./16., MoveToNextU); 
                                        (1./16., RelativeMove(37)) ]
        | _ -> [ (1., RelativeMove(0)) ]

    let solve() =
        let n = sc.NextInt().Value
        let k = sc.NextInt().Value

        let getRolls() =
            let rec getP roll =
                if roll <= n+1 then
                    (float (roll-1)) / (float (n*n))
                else
                    getP (2*n + 2 - roll)
            seq { 2..2*n } |> Seq.map (fun roll -> (roll, getP roll))
        
        let mutable p = Array.create 40 (1./39.)
        p.[30] <- 0. // G2J

        // calculate new set of p's by considering each square, each roll and each move from the result square (stay, move to another square)
        // then replace p by new set of p and 
        let rec refine() = 
            let newP = Array.create 40 0.

            for from in 0..39 do
                for (roll, pRoll) in getRolls() do
                    let too = (from + roll) % 40
                    for (pMove, move) in moves too do
                        let addedP = p.[from] * pRoll * pMove
                        match move with
                        | AbsoluteMove(x) ->
                            newP.[x] <- newP.[x] + addedP
                        | RelativeMove(dx) -> 
                            let x = (too + dx) % 40
                            if dx <> 0 && x = 33 then // hacky way to solve that CH3 might lead to CC3
                                newP.[x] <- newP.[x] + addedP * 14./16.
                                newP.[ 0] <- newP.[ 0] + addedP *  1./16.
                                newP.[10] <- newP.[10] + addedP *  1./16.
                            else 
                                newP.[x] <- newP.[x] + addedP
                        | MoveToNextR -> 
                                let x = if    5 <= too && too < 15 then 15
                                        elif 15 <= too && too < 25 then 25
                                        elif 25 <= too && too < 35 then 35
                                        else 5
                                newP.[x] <- newP.[x] + addedP
                        | MoveToNextU -> 
                                let x = if 12 <= too && too < 28 then 28 
                                        else 12
                                newP.[x] <- newP.[x] + addedP
            
            let sump = newP |> Array.sum
            //if abs(1.0 - sump) > 0.000_001 then invalidOp "huh"

            let hasChanged (old, neww) = abs(old - neww) > 0.000_000_01
            let hasChanges = Array.zip p newP |> Array.exists hasChanged
            if hasChanges then 
                p <- newP
                refine()
        
        refine()

        let result = [0..39] |> List.sortBy (fun i -> -p.[i]) |> List.take k |> List.map (fun i -> square.[i]) |> String.concat " "
        printfn "%s" result
        ()