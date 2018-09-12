namespace HackerRank.FSharp

module PokerHands =

    open Util
    open System

    /// returns whether all items in source are equal
    let allEqual source =
        match Seq.tryHead source with
        | None -> true
        | Some(h) -> source |> Seq.forall ((=) h)

    type Color =
        | Black
        | Red

    type Suit = 
        | Diamonds = 0
        | Clubs = 1
        | Hearts = 2
        | Spades = 3

    let getColor s = 
        match s with
        | Suit.Hearts | Suit.Diamonds -> Red
        | _ -> Black

    type Rank = 
        | Two = 2 
        | Three = 3 
        | Four = 4 
        | Five = 5 
        | Six = 6 
        | Seven = 7 
        | Eight = 8 
        | Nine = 9 
        | Ten = 10
        | Jack = 11 
        | Queen = 12 
        | King = 13
        | Ace = 14

    type Card = Rank * Suit

    type Hand = Card list

    type HandValue = 
        | HighCard of Rank list
        | Pair of Rank * Rank list
        | TwoPair of Rank * Rank * Rank list
        | ThreeOfAKind of Rank * Rank list
        | Straight of Rank
        | Flush of Rank list
        | FullHouse of Rank * Rank
        | FourOfAKind of Rank * Rank list
        | StraightFlush of Rank
        
    let handValue (hand: Hand) = 
        let isFlush hand =
            hand |> List.map (fun (r,s) -> s) |> allEqual
        let isWheel hand = 
            match hand with
            | [(Rank.Two,_); (Rank.Three,_); (Rank.Four,_); (Rank.Five,_); (Rank.Ace,_)] -> true
            | _ -> false
        let isStraight hand =
            let rec isSuccessive cards = 
                match cards with
                | (r1,_)::(r2,s2)::rest -> int(r1) + 1 = int(r2) && isSuccessive ((r2,s2)::rest)
                | [_] | [] -> true
            match hand with
            | _ -> isSuccessive hand

        match hand with
        | [(r1,s1); (r2,s2); (r3,s3); (r4,s4); (r5,s5)] ->
            match (r1,s1), (r2,s2), (r3,s3), (r4,s4), (r5,s5) with
            | _ when isFlush hand && isWheel hand -> StraightFlush Rank.Five
            | _ when isFlush hand && isStraight hand -> StraightFlush r5
            | _ when allEqual [r1; r2; r3; r4] -> FourOfAKind (r1, [r5])
            | _ when allEqual [r2; r3; r4; r5] -> FourOfAKind (r2, [r1])
            | _ when allEqual [r1; r2; r3] && r4 = r5 -> FullHouse (r1, r4)
            | _ when r1 = r2  && allEqual [r3; r4; r5] -> FullHouse (r3, r1)
            | _ when isFlush hand -> Flush (hand |> List.map (fun (r,s) -> r) |> List.rev)
            | _ when isWheel hand -> Straight Rank.Five
            | _ when isStraight hand -> Straight r5
            | _ when allEqual [r1; r2; r3] -> ThreeOfAKind (r1, [r5;r4])
            | _ when allEqual [r2; r3; r4] -> ThreeOfAKind (r2, [r5;r1])
            | _ when allEqual [r3; r4; r5] -> ThreeOfAKind (r3, [r2;r1])
            | _ when (r1 = r2 && r3 = r4) -> TwoPair (r3, r1, [r5])
            | _ when (r1 = r2 && r4 = r5) -> TwoPair (r4, r1, [r3])
            | _ when (r2 = r3 && r4 = r5) -> TwoPair (r4, r2, [r1])            
            | _ when r1 = r2 -> Pair (r1, [r5;r4;r3])
            | _ when r2 = r3 -> Pair (r2, [r5;r4;r1])
            | _ when r3 = r4 -> Pair (r3, [r5;r2;r1])
            | _ when r4 = r5 -> Pair (r4, [r3;r2;r1])
            | _ -> HighCard (hand |> List.map (fun (r,s) -> r) |> List.rev)
        | _ -> invalidOp "Hand should have exactly 5 cards"

    let sc = Scanner()

    let parseHand input =
        let (|ParseSuite|) c = 
            match c with 
            | 'H' -> Suit.Hearts
            | 'C' -> Suit.Clubs
            | 'S' -> Suit.Spades
            | 'D' -> Suit.Diamonds
            | _ -> invalidOp "bad suite"

        let (|ParseRank|) c = 
            match c with 
            | 'T' -> Rank.Ten
            | 'J' -> Rank.Jack
            | 'Q' -> Rank.Queen
            | 'K' -> Rank.King
            | 'A' -> Rank.Ace
            | '2' -> Rank.Two
            | '3' -> Rank.Three
            | '4' -> Rank.Four
            | '5' -> Rank.Five
            | '6' -> Rank.Six
            | '7' -> Rank.Seven
            | '8' -> Rank.Eight
            | '9' -> Rank.Nine

        let (|ParseCard|) (s: String) = 
            match s.ToCharArray() with
            | [| ParseRank rank; ParseSuite suite |] -> rank,suite
        
        match input with 
        | [ ParseCard card1; ParseCard card2; ParseCard card3; ParseCard card4; ParseCard card5 ] -> [card1;card2;card3;card4;card5] |> List.sort

    let solveOne() = 
        let hand1 = List.init 5 (fun _ -> sc.Next().Value) |> parseHand |> handValue
        let hand2 = List.init 5 (fun _ -> sc.Next().Value) |> parseHand |> handValue
        if hand1 > hand2 then
            printfn "Player 1"
        else
            printfn "Player 2"
 
    let solve() = 
        let n = sc.NextInt().Value
        for i in 1..n do
            solveOne()
        ()
    