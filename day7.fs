module Day7.Part1

open System
open System.IO

let example =
    """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
  """
        .Split('\n')
    |> Array.toList
    |> List.filter (fun s -> s <> "")
    |> List.map (fun s -> s.Split(' '))
    |> List.rev
    |> List.skip 1
    |> List.rev


type Player = { Hand: char list; Bid: int64 }


type PlayType =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard


let parseHand (input: string array list) =
    let bid (arr: string array) = arr |> Array.rev |> Array.head |> int64

    let hand (arr: string array) =
        arr
        |> Array.head
        |> fun s -> s.ToCharArray()
        |> Array.toList

    input
    |> List.map (fun arr -> { Hand = hand arr; Bid = bid arr })


let comparePlayType (pt1: PlayType) (pt2: PlayType) =
    match pt1, pt2 with
    | _ when pt1 = pt2 -> 0
    | FiveOfAKind, _ -> 1
    | _, FiveOfAKind -> -1
    | FourOfAKind, _ -> 1
    | _, FourOfAKind -> -1
    | FullHouse, _ -> 1
    | _, FullHouse -> -1
    | ThreeOfAKind, _ -> 1
    | _, ThreeOfAKind -> -1
    | TwoPair, _ -> 1
    | _, TwoPair -> -1
    | OnePair, _ -> 1
    | _, OnePair -> -1
    | HighCard, _ -> 1
    | _, HighCard -> -1


let cardStrength card =
    match card with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | _ when '2' <= card && card <= '9' -> int card - int '0'
    | _ -> 0


let classifyHand (hand: char list) =
    let grouped =
        hand
        |> List.groupBy id
        |> List.map (fun (k, v) -> (k, List.length v))
        |> List.sortByDescending (fun (_, count) -> count)

    match grouped with
    | (k, 5) :: _ -> (FiveOfAKind, grouped)
    | (k, 4) :: _ -> (FourOfAKind, grouped)
    | (k, 3) :: (k', 2) :: _ -> (FullHouse, grouped)
    | (k, 3) :: _ -> (ThreeOfAKind, grouped)
    | (k, 2) :: (k', 2) :: _ -> (TwoPair, grouped)
    | (k, 2) :: _ -> (OnePair, grouped)
    | _ -> (HighCard, grouped)


let compareCards card1 card2 = cardStrength card1 - cardStrength card2


let compareHands (hand1: char list) (hand2: char list) =
    let rank1, _ = classifyHand hand1
    let rank2, _ = classifyHand hand2

    match comparePlayType rank1 rank2 with
    | 0 -> Seq.compareWith compareCards hand1 hand2
    | c -> c


let rankHands (players: Player list) =
    players
    |> List.sortWith (fun p1 p2 -> compareHands p1.Hand p2.Hand)
    |> List.mapi (fun index player -> (index + 1, player))


let calculateWinnings (players: (int * Player) list) =
    players
    |> List.mapi (fun index (rank, player) -> (rank, player.Bid))
    |> List.map (fun (rank, bid) -> bid * int64 rank)
    |> List.sum


let sovle =
    File.ReadAllLines("input/input7.txt")
    |> Array.toList
    |> List.map (fun s -> s.Split(' '))
    |> parseHand
    |> rankHands
    |> calculateWinnings
