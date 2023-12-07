module Day7.Part2

open System
open System.IO

// Your example input
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


let cardStrength card isJoker =
    match card with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' when not isJoker -> 11
    | 'T' -> 10
    | _ when '2' <= card && card <= '9' -> int card - int '0'
    | 'J' -> 1 // Joker is weakest when not acting as a wildcard
    | _ -> 0


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

let classifyWithJokers cards : char list list =
    let cardsWithoutJokers =
        [ 'A'
          'K'
          'Q'
          'T'
          '9'
          '8'
          '7'
          '6'
          '5'
          '4'
          '3'
          '2' ]

    let explodeJokers cards card =
        match card with
        | 'J' ->
            cardsWithoutJokers
            |> List.collect (fun card -> cards |> List.map (fun cs -> card :: cs))
        | _ -> cards |> List.map (fun cs -> card :: cs)

    cards
    |> List.fold (fun s c -> explodeJokers s c) [ [] ]
    |> List.map List.rev
    |> List.distinct


let groupCardsByRank hand =
    hand
    |> List.groupBy id
    |> List.map (fun (card, occurrences) -> (card, List.length occurrences))
    |> List.sortByDescending snd


let isNOfAKind n groups =
    groups
    |> List.exists (fun (_, count) -> count = n)


let isTwoPairs groups =
    groups
    |> List.filter (fun (_, count) -> count = 2)
    |> List.length = 2


let isFullHouse groups =
    isNOfAKind 3 groups && isNOfAKind 2 groups


let handPlayType (hand: char list) : PlayType =
    let groups = groupCardsByRank hand

    match groups with
    | (card, 5) :: _ -> FiveOfAKind
    | (card, 4) :: _ -> FourOfAKind
    | _ when isFullHouse groups -> FullHouse
    | _ when isNOfAKind 3 groups -> ThreeOfAKind
    | _ when isTwoPairs groups -> TwoPair
    | _ when isNOfAKind 2 groups -> OnePair
    | _ -> HighCard


let classifyHand (hand: char list) : PlayType =
    hand
    |> classifyWithJokers
    |> List.map handPlayType
    |> List.max


let compareHands (hand1: char list) (hand2: char list) =
    let rank1 = classifyHand hand1
    let rank2 = classifyHand hand2

    match comparePlayType rank1 rank2 with
    | 0 ->
        Seq.compareWith
            (fun c1 c2 ->
                cardStrength c1 (c1 = 'J')
                - cardStrength c2 (c2 = 'J'))
            hand1
            hand2
    | c -> c

let parseHand (input: string array list) =
    let bid (arr: string array) = arr |> Array.rev |> Array.head |> int64

    let hand (arr: string array) =
        arr
        |> Array.head
        |> fun s -> s.ToCharArray()
        |> Array.toList

    input
    |> List.map (fun arr -> { Hand = hand arr; Bid = bid arr })

let rankHands (players: Player list) =
    players
    |> List.sortWith (fun p1 p2 -> compareHands p1.Hand p2.Hand)
    |> List.mapi (fun index player -> (index + 1, player))

let calculateWinnings (players: (int * Player) list) =
    players
    |> List.map (fun (rank, player) -> player.Bid * int64 rank)
    |> List.sum


let sovle =
    File.ReadAllLines("input/input7.txt")
    |> Array.toList
    |> List.map (fun s -> s.Split(' '))
    |> parseHand
    |> rankHands
    |> calculateWinnings
