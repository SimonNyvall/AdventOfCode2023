module Day4.Part2

open System.IO


type Card =
    { CardIndex: int
      WinningNumbers: int list
      GivenNumbers: int list }


let input =
    File.ReadAllLines "input/input4.txt"
    |> Array.toList


let parseCard (line: string) =
    let cardIndex =
        line.Split(':').[0].Trim().Split(' ')
        |> Array.filter (fun x -> x <> "")
        |> Array.last
        |> int

    let winningNumbers =
        line.Split(':').[1].Split('|').[0].Trim().Split(' ').[0..]
        |> Array.filter (fun x -> x <> "")

    let givenNumbers =
        line.Split(':').[1].Split('|').[1].Trim().Split(' ').[0..]
        |> Array.filter (fun x -> x <> "")

    { CardIndex = cardIndex
      WinningNumbers = winningNumbers |> Array.map int |> Array.toList
      GivenNumbers = givenNumbers |> Array.map int |> Array.toList }


let points (card: Card) =
    let intersections =
        card.WinningNumbers
        |> List.filter (fun x -> card.GivenNumbers |> List.contains x)

    intersections |> List.length


type CardId = int


type State =
    { Cards: Map<CardId, Card>
      NumberOfCopies: Map<CardId, int> }


let Solve =
    let parsed = input |> List.map parseCard


    let initalState =
        { Cards =
            parsed
            |> List.map (fun c -> (c.CardIndex, c))
            |> Map.ofList
          NumberOfCopies =
            parsed
            |> List.map (fun c -> (c.CardIndex, 1))
            |> Map.ofSeq }


    let scoreAndCopy (state: State) card =
        let p = points card
        let nbCopies = state.NumberOfCopies |> Map.find card.CardIndex

        let idsToCopy = [ (card.CardIndex + 1) .. (card.CardIndex + p) ]

        let bumpCount (state: State) id =
            { state with
                NumberOfCopies =
                    state.NumberOfCopies
                    |> Map.change id (fun (Some count) -> Some(count + nbCopies)) }

        let newCounts = idsToCopy |> List.fold bumpCount state


        newCounts

    let folded = parsed |> List.fold scoreAndCopy initalState

    folded.NumberOfCopies
    |> Map.toList
    |> List.sumBy snd

Solve |> printfn "%A"
