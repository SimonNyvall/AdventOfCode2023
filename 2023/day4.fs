module Day4.Part1

open System
open System.IO


type Card =
    { CardIndex: string
      WinningNumbers: int list
      GivenNumbers: int list }


type IntersectionCard =
    { CardIndex: string
      Intersection: int list }


let input =
    File.ReadAllLines "input/input4.txt"
    |> Array.toList


let parseCard (line: string) =
    let cardIndex = line.Split(':').[0].Trim()

    let winningNumbers =
        line.Split(':').[1].Split('|').[0].Trim().Split(' ').[0..]
        |> Array.filter (fun x -> x <> "")

    let givenNumbers =
        line.Split(':').[1].Split('|').[1].Trim().Split(' ').[0..]
        |> Array.filter (fun x -> x <> "")

    { CardIndex = cardIndex
      WinningNumbers = winningNumbers |> Array.map int |> Array.toList
      GivenNumbers = givenNumbers |> Array.map int |> Array.toList }


let getIntersection (card: Card) =
    let intersections =
        card.WinningNumbers
        |> List.filter (fun x -> card.GivenNumbers |> List.contains x)

    if intersections |> List.length > 0 then
        Some
            { CardIndex = card.CardIndex
              Intersection = intersections }
    else
        None


let calculateCardScore (card: IntersectionCard) =
    Math.Pow(2.0, (float card.Intersection.Length) - 1.0)


let Solve =
    input
    |> List.map parseCard
    |> List.map getIntersection
    |> List.filter (fun x -> x.IsSome)
    |> List.map (fun x -> x.Value)
    |> List.map calculateCardScore
    |> List.sum
    |> int
    |> printfn "%d"
