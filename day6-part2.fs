module Day6

open System
open System.IO


let example =
    """
Time:      7  15   30
Distance:  9  40  200
  """
        .Split('\n')
    |> Array.toList
    |> List.map (fun s -> s.Trim())
    |> List.filter (fun s -> s <> "")


type Time = int64


type Distance = int64


type Race = { Time: Time; Distance: Distance }


type RaceOption = { Race: Race; Outcome: int64 list }


let breakOutList (input: string list) =
    input
    |> List.head
    |> fun s -> s.Split(':')
    |> Array.toList
    |> List.map (fun s -> s.Trim())
    |> List.rev
    |> List.head
    |> fun s -> s.Trim()
    |> fun s -> s.Replace(" ", "")


let parseInput (input: string list) =

    let time = input |> breakOutList

    let distance = input |> List.rev |> breakOutList

    { Time = int64 time
      Distance = int64 distance }


let calculateWinningWays (race: Race) =
    let a = 1 |> float
    let b = -race.Time |> float
    let c = race.Distance |> float

    let discriminant = b * b - 4.0 * a * c

    let root1 =
        (-b + Math.Sqrt(float discriminant))
        / (2. * float a)

    let root2 =
        (-b - Math.Sqrt(float discriminant))
        / (2. * float a)

    let lowerBound = Math.Ceiling(root2)
    let upperBound = Math.Floor(root1)

    if lowerBound > upperBound then
        0
    else
        int (upperBound - lowerBound + 1.0)


let solve =
    File.ReadAllLines("input/input6.txt")
    |> Array.toList
    |> List.map (fun s -> s.Trim())
    |> List.filter (fun s -> s <> "")
    |> parseInput
    |> calculateWinningWays
