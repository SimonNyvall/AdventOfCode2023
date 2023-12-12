module Day6.Part1

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


type Time = int


type Distance = int


type Race = { Time: Time; Distance: Distance }


type RaceOption = { Race: Race; Outcome: int list }


let breakOutList (input: string list) : int list =
    input
    |> List.head
    |> fun s -> s.Split(':')
    |> Array.toList
    |> List.map (fun s -> s.Trim())
    |> List.rev
    |> List.head
    |> fun s -> s.Trim()
    |> fun s -> s.Split(' ')
    |> Array.toList
    |> List.filter (fun s -> s <> "")
    |> List.map (fun s -> int s)


let parseInput (input: string list) =

    let timeArr = input |> breakOutList

    let distanceArr = input |> List.rev |> breakOutList

    let zipLists = List.zip timeArr distanceArr

    zipLists
    |> List.map (fun (time, distance) -> { Time = time; Distance = distance })


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
    |> List.map calculateWinningWays
    |> List.fold (*) 1
    |> printfn "%d"
