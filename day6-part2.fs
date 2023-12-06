module Day6

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


let holdTimeOptions (holdTime: int64) : int64 list = [ 0..holdTime ]


let calculateRaceOptions (race: Race) =

    let outcomes = holdTimeOptions race.Time
    let lastElement = outcomes |> List.rev |> List.head

    let outcomeList =
        outcomes
        |> List.map (fun holdTime -> holdTime * (lastElement - holdTime))

    { Race = race; Outcome = outcomeList }


let result (race: RaceOption) =
    let raceDistance = race.Race.Distance

    race.Outcome
    |> List.filter (fun outcome -> outcome > raceDistance)


let solve =
    File.ReadAllLines("input/input6.txt")
    |> Array.toList
    |> List.map (fun s -> s.Trim())
    |> List.filter (fun s -> s <> "")
    |> parseInput
    |> calculateRaceOptions
    |> result
    |> List.length
