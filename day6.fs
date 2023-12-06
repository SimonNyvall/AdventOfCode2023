module Day6

open System
open System.IO


let example = 
  """
Time:      7  15   30
Distance:  9  40  200
  """.Split('\n')
  |> Array.toList
  |> List.map (fun s -> s.Trim())
  |> List.filter (fun s -> s <> "")
  

type Time = int


type Distance = int


type Race = {
  Time: Time;
  Distance: Distance;
}


type RaceOption = {
  Race: Race
  Outcome: int list
}


let breakOutList (input: string list): int list =
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

  let timeArr =
    input |> breakOutList
    
  let distanceArr =
    input |> List.rev |> breakOutList
 
  let zipLists =
    List.zip timeArr distanceArr

  zipLists
  |> List.map (fun (time, distance) -> {Time = time; Distance = distance})


let holdTimeOptions (holdTime: int): int list =
  [0 .. holdTime]


let calculateRaceOptions (races: Race list) =
  races
  |> List.map (fun race -> 
      let outcomes = holdTimeOptions race.Time
      let lastElement = outcomes |> List.rev |> List.head
      let outcomeList = outcomes |> List.map (fun holdTime -> holdTime * (lastElement - holdTime))
      { Race = race; Outcome = outcomeList }
    )


let result (raceOptions: RaceOption list) =
  raceOptions
  |> List.map (fun race ->
    let raceDistance = race.Race.Distance
    race.Outcome
    |> List.filter (fun outcome -> outcome > raceDistance) 
    
  )
  

let solve =
  File.ReadAllLines("input/input6.txt")
  |> Array.toList
  |> List.map (fun s -> s.Trim())
  |> List.filter (fun s -> s <> "")
  |> parseInput 
  |> calculateRaceOptions 
  |> result
  |> List.map (fun l -> l |> List.length)
  |> List.fold (fun acc n -> acc * n) 1

