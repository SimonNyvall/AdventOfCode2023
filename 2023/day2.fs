module Day2.Part1

open System.IO
open System

type Color =
    | Red
    | Green
    | Blue


type Round = { CubeCount: int; Color: Color }


type Game =
    { GameIndex: string
      Rounds: Round list }


let readInputFile = File.ReadAllLines("input/input2.txt")


let parseColor (colorStr: string) : Color =
    match colorStr.Trim().ToLower() with
    | "red" -> Red
    | "green" -> Green
    | "blue" -> Blue
    | _ -> failwith "Invalid color"


let parseRound (roundStr: string) : Round list =
    roundStr.Split(',')
    |> Array.map (fun x ->
        let parts = x.Trim().Split(' ')
        let cubeCount = int parts.[0]
        let color = parseColor parts.[1]
        { CubeCount = cubeCount; Color = color })
    |> Array.toList


let parseGame (games: string []) : Game list =
    games
    |> Array.map (fun gameStr ->
        let parts = gameStr.Split(':')
        let gameIndex = parts.[0].Trim()
        let roundsStr = parts.[1].Trim()
        let rounds = roundsStr.Split(';') |> Array.toList
        let parsedRounds = List.collect parseRound rounds

        { GameIndex = gameIndex
          Rounds = parsedRounds })
    |> Array.toList


let playAmount: Round list =
    [ { CubeCount = 12; Color = Red }
      { CubeCount = 13; Color = Green }
      { CubeCount = 14; Color = Blue } ]


let gamesPossibleToPlay (games: Game list) : Game list =
    let getPlayAmountOfColor (color: Color) : int =
        playAmount
        |> List.tryFind (fun round -> round.Color = color)
        |> function
            | Some (round) -> round.CubeCount
            | None -> 0

    games
    |> List.filter (fun game ->
        game.Rounds
        |> List.forall (fun round ->
            match round.Color with
            | Red -> round.CubeCount <= getPlayAmountOfColor Red
            | Green -> round.CubeCount <= getPlayAmountOfColor Green
            | Blue -> round.CubeCount <= getPlayAmountOfColor Blue))


let sumGameIndexes (games: Game list) : int =
    games
    |> List.sumBy (fun game ->
        let idStr = game.GameIndex.Split(' ')[1]
        int idStr)


let parsedGames = parseGame readInputFile
let possibleGames = gamesPossibleToPlay parsedGames
let solve = sumGameIndexes possibleGames
