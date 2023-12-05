module Day2.Part2

open System.IO
open System

type Color =
    | Red
    | Green
    | Blue


type Round = { CubeCount: int; Color: Color }


type Game = { Rounds: Round list list }


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


let parseGame (games: string []) =
    games
    |> Array.map (fun gameStr ->
        let parts = gameStr.Split(':')
        let roundStrs = parts.[1].Trim().Split(';')
        let rounds = roundStrs |> Array.map parseRound |> Array.toList
        { Rounds = rounds })


let maxCubesPerColor (rounds: Round list) : int * int * int =
    let redMax =
        rounds
        |> List.filter (fun r -> r.Color = Red)
        |> List.maxBy (fun r -> r.CubeCount)
        |> fun r -> r.CubeCount

    let greenMax =
        rounds
        |> List.filter (fun r -> r.Color = Green)
        |> List.maxBy (fun r -> r.CubeCount)
        |> fun r -> r.CubeCount

    let blueMax =
        rounds
        |> List.filter (fun r -> r.Color = Blue)
        |> List.maxBy (fun r -> r.CubeCount)
        |> fun r -> r.CubeCount

    (redMax, greenMax, blueMax)


let powerOfGame (game: Game) : int =
    let (red, green, blue) = game.Rounds |> List.concat |> maxCubesPerColor
    red * green * blue


let sumOfPowers (games: Game array) : int =
    games |> Array.map powerOfGame |> Array.sum


let games = parseGame readInputFile
let solve = sumOfPowers games
