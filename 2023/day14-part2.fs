module Day14part2

open System
open System.Collections.Generic

let example =
    """O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."""


let parseInput (input: string) =
    input
    |> fun s -> s.Split('\n')
    |> Array.toList
    |> List.map (fun s -> s.ToCharArray() |> Array.toList)


let input =
    System.IO.File.ReadAllText "2023/input/input14.txt"
    |> parseInput


let tiltSideWays (input: char list list) =
    let padChar = '.'

    let maxLength = input |> List.map List.length |> List.max

    let padListToLength list =
        list
        @ List.replicate (maxLength - List.length list) padChar

    let paddedInput = input |> List.map padListToLength
    List.transpose paddedInput


let concatCharArrayToString (input: char list list) : string list =
    input
    |> List.map (fun arr ->
        arr
        |> List.map (fun c -> c.ToString())
        |> String.concat "")


let stringToCharArrays (input: string list) =
    input
    |> List.map (fun s -> s.ToCharArray() |> Array.toList)


let slideStonesLeft (input: char list list) : char list list =
    let pushOsToLeft (str: string) : string =
        let parts = str.Split([| '#' |], StringSplitOptions.None)

        let processPart (part: string) : string =
            let oCount =
                part
                |> Seq.filter (fun c -> c = 'O')
                |> Seq.length

            String.replicate oCount "O"
            + String.replicate (part.Length - oCount) "."

        let processedParts: string [] = parts |> Array.map processPart
        String.Join("#", processedParts)

    input
    |> concatCharArrayToString
    |> List.map pushOsToLeft
    |> stringToCharArrays


let slideStonesRight (input: char list list) : char list list =
    let pushOsToRight (str: string) : string =
        let parts = str.Split([| '#' |], StringSplitOptions.None)

        let processPart (part: string) : string =
            let oCount =
                part
                |> Seq.filter (fun c -> c = 'O')
                |> Seq.length

            String.replicate (part.Length - oCount) "."
            + String.replicate oCount "O"

        let processedParts: string [] = parts |> Array.map processPart
        String.Join("#", processedParts)

    input
    |> concatCharArrayToString
    |> List.map pushOsToRight
    |> stringToCharArrays


let lengthOfDish =
    (input
     |> tiltSideWays
     |> List.map (fun s -> s.Length)
     |> List.max)


let mapPoints (input: string list) =
    let mapi =
        input
        |> List.map (fun x ->
            x.ToCharArray()
            |> Array.mapi (fun i c -> (System.Math.Abs(lengthOfDish - (i + 1)), c)))

    mapi
    |> List.map (fun lis -> lis |> Array.filter (fun (_, c) -> c = 'O'))
    |> List.collect Array.toList


let calculateScore (input: (int * char) list) =
    input |> List.map (fun (i, _) -> i) |> List.sum


let slideNorth (input: char list list) =
    input
    |> tiltSideWays
    |> slideStonesLeft
    |> tiltSideWays

let slideSouth (input: char list list) =
    input
    |> tiltSideWays
    |> slideStonesRight
    |> tiltSideWays

let slideWest (input: char list list) = input |> slideStonesLeft

let slideEast (input: char list list) = input |> slideStonesRight


let solve (totalCycles: int) =

    let cycleToGrid = Dictionary<int, char list list>()
    let gridToCycle = Dictionary<string, int>()

    let stringifyGrid (grid: char list list) =
        grid
        |> List.map (fun row -> row |> List.map string |> String.concat "")
        |> String.concat "\n"

    let rec doCycle (dish: char list list) (currentCycle: int) =
        if currentCycle > totalCycles then
            dish
        else
            let dishStr = stringifyGrid dish

            if gridToCycle.ContainsKey(dishStr) then
                let previousCycle = gridToCycle.[dishStr]
                let cyclePeriod = currentCycle - previousCycle
                let remainingCycles = (totalCycles - currentCycle) % cyclePeriod
                cycleToGrid.[gridToCycle.[dishStr] + remainingCycles]
            else
                let cycle =
                    dish
                    |> slideNorth
                    |> slideWest
                    |> slideSouth
                    |> slideEast

                gridToCycle.[dishStr] <- currentCycle
                cycleToGrid.[currentCycle] <- cycle
                doCycle cycle (currentCycle + 1)

    doCycle input 1
    |> tiltSideWays
    |> concatCharArrayToString
    |> mapPoints
    |> calculateScore

solve 1000000000
