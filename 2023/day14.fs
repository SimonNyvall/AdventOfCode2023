module Day14

let example =
    """O....#....
O.OO#....#
.....##...
OO##O....O
.O#....O#.
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


let slideStonesNorth (input: char list list) : string list =
    let concatCharArrayToString: string list =
        input
        |> List.map (fun arr ->
            arr
            |> List.map (fun c -> c.ToString())
            |> String.concat "")

    let pushOsToLeft (str: string) : string =
        let parts = str.Split([| '#' |], System.StringSplitOptions.None)

        let processPart (part: string) : string =
            let oCount =
                part
                |> Seq.filter (fun c -> c = 'O')
                |> Seq.length

            String.replicate oCount "O"
            + String.replicate (part.Length - oCount) "."

        let processedParts: string [] = parts |> Array.map processPart
        System.String.Join("#", processedParts)

    concatCharArrayToString |> List.map pushOsToLeft


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


let solve =
    input
    |> tiltSideWays
    |> slideStonesNorth
    |> mapPoints
    |> calculateScore
