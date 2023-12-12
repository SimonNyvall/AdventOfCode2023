module Day1

open System
open System.IO

let readtextfile =
    File.ReadAllLines("2023/input/input1.txt")
    |> Array.rev
    |> Array.skip 1
    |> Array.rev


let findDigits (line: string) =
    let digits = line.ToCharArray() |> Array.filter Char.IsDigit

    match digits.Length with
    | 0 -> None
    | 1 -> Some(digits.[0], digits.[0])
    | _ -> Some(digits.[0], digits.[digits.Length - 1])


let parseLine (line: string) =
    match findDigits line with
    | Some (firstDigit, lastDigit) ->
        int (string firstDigit) * 10
        + int (string lastDigit)
    | None -> 0


let parseAndSum fileLines =
    fileLines |> Array.map parseLine |> Array.sum


let solve = readtextfile |> parseAndSum |> printfn "%d"
