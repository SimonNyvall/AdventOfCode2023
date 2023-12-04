module day1.part2

open System
open System.IO
open System.Text.RegularExpressions

let readtextfile = File.ReadAllLines("input/input1.txt")



let findTextDigites (line: string) =
    let regex = Regex("(?=(one|two|three|four|five|six|seven|eight|nine|[1-9]))")
    let matches = regex.Matches line
    let results = ResizeArray<string>()

    for isMatch in matches do
        if isMatch.Groups.[1].Success then
            results.Add(isMatch.Groups.[1].Value)

    results.ToArray()

let textDigitMap =
    dict [ "one", "1"
           "two", "2"
           "three", "3"
           "four", "4"
           "five", "5"
           "six", "6"
           "seven", "7"
           "eight", "8"
           "nine", "9"
           "1", "1"
           "2", "2"
           "3", "3"
           "4", "4"
           "5", "5"
           "6", "6"
           "7", "7"
           "8", "8"
           "9", "9" ]

let convertTextToDigits (arr: string []) =
    arr |> Array.map (fun x -> textDigitMap.[x])


let findFirstAndLastDigits (digits: string []) =

    match digits.Length with
    | 0 -> None
    | 1 -> Some(digits.[0], digits.[0])
    | _ -> Some(digits.[0], digits.[digits.Length - 1])


let result (arr: Option<string * string> []) =
    arr
    |> Array.filter (fun x -> x.IsSome)
    |> Array.map (fun x -> x.Value)
    |> Array.map (fun (x, y) -> x + y)
    |> Array.map int
    |> Array.sum



readtextfile
|> Array.map findTextDigites
|> Array.map convertTextToDigits
|> Array.map findFirstAndLastDigits
|> result
