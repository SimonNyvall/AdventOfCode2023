
open System
open System.IO
open System.Text.RegularExpressions

let dictmap = 
  dict [
    "one", "1"; "two", "2"; "three", "3"; "four", "4"; "five", "5"; "six", "6"; "seven", "7"; "eight", "8"; "nine", "9";
  ]

let readtextfile = 
  File.ReadAllLines("input/input1.txt")

let replaceWords (line: string) =
  let regex = Regex("one|two|three|four|five|six|seven|eight|nine")
  regex.Replace(line, fun m -> dictmap.[m.Value])

let findDigits (line: string) =
  let digits = line.ToCharArray() |> Array.filter Char.IsDigit
  match digits.Length with
  | 0 -> None
  | 1 -> Some(digits.[0], digits.[0])
  | _ -> Some(digits.[0], digits.[digits.Length - 1])

let parseLine (line: string) =
  match findDigits line with
  | Some(firstDigit, lastDigit) -> 
      int (string firstDigit) * 10 + int (string lastDigit)
  | None -> 0

let parseAndSum fileLines =
  fileLines |> Array.map parseLine |> Array.sum

let totalSum = readtextfile |> Array.map replaceWords |> parseAndSum
totalSum


readtextfile
|> Array.map replaceWords

