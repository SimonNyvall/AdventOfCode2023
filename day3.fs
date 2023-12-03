module day3

open System
open System.IO
open System.Text.RegularExpressions

let puzzleInput =
    """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
        .Split('\n')
    |> Array.map (fun s -> s.Trim())
    |> Array.toSeq

type Entry =
    | Digit of string
    | Symbol of char
    | Empty

let parse =
    function
    | c when System.Char.IsDigit(c) -> Digit(c |> string)
    | '.' -> Empty
    | c -> Symbol c

let parsed =
    [ for (rownum, line) in puzzleInput |> Seq.indexed do
          [ for (colnum, character) in line |> Seq.indexed -> (colnum, rownum), parse character ] ]

type Location = int * int

type Bundles =
    | Empty
    | Number of int
    | Symbol of char

let digit (Digit d) = d

let rec bundle (line: (Location * Entry) list) : (Location list * Bundles) list =
    match line with
    | [] -> []
    | (_, Digit _) :: t ->
        let bundled =
            line
            |> List.takeWhile (fun (loc, entry) ->
                match entry with
                | Digit _ -> true
                | _ -> false)

        let rest = line |> List.skip (bundled |> Seq.length)

        let number =
            bundled
            |> List.map (snd >> digit)
            |> String.concat ""
            |> int

        let number =
            bundled
            |> List.map (snd >> digit)
            |> String.concat ""
            |> int
            |> Number

        let locations = bundled |> List.map fst

        (locations, number) :: (rest |> bundle)
    | (pos, otherwise) :: t ->
        let entry =
            match otherwise with
            | Entry.Empty -> ([ pos ], Bundles.Empty)
            | Entry.Symbol c -> ([ pos ], Bundles.Symbol c)

        entry :: (t |> bundle)


let bundled = parsed |> List.collect bundle


let numbers =
    bundled
    |> List.choose (fun (loc, d) ->
        match d with
        | Number d -> Some(d, loc)
        | _ -> None)


let symbols =
    bundled
    |> List.choose (fun (loc, d) ->
        match d with
        | Symbol s -> Some(s, loc)
        | _ -> None)


let symbolLocations = symbols |> List.collect fst


let isPartNumber symbols (number, locations) = true


let partNumber =
    number
    |> List.filter (isPartNumber symbolLocations)

// .....
// .633.
// .#...
