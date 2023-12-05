module Day5.Part2

open System
open System.IO


let rawInput =
    File.ReadAllLines("input/input5.txt")
    |> Array.toList



type Seeds = { Name: string; Values: int64 list }


type Maps =
    { Seeds: Seeds
      SeedToSoil: Map<string, int64 list list>
      SoilToFertilizer: Map<string, int64 list list>
      FertilizerToWater: Map<string, int64 list list>
      WaterToLight: Map<string, int64 list list>
      LightToTemperature: Map<string, int64 list list>
      TemperatureToHumidity: Map<string, int64 list list>
      HumidityToLocation: Map<string, int64 list list> }

let getTitleFromInt index =
    match index with
    | 1 -> "seed-to-soil map:"
    | 2 -> "soil-to-fertilizer map:"
    | 3 -> "fertilizer-to-water map:"
    | 4 -> "water-to-light map:"
    | 5 -> "light-to-temperature map:"
    | 6 -> "temperature-to-humidity map:"
    | 7 -> "humidity-to-location map:"
    | _ -> ""



let extractMapData (data: string list list) (index: int) =
    let title = getTitleFromInt index

    let startIndexOption =
        data
        |> List.tryFindIndex (fun arr -> arr.Head = title)

    let endIndexOption =
        startIndexOption
        |> Option.bind (fun startIndex ->
            let nextMapIndex =
                data
                |> List.skip (startIndex + 1)
                |> List.tryFindIndex (fun arr -> arr.Head <> title && arr.Head.Contains("map:"))
                |> Option.map (fun idx -> idx + startIndex + 1)

            match nextMapIndex with
            | Some idx -> Some idx
            | None -> Some(List.length data)) // Set to the end of the list if no next map section

    match startIndexOption, endIndexOption with
    | Some startIndex, Some endIndex when startIndex < endIndex ->
        data
        |> List.skip (startIndex + 1)
        |> List.take (endIndex - startIndex - 1)
        |> List.map (fun arr ->
            arr
            |> List.collect (fun str -> str.Split(' ') |> Array.toList)
            |> List.map int64)
    | _ -> []


let parseInput (input: string list) =
    let parts =
        input
        |> List.map (fun line -> if line = "" then "|" else line)
        |> List.map (fun line -> line.Split('|'))
        |> List.map (fun arr ->
            arr
            |> Array.filter (fun s -> s <> "")
            |> Array.toList)
        |> List.filter (fun arr -> arr <> [])


    let seeds =
        let name =
            parts
            |> List.head
            |> List.map (fun s -> s.Split(':'))
            |> List.head
            |> Array.head

        let values =
            parts
            |> List.head
            |> List.map (fun s -> s.Split(':'))
            |> List.collect Array.toList
            |> List.tail
            |> List.map (fun s -> s.Trim())
            |> List.map (fun s -> s.Split(' '))
            |> List.collect Array.toList
            |> List.map (fun s -> int64 s)

        { Name = name; Values = values }


    { Seeds = seeds
      SeedToSoil =
        [ (getTitleFromInt 1, extractMapData parts 1) ]
        |> Map.ofSeq
      SoilToFertilizer =
        [ (getTitleFromInt 2, extractMapData parts 2) ]
        |> Map.ofSeq
      FertilizerToWater =
        [ (getTitleFromInt 3, extractMapData parts 3) ]
        |> Map.ofSeq
      WaterToLight =
        [ (getTitleFromInt 4, extractMapData parts 4) ]
        |> Map.ofSeq
      LightToTemperature =
        [ (getTitleFromInt 5, extractMapData parts 5) ]
        |> Map.ofSeq
      TemperatureToHumidity =
        [ (getTitleFromInt 6, extractMapData parts 6) ]
        |> Map.ofSeq
      HumidityToLocation =
        [ (getTitleFromInt 7, extractMapData parts 7) ]
        |> Map.ofSeq }


let convertNumber (map: Map<string, int64 list list>) (num: int64) : int64 =
    let defaultValue = num
    let mapData = map |> Map.toList |> List.collect snd

    let findConversion ranges =
        ranges
        |> List.tryPick (fun range ->
            match range with
            | [ destStart; srcStart; length ] when num >= srcStart && num < srcStart + length ->
                Some(destStart + (num - srcStart))
            | _ -> None)

    findConversion mapData
    |> Option.defaultValue defaultValue


let generateSeedNumbers (start: int64) (count: int64) : int64 list = [ start .. start + count - 1L ]


let calculateLowestLocation (maps: Maps) (seedRanges: (int64 * int64) list) : int64 =
    let convertSeed seed =
        let soil = convertNumber maps.SeedToSoil seed
        let fertilizer = convertNumber maps.SoilToFertilizer soil
        let water = convertNumber maps.FertilizerToWater fertilizer
        let light = convertNumber maps.WaterToLight water
        let temperature = convertNumber maps.LightToTemperature light
        let humidity = convertNumber maps.TemperatureToHumidity temperature
        convertNumber maps.HumidityToLocation humidity

    let mutable lowestLocation = Int64.MaxValue

    for (start, count) in seedRanges do
        for seed in [ start .. start + count - 1L ] do
            let location = convertSeed seed

            if location < lowestLocation then
                lowestLocation <- location

    lowestLocation

let parseSeedRanges (input: string list) : (int64 * int64) list =
    let seedLine = input |> List.head

    let seedParts =
        seedLine.Split([| ' '; ':' |], StringSplitOptions.RemoveEmptyEntries)

    seedParts
    |> Seq.skip 1
    |> Seq.chunkBySize 2
    |> Seq.map (fun arr -> (int64 arr.[0], int64 arr.[1]))
    |> List.ofSeq

let maps = rawInput |> parseInput
let seedRanges = rawInput |> parseSeedRanges


calculateLowestLocation maps seedRanges
