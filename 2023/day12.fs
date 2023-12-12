module Day12.Part1


let example =
    """
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
  """
        .Split('\n')
    |> Array.toList
    |> List.skip 1
    |> List.rev
    |> List.skip 1
    |> List.rev


type Spring =
    { Configuration: string
      Numbers: int list }


let parse (line: string) =
    let numbers =
        line
        |> fun x -> x.Split(' ')
        |> Array.last
        |> fun x -> x.Split(',')
        |> Array.toList
        |> List.map int

    let configuration = line |> (fun x -> x.Split(' ')) |> Array.head

    { Configuration = configuration
      Numbers = numbers }


let rec count (configuration: string) (nums: int list) : int =
    if configuration = "" then
        if nums = [] then 1 else 0
    elif nums = [] then
        if configuration.Contains('#') then
            0
        else
            1
    else
        let result = ref 0

        if configuration.[0] = '.' || configuration.[0] = '?' then
            result.Value <-
                result.Value
                + count (configuration.Substring(1)) nums

        if configuration.[0] = '#' || configuration.[0] = '?' then
            match nums with
            | firstNum :: restNums ->
                if firstNum <= configuration.Length
                   && not (configuration.Substring(0, firstNum).Contains('.'))
                   && (firstNum = configuration.Length
                       || configuration.[firstNum] <> '#') then
                    if firstNum = configuration.Length then
                        result.Value <- result.Value + count "" restNums
                    else
                        result.Value <-
                            result.Value
                            + count (configuration.Substring(firstNum + 1)) restNums
            | [] -> ()

        result.Value


let solve =
    let input =
        System.IO.File.ReadAllLines "/home/nyvall/dev/proj/adventOfCode/2023/input/input12.txt"
        |> Array.toList

    input
    |> List.map parse
    |> List.map (fun x -> count x.Configuration x.Numbers)
    |> List.sum
