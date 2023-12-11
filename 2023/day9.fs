module Day9


let exmaple =
    """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
  """
        .Split('\n')
    |> Array.filter (fun s -> s <> "")
    |> Array.rev
    |> Array.skip 1
    |> Array.rev
    |> Array.toList


let parseInput (input: string list) =
    input
    |> List.map (fun s -> s.Split(' '))
    |> List.map (fun arr -> arr |> Array.toList |> List.map int)


let differenceSequence (input: int list) =
    let rec loop (input: int list) (acc: int list) =
        match input with
        | [] -> acc
        | [ x ] -> acc
        | x :: y :: xs -> loop (y :: xs) ((y - x) :: acc)

    loop input [] |> List.rev


let isAllZero (input: int list) = input |> List.forall (fun x -> x = 0)


let rec generateUntilZeroes (input: int list) =
    let diffSeq = differenceSequence input

    if isAllZero diffSeq then
        input
    else
        generateUntilZeroes diffSeq


let extrapolateNextValue (input: int list) =
    let rec generateDifferenceSequences seq acc =
        let diffSeq = differenceSequence seq

        if isAllZero diffSeq then
            (diffSeq :: seq :: acc)
        else
            generateDifferenceSequences diffSeq (seq :: acc)

    let sequences = generateDifferenceSequences input []

    let calculateNextValue sequences =
        sequences
        |> List.map (fun arr -> arr |> List.last)
        |> List.sum

    calculateNextValue sequences


let sovle =
    let input =
        System.IO.File.ReadAllLines("input/input9.txt")
        |> Array.toList

    let parsedInput = input |> parseInput

    let result =
        List.map extrapolateNextValue parsedInput
        |> List.sum

    result
