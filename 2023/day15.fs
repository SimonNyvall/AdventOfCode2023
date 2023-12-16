module Day15


let example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"


let parseInput (input: string): string list =
    input.Split(',') |> Array.toList


let rec hashString (input: string) (currentCharIndex: int) (currentVlaue: int) =
    if currentCharIndex = input.Length then
        currentVlaue
    else
        let ASCIICode = int input.[currentCharIndex]
        let sumWithChar = currentVlaue + ASCIICode
        let multipliedSum = sumWithChar * 17
        let moduloResult = multipliedSum % 256

        hashString input (currentCharIndex + 1) moduloResult


let solve =
    let input =
        System.IO.File.ReadAllText "2023/input/input15.txt"
        |> fun x -> x.Trim()
        |> parseInput 

    let result =
        input
        |> List.map (fun x -> hashString x 0 0)
        |> List.sum

    result
