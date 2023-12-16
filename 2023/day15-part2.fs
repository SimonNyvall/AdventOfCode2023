module Day15Part2


let example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"


type Content = { Label: string; FocalLength: int }

type Box = { Id: int; Contnets: Content list }


let parseInput (input: string) : string list = input.Split(',') |> Array.toList


let rec hashString (input: string) (currentCharIndex: int) (currentVlaue: int) =
    if currentCharIndex = input.Length then
        currentVlaue
    else
        let ASCIICode = int input.[currentCharIndex]
        let sumWithChar = currentVlaue + ASCIICode
        let multipliedSum = sumWithChar * 17
        let moduloResult = multipliedSum % 256

        hashString input (currentCharIndex + 1) moduloResult


let getLabel (splitChar: char) (str: string) = str.Split(splitChar).[0]
let getFocalLength (str: string) = str.Split('=') |> Array.last |> int


let appendContentToBoxAtEnd (box: Box) (content: Content) =
    { box with Contnets = List.append box.Contnets [ content ] }


let doesKeyExist (key: int) (boxes: Box list) =
    boxes |> List.exists (fun b -> b.Id = key)

let doesLabelExist (label: string) (bo: Box) =
    bo.Contnets
    |> List.exists (fun c -> c.Label = label)


let rec parsePart (parts: string list) (partIndex: int) (currentData: Box list) =
    if partIndex = parts.Length then
        currentData
    else
        let part = parts.[partIndex]

        if part.Contains "=" then
            let label = getLabel '=' part
            let focalLength = getFocalLength part

            let content =
                { Label = label
                  FocalLength = focalLength }

            let boxId = hashString label 0 0

            let boxOption =
                currentData
                |> List.tryFind (fun box -> box.Id = boxId)

            match boxOption with
            | Some box when doesLabelExist label box ->
                let updatedBoxes =
                    currentData
                    |> List.map (fun b ->
                        if b.Id = boxId then
                            let updatedContents =
                                b.Contnets
                                |> List.map (fun c ->
                                    if c.Label = label then
                                        { c with FocalLength = focalLength }
                                    else
                                        c)

                            { b with Contnets = updatedContents }
                        else
                            b)

                parsePart parts (partIndex + 1) updatedBoxes

            | _ ->
                let updatedBox =
                    appendContentToBoxAtEnd
                        (boxOption
                         |> Option.defaultValue { Id = boxId; Contnets = [] })
                        content

                let updatedData =
                    currentData
                    |> List.filter (fun b -> b.Id <> boxId)
                    |> List.append [ updatedBox ]

                parsePart parts (partIndex + 1) updatedData
        else
            let label = getLabel '-' part
            let boxId = hashString label 0 0

            let boxOption =
                currentData
                |> List.tryFind (fun box -> box.Id = boxId)

            match boxOption with
            | Some box when doesLabelExist label box ->
                let updatedBoxes =
                    currentData
                    |> List.map (fun b ->
                        if b.Id = boxId then
                            let updatedContents =
                                b.Contnets
                                |> List.filter (fun c -> c.Label <> label)

                            { b with Contnets = updatedContents }
                        else
                            b)

                if updatedBoxes
                   |> List.exists (fun b -> b.Contnets.Length = 0) then
                    let updatedBoxes =
                        updatedBoxes
                        |> List.filter (fun b -> b.Contnets.Length <> 0)

                    parsePart parts (partIndex + 1) updatedBoxes
                else
                    parsePart parts (partIndex + 1) updatedBoxes
            | _ -> parsePart parts (partIndex + 1) currentData


let calculateScore (boxes: Box list) =
    let score =
        [ for b in boxes do
              b.Contnets
              |> List.mapi (fun i c -> c.FocalLength * (i + 1) * (b.Id + 1)) ]

    score |> List.concat |> List.sum


let solve =
    let input =
        System.IO.File.ReadAllText "2023/input/input15.txt"
        |> fun x -> x.Trim()
        |> parseInput

    let result = calculateScore (parsePart input 0 [])

    result
