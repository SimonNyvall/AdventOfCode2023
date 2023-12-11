module Day8.Part1


let example =
    """
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
  """
        .Split('\n')
    |> Array.toList
    |> List.map (fun s -> s.Replace(" ", ""))
    |> List.map (fun s -> s.Replace("(", ""))
    |> List.map (fun s -> s.Replace(")", ""))
    |> List.filter (fun s -> s <> "")


type Direction =
    | Left
    | Right


type Node =
    { Name: string
      RightDestination: string
      LeftDestination: string }


type Network =
    { Instructions: string list
      Node: Node list }


let parseInput (input: string list) =
    let instructions =
        input
        |> List.head
        |> fun s ->
            s.ToCharArray()
            |> Array.toList
            |> List.map (fun c -> c.ToString())

    let elements =
        input
        |> List.skip 1
        |> List.map (fun s -> s.Replace("=", ","))
        |> List.map (fun s -> s.Split(','))
        |> List.map (fun s ->
            { Name = s.[0]
              RightDestination = s.[2]
              LeftDestination = s.[1] })

    { Instructions = instructions
      Node = elements }


let findNodeByName (nodes: Node list) (name: string) =
    nodes
    |> List.tryFind (fun node -> node.Name = name)


let rec traverse (network: Network) (currentNodeName: string) (instructionIndex: int) (stepCount: int) =
    match findNodeByName network.Node currentNodeName with
    | None -> -1
    | Some currentNode ->
        if currentNodeName = "ZZZ" then
            stepCount
        else
            let instruction = network.Instructions.[instructionIndex]

            let nextNodeName =
                match instruction with
                | "R" -> currentNode.RightDestination
                | "L" -> currentNode.LeftDestination
                | _ -> failwith "Invalid instruction"

            let nextInstructionIndex =
                (instructionIndex + 1) % (List.length network.Instructions)

            traverse network nextNodeName nextInstructionIndex (stepCount + 1)


let solve =
    let input =
        System.IO.File.ReadAllLines("input/input8.txt")
        |> Array.toList
        |> List.map (fun s -> s.Replace(" ", ""))
        |> List.map (fun s -> s.Replace("(", ""))
        |> List.map (fun s -> s.Replace(")", ""))
        |> List.filter (fun s -> s <> "")

    let network = parseInput input
    traverse network "AAA" 0 0
