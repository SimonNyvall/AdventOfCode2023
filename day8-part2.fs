module Day8.Part2


type Node = {
    Name: string
    RightDestination: string
    LeftDestination: string
}


type Network = {
    Instructions: string list
    Node: Node list
}


let parseInput (input: string list) : Network =
    let instructions: string list =
        input
        |> List.head
        |> fun s -> s.ToCharArray() |> Array.toList |> List.map string

    let mutable nodeCountMap = Map.empty
    let addPrefix (name: string) : string =
        if name = "XXX" then name
        else
            let count = match Map.tryFind name nodeCountMap with
                         | Some c -> c
                         | None -> 0
            nodeCountMap <- Map.add name (count + 1) nodeCountMap
            let prefix = if count = 0 then "11" else "22"
            prefix + name

    let transformNodes (nodes: string list) : Node list =
        nodes
        |> List.collect (fun s ->
            let parts = s.Replace("=", ",").Split(',')
            if parts.Length <> 3 then
                failwithf "Invalid input format: '%s'" s
            else
                let name = parts.[0].Trim()
                let rightDest = parts.[2].Trim()
                let leftDest = parts.[1].Trim()
                [{ Name = addPrefix name; RightDestination = addPrefix rightDest; LeftDestination = addPrefix leftDest }])

    let elements = transformNodes (List.tail input)

    { Instructions = instructions; Node = elements }


let findNodeByName (nodes: Node list) (name: string) =
    nodes |> List.tryFind (fun node -> node.Name = name)


let startsWithA (node: Node) = node.Name.EndsWith("A")
let endsWithZ (node: Node) = node.Name.EndsWith("Z")


let rec traverseMultiple (network: Network) (currentNodeNames: string list) (instructionIndex: int) (stepCount: int64) =
    match currentNodeNames with
    | [] -> stepCount
    | _ ->
        let currentNodes =
            currentNodeNames
            |> List.map (fun name -> findNodeByName network.Node name)
            |> List.choose id

        if currentNodes |> List.forall endsWithZ then
            stepCount
        else
            let nextNodes =
                currentNodes
                |> List.map (fun currentNode ->
                    let instruction = network.Instructions.[instructionIndex]
                    match instruction with
                    | "R" -> currentNode.RightDestination
                    | "L" -> currentNode.LeftDestination
                    | _ -> failwith "Invalid instruction")
                |> List.distinct

            let nextNodeNames = nextNodes |> List.map (fun nodeName -> nodeName)
            let nextInstructionIndex = (instructionIndex + 1) % (List.length network.Instructions)
            traverseMultiple network nextNodeNames nextInstructionIndex (stepCount + 1L)


let example =
    """
    LR

    AAA = (BBB, XXX)
    BBB = (XXX, ZZZ)
    ZZZ = (BBB, XXX)
    AAA = (BBB, XXX)
    BBB = (CCC, CCC)
    CCC = (ZZZ, ZZZ)
    ZZZ = (BBB, BBB)
    XXX = (XXX, XXX)
    """
      .Split('\n')
    |> Array.toList
    |> List.map (fun s -> s.Replace(" ", ""))
    |> List.map (fun s -> s.Replace("(", ""))
    |> List.map (fun s -> s.Replace(")", ""))
    |> List.filter (fun s -> s <> "")


let input = 
  System.IO.File.ReadAllLines("input/input8.txt")
  |> Array.toList
  |> List.map (fun s -> s.Replace(" ", ""))
  |> List.map (fun s -> s.Replace("(", ""))
  |> List.map (fun s -> s.Replace(")", ""))
  |> List.filter (fun s -> s <> "")

let network = parseInput input
let startingNodes = network.Node |> List.filter startsWithA |> List.map (fun node -> node.Name)
let result = traverseMultiple network startingNodes 0 0L
