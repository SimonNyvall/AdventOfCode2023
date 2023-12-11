module Day10


type Tile =
    | Vertical
    | Horizontal
    | Bend of char  // 'L', 'J', '7', 'F'
    | Ground
    | Start

let parseTile (c: char) : Tile =
    match c with
    | '|' -> Vertical
    | '-' -> Horizontal
    | 'L' | 'J' | '7' | 'F' -> Bend c
    | '.' -> Ground
    | 'S' -> Start
    | _ -> failwith "Invalid tile character"

let parseInput (input: string[]) : Map<(int * int), Tile> =
    input
    |> Array.mapi (fun y row ->
        row |> Seq.mapi (fun x c -> ((x, y), parseTile c)) |> Seq.toArray)
    |> Array.concat
    |> Map.ofSeq

let getNeighbors (tile: Tile, (x, y): (int * int)) : (int * int) list =
    match tile with
    | Vertical -> [(x, y - 1); (x, y + 1)]
    | Horizontal -> [(x - 1, y); (x + 1, y)]
    | Bend 'L' -> [(x, y - 1); (x + 1, y)]
    | Bend 'J' -> [(x, y - 1); (x - 1, y)]
    | Bend '7' -> [(x, y + 1); (x - 1, y)]
    | Bend 'F' -> [(x, y + 1); (x + 1, y)]
    | _ -> []

let findStart (grid: Map<(int * int), Tile>) : (int * int) =
    grid |> Map.tryFindKey (fun _ tile -> tile = Start) |> Option.get

let traverseLoop (grid: Map<(int * int), Tile>) start : Map<(int * int), int> =
    let rec traverse (current: (int * int)) (distance: int) (visited: Map<(int * int), int>) : Map<(int * int), int> =
        printf "Visiting %A\n" current
        match Map.tryFind current grid with
        | Some tile ->
            let neighbors = getNeighbors(tile, current) |> List.filter (fun pos -> not (Map.containsKey pos visited))
            match neighbors with
            | [next] -> traverse next (distance + 1) (Map.add current distance visited)
            | _ -> visited  // End of the loop or invalid tile
        | None -> visited  // Outside the grid

    traverse start 0 Map.empty

let solve (input: string[]) : int =
    let grid = parseInput input
    let start = findStart grid
    let distances = traverseLoop grid start
    distances |> Map.fold (fun maxDistance _ dist -> max dist maxDistance) 0

// Example usage
let input = [| 
    "..F7."; 
    ".FJ|."; 
    "SJ.L7";
    "|F--J";
    "LJ..." |]
solve input

