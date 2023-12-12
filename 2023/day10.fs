module Day10

let example =
    """
    ..F7.
    .FJ|.
    SJ.L7
    |F--J
    LJ...
    """
        .Split('\n')
        |> Array.map (fun s -> s.Trim())
        |> List.ofSeq

module Array2D =
    let findIndex el (arr: 'a[,]) =
        [ for d1 in 0 .. ((arr |> Array2D.length1) - 1) do
            for d2 in 0 .. ((arr |> Array2D.length2) - 1) do
                if arr[d1, d2] = el then yield (d1, d2) ]
        |> List.ofSeq


type Pipe = char
type Grid = Pipe[,]


let parse lines : Grid = array2D lines


type Direction = North | East | South | West



let neighbours (row, col) (grid: Grid) =
    seq {
        if row > 0 then
            yield (row - 1, col), North, grid[row - 1, col]

        if row < ((grid |> Array2D.length1) - 1) then
            yield (row + 1, col), South, grid[row + 1, col]

        if col > 0 then
            yield (row, col - 1), West, grid[row, col - 1]

        if col < ((grid |> Array2D.length2) - 1) then
            yield (row, col + 1), East, grid[row, col + 1]
    }
    |> Seq.toList


let flip =
    function
    | North -> South
    | South -> North
    | East -> West
    | West -> East


let hasConnections (dir:Direction) (p: Pipe) =
    match p, dir with
    | 'S', _ -> true
    | '.', _ -> true
    | '-', West -> true
    | '-', East -> true
    | '|', North -> true
    | '|', South -> true
    | 'L', North -> true
    | 'L', East -> true
    | 'J', North -> true
    | 'J', West -> true
    | 'F', South -> true
    | 'F', East -> true
    | '7', South -> true
    | '7', West -> true
    | _ -> false


let nextLocation (x, y) direction =
    match direction with
    | North -> (x - 1, y)
    | South -> (x + 1, y)
    | East -> (x, y + 1)
    | West -> (x, y - 1)


type State = 
    { Location: int * int
      Direction: Direction }


let nextDirection dir pipe =
    match pipe, dir with
    | '-', West -> West
    | '-', East -> East
    | '|', North -> North
    | '|', South -> South
    | 'L', South -> East
    | 'L', West -> North
    | 'J', South -> West
    | 'J', East -> North
    | 'F', North -> East
    | 'F', West -> South
    | '7', North -> West
    | '7', East -> South
    

let rec buildCycle (grid: Grid) cycle (state: State) =
    let nextLoc = nextLocation state.Location state.Direction
    let nextPipe = grid[fst nextLoc, snd nextLoc]

    if nextPipe = 'S' then
        cycle
    else
        let nextDir = nextDirection state.Direction nextPipe

        let nextState = { Location = nextLoc; Direction = nextDir }

        let nextCycle = nextLoc :: cycle
        buildCycle grid nextCycle nextState


let solve =
    let input = System.IO.File.ReadAllLines "/home/nyvall/dev/proj/adventOfCode/2023/input/input10.txt" |> List.ofSeq

    let grid = parse input
    let start = grid |> Array2D.findIndex 'S' |> List.head

    let startingNeighbours =
        neighbours start grid
        |> List.filter (fun (_, dir, pipe) -> pipe |> hasConnections (flip dir))

    let startingDirections =
        startingNeighbours
        |> List.map (fun (_, dir, _) -> dir)
        |> List.head

    let cycle =
        buildCycle grid [start] { Location = start; Direction = startingDirections }

    let result = (cycle |> Seq.length) / 2 

    result




