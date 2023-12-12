module Day11Part2

let example =
    """
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"""
        .Split('\n')
    |> Array.map (fun s -> s.Replace(" ", ""))
    |> Array.filter (fun s -> s <> "")
    |> Array.map (fun s -> s.ToCharArray() |> Array.toList)
    |> Array.toList


let expand (grid: char list list) =
    let doesRowOnlyContainDots (row: char list) = row |> List.forall (fun c -> c = '.')

    let doesColumnOnlyContainDots (grid: char list list) (columnIdx: int) =
        grid
        |> List.forall (fun row -> List.nth row columnIdx = '.')

    let expandRows grid =
        grid
        |> List.collect (fun row ->
            if doesRowOnlyContainDots row then
                [ row; row ]
            else
                [ row ])

    let expandColumns grid =
        let numCols = List.head grid |> List.length

        let columnsToExpand =
            [ for colIdx in 0 .. numCols - 1 do
                  if doesColumnOnlyContainDots grid colIdx then
                      yield colIdx ]

        let adjustColumnIndex originalIdx numExpanded = originalIdx + numExpanded

        grid
        |> List.map (fun row ->
            let newRow = ref row
            let mutable numExpanded = 0

            for colIdx in columnsToExpand do
                let adjustedIdx = adjustColumnIndex colIdx numExpanded

                newRow
                := List.insertAt adjustedIdx '.' newRow.Value

                numExpanded <- numExpanded + 1

            newRow.Value)

    grid |> expandRows |> expandColumns


let labelGalaxies (grid: char list list) =
    let mutable id = 1

    grid
    |> List.map (
        List.map (fun cell ->
            if cell = '#' then
                let result = id
                id <- id + 1
                result
            else
                0)
    )


let bfs (start: int) (finish: int) (grid: int list list) =
    let numRows = List.length grid

    let numCols = List.head grid |> List.length

    let isValid (row, col) =
        row >= 0
        && col >= 0
        && row < numRows
        && col < numCols


    let findCoordinates value =
        let coordinates =
            grid
            |> List.mapi (fun rowIdx row ->
                row
                |> List.mapi (fun colIdx cell ->
                    if cell = value then
                        Some(rowIdx, colIdx)
                    else
                        None)
                |> List.choose id)
            |> List.collect id

        match coordinates with
        | head :: _ -> head
        | [] -> failwithf "Galaxy %d not found" value

    let startCoordinates = findCoordinates start

    let endCoordinates = findCoordinates finish

    let mutable visited = Array2D.init numRows numCols (fun _ _ -> false)

    let mutable queue = System.Collections.Generic.Queue<_>()

    queue.Enqueue((startCoordinates, 0))

    let directions = [ (-1, 0); (1, 0); (0, -1); (0, 1) ]

    let isValid (row, col) =
        row >= 0
        && col >= 0
        && row < numRows
        && col < numCols


    let rec loop () =
        if queue.Count > 0 then
            let ((row, col), dist) = queue.Dequeue()

            if (row, col) = endCoordinates then
                Some dist
            else
                for (dr, dc) in directions do
                    let newRow, newCol = row + dr, col + dc

                    if isValid (newRow, newCol) then
                        let newCell = List.nth (List.nth grid newRow) newCol

                        let canMoveOntoCell =
                            (newCell = 0 || newCell = start || newCell = finish)
                            && not visited.[newRow, newCol]

                        if canMoveOntoCell then
                            visited.[newRow, newCol] <- true
                            queue.Enqueue(((newRow, newCol), dist + 1))

                loop ()
        else
            None

    match loop () with
    | Some dist -> dist
    | None -> -1


let sumPathLengths (labeledGrid: int list list) =
    let galaxies =
        labeledGrid
        |> List.collect id
        |> List.filter (fun x -> x > 0)
        |> List.distinct

    let pairs =
        [ for g1 in galaxies do
              for g2 in galaxies do
                  if g1 < g2 then yield (g1, g2) ]

    pairs
    |> List.map (fun (g1, g2) -> bfs g1 g2 labeledGrid)
    |> List.sum


let input =
    System.IO.File.ReadAllLines("input/input11.txt")
    |> Array.map (fun s -> s.ToCharArray() |> Array.toList)
    |> Array.toList


let solve = input |> expand |> labelGalaxies |> sumPathLengths
