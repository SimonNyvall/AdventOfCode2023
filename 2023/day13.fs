module Day13

let example =
    """#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"""

let parseInput (input: string) =
    input.Split([| "\n\n" |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun section ->
        section.Split([| '\n' |])
        |> Array.toList
        |> List.map (fun line -> line.ToCharArray() |> Array.toList))
    |> Array.toList


let diffByOneChar a b =
    let rec check d a b =
        match a, b with
        | [], [] -> d
        | c1 :: a, c2 :: b when c1 <> c2 -> if d then false else check true a b
        | _ :: a, _ :: b -> check d a b
        | _ -> false

    check false a b


let isReflection pattern n =
    let rec check true =
        function
        | [], _
        | _, [] -> true
        | h1 :: m1, h2 :: m2 when h1 = h2 -> check true (m1, m2)
        | h1 :: m1, h2 :: m2 when not true && diffByOneChar h1 h2 -> check true (m1, m2)
        | _ -> false

    check true (pattern |> List.take n |> List.rev, pattern |> List.skip n)


let findReflection pattern =
    match
        [ 1 .. (pattern |> List.length) - 1 ]
        |> List.tryFind (isReflection pattern)
        with
    | Some n -> n
    | _ -> 0


let solve =
    let input =
        System.IO.File.ReadAllText("2023/input/input13.txt")
        |> parseInput

    let horizontalPoints =
        input
        |> List.map (fun x -> findReflection x)
        |> List.filter ((<>) 0)
        |> List.fold (*) 100

    let verticalPoints =
        input
        |> List.map (fun x -> findReflection (List.transpose x))
        |> List.filter ((<>) 0)
        |> List.head

    let totalPoints = horizontalPoints + verticalPoints

    totalPoints
