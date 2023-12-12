type Run =
    { Year: string
      Day: string
      Part: string }

let avaliableYear = [ "2023" ]

let avaliableDay =
    [ "01"
      "02"
      "03"
      "04"
      "05"
      "06"
      "07"
      "08"
      "09"
      "10"
      "11"
      "12" ]

let avaliablePart = [ "1"; "2" ]

let parseArgs args =
    if args |> Array.length = 1 && args.[0] = "today" then
        let today = System.DateTime.Now
        let year = today.Year.ToString()
        let day = today.Day.ToString()

        if args |> Array.length < 3 then
            printf "Please specify part"
            exit 1
        else
            let part = args.[2]
            { Year = year; Day = day; Part = part }

    else

    if args |> Array.length < 3 then
        printf "Please specify year, day and part"
        exit 1
    else

        let year = args.[0]
        let day = args.[1]
        let part = args.[2]

        { Year = year; Day = day; Part = part }


let run (run: Run) =
    let year = run.Year
    let day = run.Day
    let part = run.Part

    let path =
        if part = "1" then
            sprintf "/home/nyvall/dev/proj/adventOfCode/%s/day%s.fs" year day
        else
            sprintf "/home/nyvall/dev/proj/adventOfCode/%s/day%s-part2.fs" year day

    if not (System.IO.File.Exists path) then
        printf "Day %s not implemented" day
        exit 1
    else
        let args = [| "dotnet"; "fsi"; path; part |]
        let process = System.Diagnostics.Process.Start("dotnet", "fsi " + path)
        process.WaitForExit()
        exit 0

[<EntryPoint>]
let main args =
    let runArgs = parseArgs args
    run runArgs
    0
