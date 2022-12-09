open System

let rec calculateSizes lines (path : string) files =
    match lines with
    | [] -> 
        files 
        |> List.groupBy fst 
        |> List.map (fun (path, _) -> path, files |> List.where (fun (p : string, _) -> p.StartsWith path) |> List.sumBy snd)
        |> Map
    | head :: tail ->
        match head with
        | "$ cd .."                                -> path.Substring(0, path |> Seq.findIndexBack ((=) '/')), files
        | l when l.StartsWith "$ cd" && path = "/" -> $"/{l.Substring 5}", files
        | l when l.StartsWith "$ cd"               -> $"{path}/{l.Substring 5}", files
        | l when l.[0] |> Char.IsDigit             -> path, (path, l |> Seq.takeWhile Char.IsDigit |> Array.ofSeq |> String |> int) :: files
        | _                                        -> path, (path, 0) :: files
        ||> calculateSizes tail

let terminalOutput = IO.File.ReadLines "inputs/day07.txt" |> Seq.tail |> Seq.toList
let sizes = calculateSizes terminalOutput "/" []
let spaceNeeded = sizes |> Map.find "/" |> fun size -> 30000000 - (70000000 - size)

printfn "Part 1: %i" (sizes |> Map.values |> Seq.where ((>=) 100000) |> Seq.sum)
printfn "Part 2: %A" (sizes |> Map.values |> Seq.sort |> Seq.find ((<) spaceNeeded))
