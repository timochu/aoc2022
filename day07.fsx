let rec calculateSizes lines (path : string) files =
    if lines |> Array.isEmpty then
        files 
        |> List.groupBy fst 
        |> List.map (fun (path, _) -> path, files |> List.where (fun (p : string, _) -> p.StartsWith path) |> List.sumBy snd)
        |> Map
    else
        match lines |> Array.head with
        | "$ ls" -> path, files
        | "$ cd .." -> path.Remove(path.LastIndexOf "/"), files
        | line when line.StartsWith "$ cd" && path = "/" -> $"/{line.Substring 5}", files
        | line when line.StartsWith "$ cd" -> $"{path}/{line.Substring 5}", files
        | line when line.StartsWith "dir" -> path, (path, 0) :: files
        | file -> path, (path, file |> Seq.takeWhile System.Char.IsDigit |> Array.ofSeq |> System.String |> int) :: files
        ||> calculateSizes (lines |> Array.tail)

let sizes = calculateSizes (System.IO.File.ReadAllLines "inputs/day07.txt") "/" []
let spaceNeeded = sizes |> Map.find "/" |> fun size -> 30000000 - (70000000 - size)

printfn "Part 1: %i" (sizes |> Map.values |> Seq.where ((>=) 100000) |> Seq.sum)
printfn "Part 2: %A" (sizes |> Map.values |> Seq.sort |> Seq.find ((<) spaceNeeded))
