let rec calculateSizes lines (path : string) files =
    if lines |> Array.isEmpty then
        files 
        |> List.groupBy fst 
        |> List.map (fun (path, _) -> path, files |> List.where (fun (p : string, _) -> p.StartsWith path) |> List.sumBy snd)
    else
        match lines |> Array.head |> (fun (s : string) -> s.Split " ") with
        | [|"$"; "ls"|]        -> path, files
        | [|"$"; "cd" ; ".."|] -> path.Remove(path.LastIndexOf "/"), files
        | [|"$"; "cd" ; dir|]  -> (if path = "/" then $"/{dir}" else $"{path}/{dir}"), files
        | [|"dir"; _ |]        -> path, (path, 0) :: files
        | [|size; _ |]         -> path, (path, size |> int) :: files
        ||> calculateSizes (lines |> Array.tail)

let sizes = calculateSizes (System.IO.File.ReadAllLines "inputs/day07.txt" |> Array.tail) "/" []
let spaceNeeded = sizes |> List.find (fst >> (=) "/") |> fun (_, size) -> 30000000 - (70000000 - size)

printfn "Part 1: %i" (sizes |> List.where (snd >> (>=) 100000) |> List.sumBy snd)
printfn "Part 2: %A" (sizes |> List.sortBy snd |> List.find (snd >> (<) spaceNeeded))
