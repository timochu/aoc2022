let rec calculateSizes (path : string) files lines = 
    if lines |> Array.isEmpty then 
        files 
        |> List.groupBy fst 
        |> List.map (fun (path, _) -> path, files |> List.where (fun (p : string, _) -> p.StartsWith path) |> List.sumBy snd)
    else
        let path, f =
            match lines |> Array.head with
            | "$ cd .."                                -> path.Substring(0, path |> Seq.findIndexBack ((=) '/')), files
            | l when l.StartsWith "$ cd" && path = "/" -> $"/{l.Substring(5)}", files
            | l when l.StartsWith "$ cd"               -> $"{path}/{l.Substring(5)}", files
            | l when l.[0] |> System.Char.IsDigit      -> path, (path, l |> fun (i:string) -> i.Split " " |> Seq.head |> int) :: files
            | l when l.StartsWith "dir"                -> path, (path, 0) :: files
            | _ -> path, files
        calculateSizes path f (lines |> Array.tail)

let sizes = System.IO.File.ReadAllLines "inputs/day07.txt" |> Array.skip 1 |> calculateSizes "/" []
let spaceNeeded = sizes |> List.find (fst >> (=) "/") |> fun (_, spaceUsed) -> 30000000 - (70000000 - spaceUsed)

printfn "Part 1: %i" (sizes |> List.where (snd >> (>=) 100000) |> List.sumBy snd)
printfn "Part 2: %A" (sizes |> List.sortBy snd |> List.find (snd >> (<) spaceNeeded))
