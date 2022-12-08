let rec calculateSizes (path : string) files lines = 
    if lines |> Array.isEmpty then 
        files |> List.groupBy fst |> List.map (fun (x,_) -> x, files |> List.sumBy (fun ((pp : string), size) -> if pp.StartsWith(x) then size else 0))
    else
        let path, f =
            match lines |> Array.head with
            | "$ cd .." -> path.Substring(0, path |> Seq.findIndexBack ((=) '/')), files
            | line when line.StartsWith("$ cd") && path = "/" -> $"/{line.Substring(5)}", files
            | line when line.StartsWith("$ cd") -> $"{path}/{line.Substring(5)}", files
            | line when line |> Seq.head |> System.Char.IsDigit -> path,  (path, line |> Seq.takeWhile System.Char.IsDigit |> Seq.toArray |> System.String |> int) :: files
            | line when line.StartsWith("dir") -> path, (path, 0) :: files
            | _ -> path, files
        calculateSizes path f (lines |> Array.tail)

let sizes = System.IO.File.ReadAllLines "inputs/day07.txt" |> Array.skip 1 |> calculateSizes "/" []
let spaceNeeded = sizes |> List.find (fst >> (=) "/") |> fun (_, spaceUsed) -> 30000000 - (70000000 - spaceUsed)

printfn "Part 1: %i" (sizes |> List.where (snd >> (>=) 100000) |> List.sumBy snd)
printfn "Part 2: %A" (sizes |> List.sortBy snd |> List.find (snd >> (<) spaceNeeded))
