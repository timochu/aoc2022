let rec calculateSizes lines (path : string) files = 
    if lines |> Array.isEmpty then 
        files |> List.groupBy fst |> List.map (fun (x,_) -> x, files |> List.sumBy (fun ((pp : string), size) -> if pp.StartsWith(x) then size else 0))
    else
        let line = lines |> Array.head
        let path =
            match line with
            | "$ cd .." -> path.Substring(0, path |> Seq.findIndexBack ((=) '/'))
            | c when c.StartsWith("$ cd") && path = "/" -> $"/{c.Substring(5)}"
            | c when c.StartsWith("$ cd") -> $"{path}/{c.Substring(5)}"
            | _ -> path
        
        let f = 
            if line |> Seq.head |> System.Char.IsDigit then
                let size = line |> Seq.takeWhile System.Char.IsDigit |> Seq.toArray |> System.String |> int
                files @ [(path, size)]
            elif line.StartsWith("dir") then
                files @ [(path, 0)]
            else files

        calculateSizes (lines |> Array.tail) path f

let sizes = calculateSizes (System.IO.File.ReadAllLines "inputs/day07.txt" |> Array.skip 1) "/" []
let spaceNeeded = sizes |> List.find (fst >> (=) "/") |> fun (_, spaceUsed) -> 30000000 - (70000000 - spaceUsed)
printfn "Part 1: %i" (sizes |> List.where (snd >> (>=) 100000) |> List.sumBy snd)
printfn "Part 2: %A" (sizes |> List.sortBy snd |> List.find (snd >> (<) spaceNeeded))
