let output = System.IO.File.ReadAllLines "inputs/day07.txt"

let rec calculateSizes lines (path : string) files =
    if lines |> Array.isEmpty then files
    else
        let line = lines |> Array.head
        let path =
            match line with
            | "$ cd /" -> "/"
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

let result = calculateSizes output "" List.empty

result 
|> List.groupBy fst 
|> List.map (fun (x,_) -> x, result |> List.where (fun (path, _) -> path.StartsWith(x)) |> List.sumBy snd) 
|> List.where (snd >> (>=) 100000) 
|> List.sumBy snd
|> printfn "Part 1: %i"

// let totalSpaceUsed = result |> List.where (fun x -> x |> fst |> (=) "/") |> List.exactlyOne |> snd

let totalSpace = 70000000
let totalSpaceNeeded = 30000000

let spaceUsed =
    result 
    |> List.groupBy fst 
    |> List.map (fun (x,_) -> x, result |> List.where (fun (path, _) -> path.StartsWith(x)) |> List.sumBy snd) 
    |> List.where (fun x -> x |> fst |> (=) "/") 
    |> List.exactlyOne 
    |> snd

let spaceAvailable = totalSpace - spaceUsed
let spaceNeeded = totalSpaceNeeded - spaceAvailable

result 
|> List.groupBy fst 
|> List.map (fun (x,_) -> x, result |> List.where (fun (path, _) -> path.StartsWith(x)) |> List.sumBy snd) 
|> List.sortBy snd
|> List.find (fun (_, size) -> size > spaceNeeded)
|> printfn "Part 2: %A"

printfn "Part 2: %A" spaceNeeded