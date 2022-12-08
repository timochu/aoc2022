let output = System.IO.File.ReadAllLines "inputs/day07.txt"

let rec calculateSizes (lines : string[]) (currentPath : string) (files : List<string * int>) =
    if lines |> Array.length = 0 then files
    else
        let line = lines |> Array.head
        let path =
            match line with
            | "$ cd /" -> "/"
            | "$ cd .." -> currentPath.Substring(0, currentPath |> Seq.findIndexBack ((=) '/'))
            | c when c.StartsWith("$ cd") -> 
                if currentPath = "/" then $"/{c.Substring(5)}"
                else $"{currentPath}/{c.Substring(5)}"
            | _ -> currentPath
        printfn "currentPath %s" path
        
        let foo = 
            if line |> Seq.head |> System.Char.IsDigit then
                let size = line |> Seq.takeWhile System.Char.IsDigit |> Seq.toArray |> System.String |> int
                files @ [(path, size)]
            elif line.StartsWith("dir") then
                files @ [(path, 0)]
            else files

        calculateSizes (lines |> Array.tail) path foo

let result = calculateSizes output "" List.empty
printfn "\n\n----------\n\n"

result |> List.iter (printfn "%A")
printfn "\n\n----------\n\n"
result 
|> List.groupBy fst 
|> List.map (fun (x,y) -> x, result |> List.where (fun (path, _) -> path.StartsWith(x)) |> List.sumBy snd) 
// |> List.where (fun (_, y) -> y <= 100000) 
// |> List.sumBy snd
|> List.iter (printfn "%A")

printfn "\n\n----------\n\n"
result 
|> List.groupBy fst 
|> List.map (fun (x,y) -> x, result |> List.where (fun (path, _) -> path.StartsWith(x)) |> List.sumBy snd) 
|> List.where (fun (_, y) -> y <= 100000) 
|> List.sumBy snd
|> printfn "Part 1: %i"