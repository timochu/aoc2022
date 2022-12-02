// puzzle: https://adventofcode.com/2022/day/2
#time

let hands = System.IO.File.ReadAllLines "inputs/day02.txt" 

hands
|> Array.sumBy (
    function
    | "B X" -> 1
    | "C Y" -> 2
    | "A Z" -> 3
    | "A X" -> 4
    | "B Y" -> 5
    | "C Z" -> 6
    | "C X" -> 7
    | "A Y" -> 8
    | "B Z" -> 9
    | _ -> failwithf "unknown combination")
|> printfn "Part 1: %i"

hands
|> Array.sumBy (
    function
    | "B X" -> 1
    | "C X" -> 2
    | "A X" -> 3
    | "A Y" -> 4
    | "B Y" -> 5
    | "C Y" -> 6
    | "C Z" -> 7
    | "A Z" -> 8
    | "B Z" -> 9
    | _ -> failwithf "unknown combination")
|> printfn "Part 2: %i"















let hands3 = 
    "inputs/day02.txt" 
    |> System.IO.File.ReadAllLines 
    |> Array.map (fun s -> Seq.head s, Seq.last s)

hands3
|> Array.sumBy (
    function
    | 'B', 'X' -> 0 + 1 // 1
    | 'C', 'Y' -> 0 + 2 // 2
    | 'A', 'Z' -> 0 + 3 // 3
    | 'A', 'X' -> 3 + 1 // 4
    | 'B', 'Y' -> 3 + 2 // 5
    | 'C', 'Z' -> 3 + 3 // 6
    | 'C', 'X' -> 6 + 1 // 7
    | 'A', 'Y' -> 6 + 2 // 8
    | 'B', 'Z' -> 6 + 3 // 9
    | _ -> failwithf "unknown combination")
|> printfn "Part 1: %i"

hands3
|> Array.sumBy (
    function
    | 'B', 'X' -> 0 + 1 // 1
    | 'C', 'X' -> 0 + 2 // 2
    | 'A', 'X' -> 0 + 3 // 3
    | 'A', 'Y' -> 3 + 1 // 4
    | 'B', 'Y' -> 3 + 2 // 5
    | 'C', 'Y' -> 3 + 3 // 6
    | 'C', 'Z' -> 6 + 1 // 7
    | 'A', 'Z' -> 6 + 2 // 8
    | 'B', 'Z' -> 6 + 3 // 9
    | _ -> failwithf "unknown combination")
|> printfn "Part 2: %i"


