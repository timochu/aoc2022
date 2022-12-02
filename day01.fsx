// puzzle: https://adventofcode.com/2022/day/1
#time
let split (separator : string) (s : string) = s.Split separator

"inputs/day01.txt" 
|> System.IO.File.ReadAllText 
|> split "\n\n"
|> Array.map (split "\n" >> Array.sumBy int) 
|> Array.sortDescending
|> fun calories ->
    printfn "Part 1: %i" (calories |> Array.head)
    printfn "Part 2: %i" (calories |> Array.take 3 |> Array.sum)
