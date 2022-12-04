// puzzle: https://adventofcode.com/2022/day/4
#time

let pairings = 
    System.IO.File.ReadAllLines "inputs/day04.txt" 
    |> Array.map (
        fun s -> s.Split(',', '-')
        >> Array.map int 
        >> fun [| a1; a2; b1; b2 |] -> Set [a1..a2], Set [b1..b2])

printfn "Part 1: %A" (pairings |> Array.countBy (fun (p1, p2) -> Set.isSubset p1 p2 || Set.isSubset p2 p1))
printfn "Part 2: %A" (pairings |> Array.countBy (fun (p1, p2) -> Set.intersect p1 p2) |> Seq.isEmpty |> not)