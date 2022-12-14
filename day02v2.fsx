// puzzle: https://adventofcode.com/2022/day/2
#time

let hands = System.IO.File.ReadAllLines "inputs/day02.txt"

let scores =
    function 
    | "A X" -> 4, 3
    | "A Y" -> 8, 4
    | "A Z" -> 3, 8
    | "B X" -> 1, 1
    | "B Y" -> 5, 5
    | "B Z" -> 9, 9
    | "C X" -> 7, 2
    | "C Y" -> 2, 6
    | "C Z" -> 6, 7

hands |> Array.sumBy (scores >> fst) |> printfn "Part 1: %i"
hands |> Array.sumBy (scores >> snd) |> printfn "Part 2: %i"