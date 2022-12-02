// puzzle: https://adventofcode.com/2022/day/2
#time

let (|Rock|Paper|Scissors|) = function | 'A' | 'X' -> Rock | 'B' | 'Y' -> Paper | _ -> Scissors
let (|Win|Draw|Lose|) = function | 'X' -> Lose | 'Y' -> Draw | _ -> Win

let hands = System.IO.File.ReadAllLines "inputs/day02.txt" |> Array.map (fun l -> Seq.head l, Seq.last l)
    
hands
|> Array.sumBy (
    function 
    | Paper,    Rock     -> 1
    | Scissors, Paper    -> 2
    | Rock,     Scissors -> 3
    | Rock,     Rock     -> 4
    | Paper,    Paper    -> 5
    | Scissors, Scissors -> 6
    | Scissors, Rock     -> 7
    | Rock,     Paper    -> 8
    | Paper,    Scissors -> 9)
|> printfn "Part 1: %i"

hands
|> Array.sumBy (
    function 
    | Paper,    Lose -> 1
    | Scissors, Lose -> 2
    | Rock,     Lose -> 3
    | Rock,     Draw -> 4
    | Paper,    Draw -> 5
    | Scissors, Draw -> 6
    | Scissors, Win  -> 7
    | Rock,     Win  -> 8
    | Paper,    Win  -> 9)
|> printfn "Part 2: %i"