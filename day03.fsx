// puzzle: https://adventofcode.com/2022/day/3
#time

let priority = " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rucksacks = "inputs/day03.txt" |> System.IO.File.ReadAllLines

rucksacks
|> Array.sumBy(
    Seq.map priority.IndexOf
    >> Seq.splitInto 2
    >> Seq.map set
    >> (fun x -> Set.intersect (Seq.head x) (Seq.last x))
    >> Set.maxElement)
|> printfn "Part 1: %i"

rucksacks
|> Array.chunkBySize 3
|> Array.sumBy(
    Array.map set
    >> Set.intersectMany
    >> Set.maxElement
    >> priority.IndexOf)
|> printfn "Part 2: %i"
