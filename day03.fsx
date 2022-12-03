// puzzle: https://adventofcode.com/2022/day/3
#time

let priority = " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rucksacks = "inputs/day03.txt" |> System.IO.File.ReadAllLines

rucksacks
|> Array.sumBy(
        Seq.map (fun c -> priority |> Seq.findIndex (fun i -> i = c))
        >> Seq.splitInto 2
        >> Seq.map set
        >> (fun x -> Set.intersect (Seq.head x) (Seq.last x))
        >> Set.maxElement)
|> printfn "Part 1: %i"

rucksacks
|> Array.chunkBySize 3
|> Array.sumBy (
    Array.map (Seq.toArray >> Set)
    >> Set.intersectMany
    >> Set.maxElement
    >> (fun c -> priority |> Seq.findIndex (fun i -> i = c)))
|> printfn "Part 2: %i"
