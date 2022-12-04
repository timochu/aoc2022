// puzzle: https://adventofcode.com/2022/day/4
#time
let split (separator : char[]) (s : string) = s.Split separator

let pairings = System.IO.File.ReadAllLines "inputs/day04.txt"

pairings
|> Array.where (
    split [|',';'-'|]
    >> Array.map int
    >> fun x -> Set [x.[0] .. x.[1]], Set [x.[2] .. x.[3]]
    >> fun (x, y) -> Set.isSubset x y || Set.isSubset y x)
|> Array.length
|> printfn "Part 1: %i"

pairings
|> Array.where (
    split [|',';'-'|]
    >> Array.map int
    >> fun x -> [Set [x.[0] .. x.[1]]; Set [x.[2] .. x.[3]]]
    >> Set.intersectMany
    >> Seq.isEmpty
    >> not)
|> Array.length
|> printfn "Part 2: %i"
