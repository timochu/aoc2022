// puzzle: https://adventofcode.com/2022/day/4
#time
let split (separator : char[]) (s : string) = s.Split separator |> List.ofArray

"inputs/day04.txt"
|> System.IO.File.ReadAllLines
|> Array.where (
    split [|',';'-'|]
    >> List.map int
    >> fun x -> Set [x.[0] .. x.[1]], Set [x.[2] .. x.[3]]
    >> fun (x, y) -> Set.isSubset x y || Set.isSubset y x)
|> Array.length
|> printfn "%A"
