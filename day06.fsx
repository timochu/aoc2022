let signal = System.IO.File.ReadAllText "inputs/day06.txt"
let find n = Seq.windowed n >> Seq.map set >> Seq.findIndex (Set.count >> (=) n) >> (+) n
printfn "Part 1: %i & Part 2: %i" (signal |> find 4) (signal |> find 14)
