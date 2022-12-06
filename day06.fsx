let input = System.IO.File.ReadAllText "inputs/day06.txt"
let find n = Seq.windowed n >> Seq.map set >> Seq.findIndex (fun s -> s.Count = n) >> (+) n
printfn "Part 1: %i\nPart 2: %i" (input |> find 4) (input |> find 14)
