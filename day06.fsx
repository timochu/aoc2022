System.IO.File.ReadAllText "inputs/day06.txt"
|> Seq.windowed 4
|> Seq.map set
|> Seq.findIndex (fun s -> s.Count = 4)
|> (+) 4
|> printfn "Part 1: %i"
