let input = System.IO.File.ReadAllLines "inputs/day11.txt"
let computer = new System.Data.DataTable()

let items      = input |> fun s -> [for i in 1 .. 7 .. input.Length -> (s[i][18..]).Split ", " |> Array.map int |> System.Collections.Generic.Queue]
let operations = input |> fun s -> [for i in 2 .. 7 .. input.Length -> s[i][19..]]
let tests      = input |> fun s -> [for i in 3 .. 7 .. input.Length -> s[i][21..] |> int]
let trues      = input |> fun s -> [for i in 4 .. 7 .. input.Length -> s[i][29..] |> int]
let falses     = input |> fun s -> [for i in 5 .. 7 .. input.Length -> s[i][29..] |> int]

let mutable counter = Array.zeroCreate items.Length

[1 .. 20] 
|> List.iter (fun _ -> 
    items |> List.iteri (fun monkey queue ->
        while queue.Count > 0 do
            let item = queue.Dequeue()
            let op = operations[monkey] |> fun s -> s.Replace("old", item |> string)
            let preInspectionWorry = computer.Compute(op,"") |> string |> int
            let postInspectionWorry = preInspectionWorry / 3 
            let destination = if postInspectionWorry % tests[monkey] = 0 then trues[monkey] else falses[monkey]
            items[destination].Enqueue postInspectionWorry
            counter <- Array.updateAt monkey (counter[monkey]+1) counter
    )
)

counter |> Array.sortDescending |> Array.take 2 |> Array.reduce (*) |> printfn "Part 1: %A"

