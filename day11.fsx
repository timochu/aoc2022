open System.Collections.Generic

let input = System.IO.File.ReadAllLines "inputs/day11.txt"
let items, items2 = input |> fun s -> [for i in 1 .. 7 .. input.Length -> (s[i][18..]).Split ", " |> Array.map int64 |> fun x -> Queue x, Queue x] |> List.unzip
let operations    = input |> fun s -> [for i in 2 .. 7 .. input.Length -> s[i][19..]]
let tests         = input |> fun s -> [for i in 3 .. 7 .. input.Length -> s[i][21..] |> int64]
let trues         = input |> fun s -> [for i in 4 .. 7 .. input.Length -> s[i][29..] |> int]
let falses        = input |> fun s -> [for i in 5 .. 7 .. input.Length -> s[i][29..] |> int]

let compute rounds worryReducer (items : int64 Queue list) =
    let inspections = Array.zeroCreate items.Length
    for _ = 1 to rounds do
        items 
        |> List.iteri (fun monkey queue ->
            while queue |> Seq.isEmpty |> not do
                queue.Dequeue()
                |> fun item -> operations[monkey].Replace("old", string item).Split " "
                |> fun [|a; op; b|] -> if op = "+" then (int64 a) + (int64 b) else (int64 a) * (int64 b)
                |> worryReducer
                |> fun (worry : int64) ->
                    let dst = if worry % tests[monkey] = 0 then trues[monkey] else falses[monkey]
                    items[dst].Enqueue worry
                inspections[monkey] <- inspections[monkey] + 1UL
            )
    inspections |> Array.sortDescending |> Array.take 2 |> Array.map uint64 |> Array.reduce (*)

printfn "Part 1: %i" (items |> compute 20 (fun  worry -> worry / 3L))
printfn "Part 2: %i" (items2 |> compute 10000 (fun  worry -> worry % (tests |> List.reduce (*))))
