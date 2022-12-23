let input = System.IO.File.ReadAllLines "inputs/day11.txt"
let items      = input |> fun s -> [for i in 1 .. 7 .. input.Length -> (s[i][18..]).Split ", " |> Array.map int64 |> System.Collections.Generic.Queue]
let items2     = input |> fun s -> [for i in 1 .. 7 .. input.Length -> (s[i][18..]).Split ", " |> Array.map int64 |> System.Collections.Generic.Queue]
let operations = input |> fun s -> [for i in 2 .. 7 .. input.Length -> s[i][19..]]
let tests      = input |> fun s -> [for i in 3 .. 7 .. input.Length -> s[i][21..] |> int64]
let trues      = input |> fun s -> [for i in 4 .. 7 .. input.Length -> s[i][29..] |> int]
let falses     = input |> fun s -> [for i in 5 .. 7 .. input.Length -> s[i][29..] |> int]
let divisibilityProduct = tests |> List.reduce (*)
let counter1, counter2 = Array.zeroCreate items.Length, Array.zeroCreate items.Length

let operation = 
    function
    | [|a ; "+" ; b|] -> (int64 a) + (int64 b)
    | [|a ; "*" ; b|] -> (int64 a) * (int64 b)
    | _ -> failwithf "fuu"

[ for _ in 1 .. 20 ->
    items 
    |> List.iteri (fun monkey queue ->
        while queue |> Seq.isEmpty |> not do
            queue.Dequeue()
            |> fun item -> operations[monkey].Replace("old", string item).Split " "
            |> operation
            |> fun worry -> worry / 3L
            |> fun worry ->
                let dst = if worry % tests[monkey] = 0 then trues[monkey] else falses[monkey]
                items[dst].Enqueue worry
                counter1[monkey] <- (counter1[monkey]+1UL
            )
        )
]

[for _ in 1 .. 10000 -> 
    items2 |> List.iteri (fun monkey queue ->
        while queue |> Seq.isEmpty |> not do
            operations[monkey].Replace("old", string (queue.Dequeue())).Split " "
            |> operation
            |> fun worry -> worry % divisibilityProduct
            |> fun worry ->
                let dst = if worry % tests[monkey] = 0 then trues[monkey] else falses[monkey]
                items2[dst].Enqueue worry
                counter2[monkey] <- (counter2[monkey]+1)
    )
]

counter1 |> Array.sortDescending |> Array.take 2 |> Array.reduce (*) |> printfn "Part 1: %i"
counter2 |> Array.sortDescending |> Array.take 2 |> Array.map uint64 |> Array.reduce (*) |> printfn "Part 2: %i"
