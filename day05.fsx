// puzzle: https://adventofcode.com/2022/day/5
#time

open System
open System.Collections.Generic
open type System.StringSplitOptions

let stacksInput, orders = 
    IO.File.ReadAllLines "inputs/day05.txt"
    |> fun i -> Array.take 8 i, Array.skip 10 i |> Array.map (
        fun x -> x.Split([|"move "; " from "; " to "|], RemoveEmptyEntries)
        >> Array.map (int >> (fun x -> x-1)) >> (fun x -> x.[0], x.[1], x.[2]))

let part1Stacks, part2Stacks = 
    [| for column in 1 .. +4 .. 33 -> 
        [| for row in 7 .. -1 .. 0 -> stacksInput.[row].[column] |]
        |> Seq.where ((=) ' ' >> not)
        |> fun s -> Stack s, Stack s |] 
    |> Array.unzip

orders
|> Array.iter (fun (count, from, destination) ->
    [| for _ in 0 .. count -> part1Stacks.[from].Pop() |]
    |> Array.iter part1Stacks.[destination].Push)

orders
|> Array.iter (fun (count, from, destination) ->
    [| for _ in 0 .. count -> part2Stacks.[from].Pop() |] 
    |> Array.rev 
    |> Array.iter part2Stacks.[destination].Push )

printfn "Part 1: %s" (part1Stacks |> Array.map (fun x -> x.Peek()) |> String)
printfn "Part 2: %s" (part2Stacks |> Array.map (fun x -> x.Peek()) |> String)
