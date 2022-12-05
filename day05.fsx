// puzzle: https://adventofcode.com/2022/day/4
#time

open System
open System.Collections.Generic
open type System.StringSplitOptions

let stacksInput, ordersInput = 
    IO.File.ReadAllLines "inputs/day05.txt"
    |> fun i -> Array.take 8 i, Array.skip 10 i

let stacks = 
    [| for column in 1 .. +4 .. 33 -> 
        [| for row in 7 .. -1 .. 0 -> stacksInput.[row].[column] |]
        |> Seq.where ((=) ' ' >> not)
        |> Stack |]

let orders =
    ordersInput
    |> Array.map (
        fun x -> x.Split([|"move "; " from "; " to "|], RemoveEmptyEntries)
        >> Array.map (int >> (fun x -> x-1)) >> (fun x -> x.[0], x.[1], x.[2])) 

orders
|> Array.map (fun (count, from, destination) ->
    [| for _ in 0 .. count -> stacks.[from].Pop() |> stacks.[destination].Push |])

printfn "Part 1: %s" (stacks |> Array.map (fun x -> x.Peek()) |> String)