// puzzle: https://adventofcode.com/2022/day/5
#time

open System
open System.Collections.Generic

let stacksInput, orders = 
    IO.File.ReadAllLines "inputs/day05.txt"
    |> fun i -> Array.take 8 i, Array.skip 10 i |> Array.map (
        fun x -> x.Split([|"move "; " from "; " to "|], StringSplitOptions.RemoveEmptyEntries)
        >> Array.map (int >> (fun x -> x-1)) >> (fun x -> x.[0], x.[1], x.[2]))

let s1, s2 =
    [| for column in 1 .. +4 .. 33 ->
        [| for row in 7 .. -1 .. 0 -> stacksInput.[row].[column] |]
        |> Seq.where ((=) ' ' >> not)
        |> fun s -> Stack s, Stack s |]
    |> Array.unzip

orders
|> Array.iter (fun (c, f, t) ->
    [| for _ in 0 .. c -> s1.[f].Pop() |] |> Array.iter s1.[t].Push
    [| for _ in 0 .. c -> s2.[f].Pop() |] |> Array.rev |> Array.iter s2.[t].Push)

printfn "Part 1: %s" (s1 |> Array.map (fun x -> x.Peek()) |> String)
printfn "Part 2: %s" (s2 |> Array.map (fun x -> x.Peek()) |> String)
