open System
open System.Collections.Generic

let input = IO.File.ReadAllLines "inputs/day05.txt"

let orders = input |> Array.skip 10 |> Array.map (
    fun x -> x.Split([|"move "; " from "; " to "|], StringSplitOptions.RemoveEmptyEntries)
    >> Array.map (int >> (fun x -> x-1)) >> (fun x -> x.[0], x.[1], x.[2]))

let stacks1, stacks2 = 
    let stacks = input |> Array.take 8
    [| for column in 1 .. +4 .. 33 ->
        [| for row in 7 .. -1 .. 0 -> stacks.[row].[column] |]
        |> Seq.where Char.IsLetter
        |> fun s -> Stack s, Stack s |]
    |> Array.unzip

orders |> Array.iter (fun (count, from, ``to``) ->
    [| for _ in 0 .. count -> stacks1.[from].Pop() |]              |> Array.iter stacks1.[``to``].Push
    [| for _ in 0 .. count -> stacks2.[from].Pop() |] |> Array.rev |> Array.iter stacks2.[``to``].Push)

printfn "Part 1: %s" (stacks1 |> Array.map (fun x -> x.Peek()) |> String)
printfn "Part 2: %s" (stacks2 |> Array.map (fun x -> x.Peek()) |> String)
